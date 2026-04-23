(* Phase 7 full code generation.
   Lowers typed rgo AST to Go source following PRD §5.1–§5.12 mapping rules. *)

open Ast

(* ---------- Go keyword escaping ---------- *)

let go_keywords =
  [
    "break";
    "case";
    "chan";
    "const";
    "continue";
    "default";
    "defer";
    "else";
    "fallthrough";
    "for";
    "func";
    "go";
    "goto";
    "if";
    "import";
    "interface";
    "map";
    "package";
    "range";
    "return";
    "select";
    "struct";
    "switch";
    "type";
    "var";
  ]

let is_go_keyword s = List.mem s go_keywords
let escape_ident s = if is_go_keyword s then s ^ "_" else s

(* ---------- name casing helpers ---------- *)

let capitalize s =
  if String.length s = 0 then s
  else
    String.make 1 (Char.uppercase_ascii s.[0])
    ^ String.sub s 1 (String.length s - 1)

let uncapitalize s =
  if String.length s = 0 then s
  else
    String.make 1 (Char.lowercase_ascii s.[0])
    ^ String.sub s 1 (String.length s - 1)

(* ---------- type environment (rebuilt from AST for codegen) ---------- *)

module SMap = Map.Make (String)

type variant_shape = VUnit | VTuple of Ast.ty list | VStruct of Ast.field list
type struct_info = { si_fields : Ast.field list }

type enum_info = {
  ei_variants : (string * variant_shape) list;
  ei_tparams : string list;
}

type fn_info = { fi_ret : Ast.ty option }

type method_info_cg = {
  mi_self : Ast.self_param option;
  mi_params : Ast.param list;
  mi_ret : Ast.ty option;
}

type impl_info_cg = {
  ii_methods : method_info_cg SMap.t;
  ii_assoc_fns : fn_info SMap.t;
}

(* Shared mutable state for tracking needs across all env copies *)
type cg_shared = {
  mutable tmp_counter : int;
  mutable needs_option_struct : bool;
  mutable needs_rgo_repeat : bool;
  mutable needs_fmt : bool;
  mutable needs_errors : bool;
  mutable needs_math : bool;
  mutable needs_imported_pkgs : bool;
  mutable needs_result_struct : bool;
  mutable self_ref_traits : string list;
      (* Trait names that use Self in their signatures *)
}

(* Trait info for codegen: maps trait name -> trait items (for default method synthesis) *)
type trait_info_cg = { tic_items : Ast.trait_item list }

type cg_env = {
  structs : struct_info SMap.t;
  enums : enum_info SMap.t;
  fns : fn_info SMap.t;
  impls : impl_info_cg SMap.t;
  traits : trait_info_cg SMap.t;
  trait_impls : unit SMap.t; (* "TraitName:TypeName" -> () *)
  (* Per-function context *)
  ret_ty : Ast.ty option;
  self_type_name : string option;
  self_type : Ast.ty option;
  impl_type_params : type_param list;
  type_params : string list;
  (* Value bindings: name -> (ast_ty option, is_mut) *)
  values : (Ast.ty option * bool) SMap.t list;
  (* Import metadata: alias -> (go_import_path, go_package_name) *)
  imported_packages : (string * string) SMap.t;
  (* Drop cleanup: binding name -> guard variable name *)
  drop_guards : string SMap.t;
  (* Scope drops: stack of drop binding names per nested scope level.
     Function scope is NOT tracked here (it uses defer). *)
  scope_drops : string list list;
  (* Loop body depths: stack of scope depths for enclosing loop bodies *)
  loop_body_depths : int list;
  (* Result variable decompositions: variable name -> error variable name *)
  result_decomps : string SMap.t;
  (* True when generating inside an expression-context IIFE, so flat-Result
     tuple flattening is suppressed; IIFEs always return a single value. *)
  in_iife : bool;
  (* Shared mutable state *)
  shared : cg_shared;
}

let fresh_tmp env prefix =
  let n = env.shared.tmp_counter in
  env.shared.tmp_counter <- n + 1;
  Printf.sprintf "__%s_%d" prefix n

let push_scope ?(is_function = false) env =
  let new_scope_drops =
    if is_function then env.scope_drops else [] :: env.scope_drops
  in
  { env with values = SMap.empty :: env.values; scope_drops = new_scope_drops }

(* ---------- Scope cleanup helpers ---------- *)

let emit_scope_cleanup env buf indent =
  match env.scope_drops with
  | current_drops :: _ ->
      List.iter
        (fun name ->
          match SMap.find_opt name env.drop_guards with
          | Some guard ->
              let esc = escape_ident name in
              Printf.bprintf buf "%sif %s {\n" indent guard;
              Printf.bprintf buf "%s\t%s.Drop()\n" indent esc;
              Printf.bprintf buf "%s}\n" indent;
              Printf.bprintf buf "%s%s = false\n" indent guard
          | None -> ())
        (List.rev current_drops)
  | [] -> ()

let has_nested_drops env =
  List.exists
    (fun drops -> List.exists (fun name -> SMap.mem name env.drop_guards) drops)
    env.scope_drops

let emit_all_nested_cleanup env buf indent =
  let rec clean drops_list =
    match drops_list with
    | [] -> ()
    | drops :: rest ->
        clean rest;
        List.iter
          (fun name ->
            match SMap.find_opt name env.drop_guards with
            | Some guard ->
                let esc = escape_ident name in
                Printf.bprintf buf "%sif %s {\n" indent guard;
                Printf.bprintf buf "%s\t%s.Drop()\n" indent esc;
                Printf.bprintf buf "%s}\n" indent;
                Printf.bprintf buf "%s%s = false\n" indent guard
            | None -> ())
          (List.rev drops)
  in
  clean env.scope_drops

let emit_loop_cleanup env buf indent =
  match env.loop_body_depths with
  | target_depth :: _ ->
      let current_depth = List.length env.values in
      let scopes_to_clean = current_depth - target_depth in
      let rec take_n n lst acc =
        if n <= 0 then (List.rev acc, lst)
        else
          match lst with
          | [] -> (List.rev acc, [])
          | x :: xs -> take_n (n - 1) xs (x :: acc)
      in
      let scopes_to_clean_list, _rest =
        take_n scopes_to_clean env.scope_drops []
      in
      List.iter
        (fun drops ->
          List.iter
            (fun name ->
              match SMap.find_opt name env.drop_guards with
              | Some guard ->
                  let esc = escape_ident name in
                  Printf.bprintf buf "%sif %s {\n" indent guard;
                  Printf.bprintf buf "%s\t%s.Drop()\n" indent esc;
                  Printf.bprintf buf "%s}\n" indent;
                  Printf.bprintf buf "%s%s = false\n" indent guard
              | None -> ())
            (List.rev drops))
        scopes_to_clean_list
  | [] -> ()

(* ---------- Drop-type helpers (delegated to Codegen_ownership) ---------- *)

let is_drop_type_cg env (t : Ast.ty) : bool =
  Codegen_ownership.is_drop_type
    ~has_trait_impl:(fun k -> SMap.mem k env.trait_impls)
    t

let add_value name ty_opt ~is_mut env =
  match env.values with
  | scope :: rest ->
      { env with values = SMap.add name (ty_opt, is_mut) scope :: rest }
  | [] -> env

let lookup_value name env =
  let rec go = function
    | [] -> None
    | scope :: rest -> (
        match SMap.find_opt name scope with Some v -> Some v | None -> go rest)
  in
  go env.values

let lookup_value_ty name env =
  match lookup_value name env with Some (ty, _) -> ty | None -> None

(* ---------- AST type -> Go type string ---------- *)

(* Determine if an rgo type is nullable in Go (i.e., has a nil zero value) *)
let rec is_nullable_ty env (t : Ast.ty) : bool =
  match t with
  | TyRef _ -> true
  | TyGeneric ({ node = "Vec"; _ }, _) -> true
  | TyGeneric ({ node = "HashMap"; _ }, _) -> true
  | TyGeneric ({ node = "Option"; _ }, [ arg ]) ->
      (* Option<T> lowers to *T (nullable) when T is non-nullable,
         or to Option[T] struct (non-nullable) when T is nullable *)
      not (is_nullable_ty env arg)
  | TyName { node = name; _ } -> SMap.mem name env.enums
  | TyGeneric ({ node = name; _ }, _) -> SMap.mem name env.enums
  | TySelf -> (
      match env.self_type_name with
      | Some n -> SMap.mem n env.enums
      | None -> false)
  | TyTuple _ -> false
  | TyPath (pkg, member) -> Interop.type_is_pointer pkg.node member.node

let rec go_type env (t : Ast.ty) : string =
  match t with
  | TyName { node = "i8"; _ } -> "int8"
  | TyName { node = "i16"; _ } -> "int16"
  | TyName { node = "i32"; _ } -> "int32"
  | TyName { node = "i64"; _ } -> "int64"
  | TyName { node = "u8"; _ } -> "uint8"
  | TyName { node = "u16"; _ } -> "uint16"
  | TyName { node = "u32"; _ } -> "uint32"
  | TyName { node = "u64"; _ } -> "uint64"
  | TyName { node = "f32"; _ } -> "float32"
  | TyName { node = "f64"; _ } -> "float64"
  | TyName { node = "bool"; _ } -> "bool"
  | TyName { node = "str"; _ } | TyName { node = "String"; _ } -> "string"
  | TyName { node = "Self"; _ } -> (
      match env.self_type with
      | Some t -> go_type env t
      | None -> ( match env.self_type_name with Some n -> n | None -> "Self"))
  | TyName { node = name; _ } ->
      (* If this is the Self type used bare (e.g., Box in impl<T> Box<T>),
         resolve to the full generic type *)
      if
        env.impl_type_params <> []
        && env.self_type_name = Some name
        && not (List.mem name env.type_params)
      then match env.self_type with Some st -> go_type env st | None -> name
      else name
  | TyGeneric ({ node = "Option"; _ }, [ arg ]) ->
      if is_nullable_ty env arg then begin
        env.shared.needs_option_struct <- true;
        "Option[" ^ go_type env arg ^ "]"
      end
      else "*" ^ go_type env arg
  | TyGeneric ({ node = "Result"; _ }, [ ok; err ]) ->
      env.shared.needs_result_struct <- true;
      "Result[" ^ go_type env ok ^ ", " ^ go_type env err ^ "]"
  | TyGeneric ({ node = "Vec"; _ }, [ arg ]) -> "[]" ^ go_type env arg
  | TyGeneric ({ node = "HashMap"; _ }, [ k; v ]) ->
      "map[" ^ go_type env k ^ "]" ^ go_type env v
  | TyGeneric ({ node = name; _ }, args) ->
      name ^ "[" ^ String.concat ", " (List.map (go_type env) args) ^ "]"
  | TyRef inner -> "*" ^ go_type env inner
  | TyTuple _ -> failwith "codegen: tuple types not supported in Go output"
  | TySelf -> (
      match env.self_type with
      | Some t -> go_type env t
      | None -> ( match env.self_type_name with Some n -> n | None -> "Self"))
  | TyPath (pkg, member) -> (
      (* Package-qualified stdlib type — resolve via Interop registry *)
      env.shared.needs_imported_pkgs <- true;
      let go_pkg =
        match SMap.find_opt pkg.node env.imported_packages with
        | Some (_, go_pkg_name) -> go_pkg_name
        | None -> pkg.node
      in
      match Interop.go_qualified_type pkg.node member.node go_pkg with
      | Some qt -> qt
      | None -> go_pkg ^ "." ^ member.node)

(* Returns true when a Result type should be flattened to a Go (T, error) tuple.
   A Result is "flat" when its ok type is not itself a Result; nested Results
   like Result<Result<T, E>, E> must preserve the outer Result structure. *)
let is_flat_result_ty (t : Ast.ty) : bool =
  match t with
  | TyGeneric ({ node = "Result"; _ }, [ ok; _ ]) -> (
      match ok with TyGeneric ({ node = "Result"; _ }, _) -> false | _ -> true)
  | _ -> false

let ret_ty_is_flat_result (ret_ty : Ast.ty option) : bool =
  match ret_ty with Some t -> is_flat_result_ty t | None -> false

(* Flat-Result tuple returns are only emitted at actual function boundaries,
   never inside expression-context IIFEs which always return a single value. *)
let should_flatten_result_return (env : cg_env) : bool =
  ret_ty_is_flat_result env.ret_ty && not env.in_iife

(* Go return type for printing (with leading space or parens) *)
let go_ret_sig env (ret : Ast.ty option) : string =
  match ret with
  | None -> ""
  | Some (TyGeneric ({ node = "Result"; _ }, [ ok; _err ]))
    when ret_ty_is_flat_result ret ->
      " (" ^ go_type env ok ^ ", error)"
  | Some t -> " " ^ go_type env t

(* ---------- Go zero value for a type ---------- *)
let rec go_zero_value env (t : Ast.ty) : string =
  match t with
  | TyName { node = "i8" | "i16" | "i32" | "i64"; _ } -> "0"
  | TyName { node = "u8" | "u16" | "u32" | "u64"; _ } -> "0"
  | TyName { node = "f32" | "f64"; _ } -> "0"
  | TyName { node = "bool"; _ } -> "false"
  | TyName { node = "str" | "String"; _ } -> "\"\""
  | TyName { node = name; _ } ->
      if SMap.mem name env.enums then "nil" else name ^ "{}"
  | TyGeneric ({ node = "Option"; _ }, [ arg ]) ->
      if is_nullable_ty env arg then begin
        env.shared.needs_option_struct <- true;
        "Option[" ^ go_type env arg ^ "]{}"
      end
      else "nil"
  | TyGeneric ({ node = "Vec"; _ }, _) -> "nil"
  | TyGeneric ({ node = "HashMap"; _ }, _) -> "nil"
  | TyGeneric ({ node = name; _ }, args) ->
      if SMap.mem name env.enums then "nil"
      else name ^ "[" ^ String.concat ", " (List.map (go_type env) args) ^ "]{}"
  | TyRef _ -> "nil"
  | TyTuple _ -> "nil"
  | TySelf -> (
      match env.self_type with
      | Some t -> go_zero_value env t
      | None -> (
          match env.self_type_name with
          | Some n -> if SMap.mem n env.enums then "nil" else n ^ "{}"
          | None -> "nil"))
  | TyPath (pkg, member) ->
      if Interop.type_is_pointer pkg.node member.node then "nil" else "nil"

(* For a Result-typed expression used in CtxExpr (IIFE) context, return the
   Go function signature and an environment with ret_ty preserved for
   constructor type inference but in_iife set so that tuple flattening is
   suppressed.  IIFEs always return a single value, never a Go multi-value tuple. *)
let result_iife_sig_and_env env ret_ty : string * cg_env =
  let env' =
    match ret_ty with
    | Some _ -> { env with ret_ty; in_iife = true }
    | None -> { env with in_iife = true }
  in
  match ret_ty with
  | Some (TyGeneric ({ node = "Result"; _ }, [ _; _ ]))
    when ret_ty_is_flat_result ret_ty ->
      (* Flat Results inside IIFEs still return the struct type, not a tuple *)
      let sig_ = go_type env (Option.get ret_ty) in
      (sig_, env')
  | _ ->
      let sig_ = match ret_ty with Some t -> go_type env t | None -> "any" in
      (sig_, env')

(* ---------- type inference for expressions (mirrors typecheck) ---------- *)

let infer_lit_type (l : lit) : Ast.ty option =
  let d n =
    {
      node = n;
      span = { start = { line = 0; col = 0 }; stop = { line = 0; col = 0 } };
    }
  in
  match l with
  | LitInt _ -> Some (TyName (d "i64"))
  | LitFloat _ -> Some (TyName (d "f64"))
  | LitString _ -> Some (TyName (d "str"))
  | LitBool _ -> Some (TyName (d "bool"))

let dummy_loc n =
  {
    node = n;
    span = { start = { line = 0; col = 0 }; stop = { line = 0; col = 0 } };
  }

(* Check if a name is an imported stdlib package alias *)
let is_imported_package name env = SMap.mem name env.imported_packages

(* Emit cleanup and guard suppression before an early-return expression.
   Returns true if any cleanup was emitted (in which case the caller must
   add indent before the return keyword on a fresh line).
   Returns false if no cleanup was needed (the caller's indent is still
   the current line content). *)
let emit_return_cleanup env buf indent (return_expr : Ast.expr) =
  let has_consumed =
    let consumed =
      Codegen_ownership.collect_consumed_idents
        ~is_user_fn:(fun n -> SMap.mem n env.fns)
        ~is_user_method:(fun tn mn ->
          match SMap.find_opt tn env.impls with
          | Some ii -> SMap.mem mn ii.ii_methods
          | None -> false)
        ~is_stdlib_call:(fun pkg _member ->
          is_imported_package pkg env || SMap.mem pkg env.enums)
        ~lookup_value_ty:(fun n -> lookup_value_ty n env)
        return_expr
    in
    List.exists (fun name -> SMap.mem name env.drop_guards) consumed
  in
  if (not (has_nested_drops env)) && not has_consumed then false
  else begin
    (* Suppress consumed guards BEFORE nested cleanup so bindings whose
       ownership is being transferred are not cleaned up by
       emit_all_nested_cleanup. *)
    Codegen_ownership.suppress_consumed_guards_inline
      ~lookup_guard:(fun n -> SMap.find_opt n env.drop_guards)
      ~is_user_fn:(fun n -> SMap.mem n env.fns)
      ~is_user_method:(fun tn mn ->
        match SMap.find_opt tn env.impls with
        | Some ii -> SMap.mem mn ii.ii_methods
        | None -> false)
      ~is_stdlib_call:(fun pkg _member ->
        is_imported_package pkg env || SMap.mem pkg env.enums)
      ~lookup_value_ty:(fun n -> lookup_value_ty n env)
      buf indent return_expr;
    emit_all_nested_cleanup env buf indent;
    true
  end

(* Convert a Types.ty (from the Interop registry) to an Ast.ty option
   for codegen-side type inference. *)
let interop_ty_to_ast_ty (t : Types.ty) : Ast.ty option =
  match t with
  | Types.TString -> Some (TyName (dummy_loc "str"))
  | Types.TInt 8 -> Some (TyName (dummy_loc "i8"))
  | Types.TInt 16 -> Some (TyName (dummy_loc "i16"))
  | Types.TInt 32 -> Some (TyName (dummy_loc "i32"))
  | Types.TInt 64 -> Some (TyName (dummy_loc "i64"))
  | Types.TUint 8 -> Some (TyName (dummy_loc "u8"))
  | Types.TUint 16 -> Some (TyName (dummy_loc "u16"))
  | Types.TUint 32 -> Some (TyName (dummy_loc "u32"))
  | Types.TUint 64 -> Some (TyName (dummy_loc "u64"))
  | Types.TFloat 32 -> Some (TyName (dummy_loc "f32"))
  | Types.TFloat 64 -> Some (TyName (dummy_loc "f64"))
  | Types.TBool -> Some (TyName (dummy_loc "bool"))
  | Types.TImported (pkg, name) -> Some (TyPath (dummy_loc pkg, dummy_loc name))
  | Types.TVoid -> None
  | _ -> None

(* snake_case to PascalCase conversion for Go function names *)
let snake_to_pascal (s : string) : string =
  s |> String.split_on_char '_'
  |> List.map (fun part ->
      if String.length part = 0 then ""
      else
        String.make 1 (Char.uppercase_ascii part.[0])
        ^ String.sub part 1 (String.length part - 1))
  |> String.concat ""

(* Recursively unify an AST formal type against an AST actual type to collect
   type-parameter bindings.  Only binds parameters whose names appear in
   [tparams].  Used by codegen to infer generic type arguments for enum
   variant constructors with nested payload types. *)
let unify_ast_tparams tparams bindings (formal : Ast.ty) (actual : Ast.ty) =
  let rec go f a =
    match (f, a) with
    | TyName { node = tp; _ }, _ when List.mem tp tparams ->
        if not (Hashtbl.mem bindings tp) then Hashtbl.replace bindings tp a
    | TyGeneric ({ node = fn'; _ }, fargs), TyGeneric ({ node = an; _ }, aargs)
      when fn' = an && List.length fargs = List.length aargs ->
        List.iter2 go fargs aargs
    | TyRef fi, TyRef ai -> go fi ai
    | _ -> ()
  in
  go formal actual

let rec infer_expr_type env (e : Ast.expr) : Ast.ty option =
  match e with
  | ExprLit l -> infer_lit_type l
  | ExprIdent name -> (
      match name.node with
      | "None" -> env.ret_ty (* use return type context *)
      | _ -> (
          match lookup_value name.node env with
          | Some (ty_opt, _) -> ty_opt
          | None -> (
              match SMap.find_opt name.node env.fns with
              | Some _ -> None (* function value *)
              | None -> None)))
  | ExprSelf -> (
      match lookup_value "self" env with
      | Some (ty_opt, _) -> ty_opt
      | None -> None)
  | ExprUnary (_, e) -> infer_expr_type env e
  | ExprBinary ((Eq | Ne | Lt | Gt | Le | Ge | And | Or), _, _) ->
      Some (TyName (dummy_loc "bool"))
  | ExprBinary (Add, l, _) -> (
      match infer_expr_type env l with
      | Some (TyName { node = "str" | "String"; _ }) ->
          Some (TyName (dummy_loc "str"))
      | t -> t)
  | ExprBinary (_, l, _) -> infer_expr_type env l
  | ExprCall (ExprIdent { node = "Some"; _ }, [ arg ]) -> (
      match infer_expr_type env arg with
      | Some inner -> Some (TyGeneric (dummy_loc "Option", [ inner ]))
      | None -> None)
  | ExprCall (ExprIdent { node = "Ok"; _ }, [ arg ]) -> (
      match (infer_expr_type env arg, env.ret_ty) with
      | Some ok_ty, Some (TyGeneric ({ node = "Result"; _ }, [ _; err_ty ])) ->
          Some (TyGeneric (dummy_loc "Result", [ ok_ty; err_ty ]))
      | Some ok_ty, _ ->
          Some
            (TyGeneric (dummy_loc "Result", [ ok_ty; TyName (dummy_loc "str") ]))
      | _ -> env.ret_ty)
  | ExprCall (ExprIdent { node = "Err"; _ }, [ arg ]) -> (
      match (infer_expr_type env arg, env.ret_ty) with
      | Some err_ty, Some (TyGeneric ({ node = "Result"; _ }, [ ok_ty; _ ])) ->
          Some (TyGeneric (dummy_loc "Result", [ ok_ty; err_ty ]))
      | Some err_ty, _ ->
          Some
            (TyGeneric (dummy_loc "Result", [ TyName (dummy_loc "i64"); err_ty ]))
      | _ -> env.ret_ty)
  | ExprCall (ExprIdent fn_name, _) -> (
      match SMap.find_opt fn_name.node env.fns with
      | Some fi -> fi.fi_ret
      | None -> None)
  | ExprCall (ExprPath (type_name, fn_name), args) -> (
      if
        (* Check for imported stdlib package call *)
        is_imported_package type_name.node env
      then
        (* Return the inferred Go type via Interop registry *)
        match Interop.fn_type type_name.node fn_name.node with
        | Some fti -> interop_ty_to_ast_ty fti.fti_ret
        | None -> None
      else
        (* Enum variant constructor or associated function *)
        match SMap.find_opt type_name.node env.enums with
        | Some ei -> (
            match ei.ei_tparams with
            | [] -> Some (TyName type_name)
            | tparams -> (
                (* Infer generic args from variant payload *)
                match List.assoc_opt fn_name.node ei.ei_variants with
                | Some (VTuple field_tys)
                  when List.length field_tys = List.length args ->
                    let bindings = Hashtbl.create 4 in
                    List.iter2
                      (fun ft arg ->
                        match infer_expr_type env arg with
                        | Some t -> unify_ast_tparams tparams bindings ft t
                        | None -> ())
                      field_tys args;
                    let type_args =
                      List.map
                        (fun tp ->
                          match Hashtbl.find_opt bindings tp with
                          | Some t -> t
                          | None -> TyName (dummy_loc "any"))
                        tparams
                    in
                    Some (TyGeneric (type_name, type_args))
                | _ -> Some (TyName type_name)))
        | None -> (
            match SMap.find_opt type_name.node env.impls with
            | Some ii -> (
                match SMap.find_opt fn_name.node ii.ii_assoc_fns with
                | Some fi -> (
                    match fi.fi_ret with
                    | Some TySelf -> Some (TyName type_name)
                    | other -> other)
                | None -> None)
            | None -> None))
  | ExprPath (type_name, member) -> (
      if is_imported_package type_name.node env then
        Some (TyPath (type_name, member))
      else
        match SMap.find_opt type_name.node env.enums with
        | Some _ -> Some (TyName type_name)
        | None -> None)
  | ExprMethodCall (recv, method_name, _) ->
      infer_method_ret_type env recv method_name
  | ExprFieldAccess (recv, field) -> (
      match infer_expr_type env recv with
      | Some (TyName { node = sname; _ }) -> (
          match SMap.find_opt sname env.structs with
          | Some si -> (
              match
                List.find_opt
                  (fun (f : Ast.field) -> f.fd_name.node = field.node)
                  si.si_fields
              with
              | Some f -> Some f.fd_ty
              | None -> None)
          | None -> None)
      | Some (TyPath (pkg_alias, { node = tname; _ })) -> (
          (* Stdlib field access: resolve via Interop registry *)
          match Interop.receiver_field pkg_alias.node tname field.node with
          | Some rf -> interop_ty_to_ast_ty rf.rfi_ty
          | None -> None)
      | _ -> None)
  | ExprStruct (ty, _) -> Some ty
  | ExprStructVariant (type_name, _, _) -> (
      match SMap.find_opt type_name.node env.enums with
      | Some _ -> Some (TyName type_name)
      | None -> None)
  | ExprIf (_, then_blk, _) -> infer_block_type_with_local_fallback env then_blk
  | ExprMatch (scrutinee, arms) -> (
      match arms with
      | arm :: _ -> infer_arm_expr_type env scrutinee arm
      | [] -> None)
  | ExprBlock blk -> infer_block_type env blk
  | ExprReturn _ -> Some (TyName (dummy_loc "void"))
  | ExprBreak | ExprContinue -> None
  | ExprAssign _ -> None
  | ExprQuestion e -> (
      match infer_expr_type env e with
      | Some (TyGeneric ({ node = "Result"; _ }, [ ok_ty; _ ])) -> Some ok_ty
      | Some (TyGeneric ({ node = "Option"; _ }, [ inner ])) -> Some inner
      | _ -> None)
  | ExprArray elems -> (
      match elems with
      | first :: _ -> (
          match infer_expr_type env first with
          | Some elem_ty -> Some (TyGeneric (dummy_loc "Vec", [ elem_ty ]))
          | None -> None)
      | [] -> None)
  | ExprRepeat (elem, _) -> (
      match infer_expr_type env elem with
      | Some elem_ty -> Some (TyGeneric (dummy_loc "Vec", [ elem_ty ]))
      | None -> None)
  | ExprIndex (arr, _) -> (
      match infer_expr_type env arr with
      | Some (TyGeneric ({ node = "Vec"; _ }, [ inner ])) -> Some inner
      | _ -> None)
  | ExprCast (_, ty) -> Some ty
  | ExprLoop _ | ExprWhile _ | ExprFor _ -> None
  | ExprLambda _ -> None
  | ExprCall (_, _) -> None

and infer_block_type env blk =
  match blk.final_expr with
  | Some e -> infer_expr_type env e
  | None -> (
      match List.rev blk.stmts with
      | StmtExpr e :: _ -> infer_expr_type env e
      | _ -> None)

(* Try to infer the type of an identifier bound inside a block by scanning
   the block's statements for a matching let binding. *)
and infer_local_binding_ty env blk name =
  let rec scan stmts =
    match stmts with
    | [] -> None
    | StmtLet { pat = PatBind n; ty; init; _ } :: _ when n.node = name -> (
        match ty with Some t -> Some t | None -> infer_expr_type env init)
    | _ :: rest -> scan rest
  in
  scan blk.stmts

(* Infer block type, falling back to local let-binding type when the final
   expression is an identifier bound inside the block. *)
and infer_block_type_with_local_fallback env blk =
  match infer_block_type env blk with
  | Some t -> Some t
  | None -> (
      match blk.final_expr with
      | Some (ExprIdent { node = name; _ }) ->
          infer_local_binding_ty env blk name
      | _ -> None)

(* Try to infer the type of a variable bound by a match pattern. *)
and infer_pat_binding_ty env scrutinee_ty pat name =
  let rec find pat ty =
    match pat with
    | PatBind n when n.node = name -> Some ty
    | PatBind _ -> None
    | PatWild | PatLit _ -> None
    | PatTuple (ename, vname, pats) -> (
        let payload_tys =
          match ty with
          | TyGeneric ({ node = "Result"; _ }, [ ok_ty; err_ty ]) -> (
              match vname.node with
              | "Ok" when List.length pats = 1 -> Some [ ok_ty ]
              | "Err" when List.length pats = 1 -> Some [ err_ty ]
              | _ -> None)
          | TyGeneric ({ node = "Option"; _ }, [ inner_ty ]) -> (
              match vname.node with
              | "Some" when List.length pats = 1 -> Some [ inner_ty ]
              | _ -> None)
          | _ -> (
              match SMap.find_opt ename.node env.enums with
              | Some ei -> (
                  match List.assoc_opt vname.node ei.ei_variants with
                  | Some (VTuple tys) -> Some tys
                  | Some (VStruct fields) ->
                      Some (List.map (fun f -> f.fd_ty) fields)
                  | _ -> None)
              | None -> None)
        in
        match payload_tys with
        | Some tys -> find_in_list pats tys
        | None -> None)
    | PatStruct (ename, vname, field_pats) -> (
        match SMap.find_opt ename.node env.enums with
        | Some ei -> (
            match List.assoc_opt vname.node ei.ei_variants with
            | Some (VStruct fields) -> find_in_field_pats field_pats fields
            | _ -> None)
        | None -> None)
  and find_in_list pats tys =
    match (pats, tys) with
    | p :: ps, t :: ts -> (
        match find p t with Some r -> Some r | None -> find_in_list ps ts)
    | _ -> None
  and find_in_field_pats field_pats fields =
    match field_pats with
    | [] -> None
    | fp :: rest -> (
        let field_ty_opt =
          List.find_opt (fun f -> f.fd_name.node = fp.fp_name.node) fields
        in
        match field_ty_opt with
        | None -> find_in_field_pats rest fields
        | Some f -> (
            match fp.fp_pat with
            | None -> (
                match find (PatBind fp.fp_name) f.fd_ty with
                | Some r -> Some r
                | None -> find_in_field_pats rest fields)
            | Some p -> (
                match find p f.fd_ty with
                | Some r -> Some r
                | None -> find_in_field_pats rest fields)))
  in
  find pat scrutinee_ty

(* Infer the type of a match arm expression, handling blocks with local
   bindings and pattern-bound identifiers. *)
and infer_arm_expr_type env scrutinee arm =
  match infer_expr_type env arm.arm_expr with
  | Some t -> Some t
  | None -> (
      match arm.arm_expr with
      | ExprBlock blk -> infer_block_type_with_local_fallback env blk
      | ExprIdent { node = name; _ } -> (
          match infer_expr_type env scrutinee with
          | Some ty -> infer_pat_binding_ty env ty arm.arm_pat name
          | None -> None)
      | _ -> None)

and infer_method_ret_type env recv method_name =
  match infer_expr_type env recv with
  | Some (TyGeneric ({ node = "Vec"; _ }, [ inner ])) -> (
      match method_name.node with
      | "len" -> Some (TyName (dummy_loc "i64"))
      | "pop" -> Some (TyGeneric (dummy_loc "Option", [ inner ]))
      | "push" -> None
      | _ -> None)
  | Some (TyGeneric ({ node = "HashMap"; _ }, [ _k; v ])) -> (
      match method_name.node with
      | "len" -> Some (TyName (dummy_loc "i64"))
      | "get" -> Some (TyGeneric (dummy_loc "Option", [ v ]))
      | "contains_key" -> Some (TyName (dummy_loc "bool"))
      | _ -> None)
  | Some (TyName { node = "str" | "String"; _ }) -> (
      match method_name.node with
      | "len" -> Some (TyName (dummy_loc "i64"))
      | _ -> None)
  | Some (TyName { node = tname; _ }) -> (
      match SMap.find_opt tname env.impls with
      | Some ii -> (
          match SMap.find_opt method_name.node ii.ii_methods with
          | Some mi -> mi.mi_ret
          | None -> None)
      | None -> None)
  | Some (TyPath (pkg_alias, { node = tname; _ })) -> (
      (* Stdlib receiver method return types: resolve via Interop registry *)
      match Interop.receiver_method pkg_alias.node tname method_name.node with
      | Some rm -> interop_ty_to_ast_ty rm.rmi_ret
      | None -> None)
  | _ -> None

(* Check whether a trait uses Self in any method signature *)
let trait_uses_self (items : Ast.trait_item list) : bool =
  let rec ty_has_self (t : Ast.ty) : bool =
    match t with
    | TySelf -> true
    | TyName _ -> false
    | TyGeneric (_, args) -> List.exists ty_has_self args
    | TyRef inner -> ty_has_self inner
    | TyTuple ts -> List.exists ty_has_self ts
    | TyPath _ -> false
  in
  List.exists
    (fun (ti : Ast.trait_item) ->
      match ti with
      | TraitFnSig sig_ -> (
          List.exists
            (fun (p : Ast.param) -> ty_has_self p.p_ty)
            sig_.sig_params
          || match sig_.sig_ret with Some t -> ty_has_self t | None -> false)
      | TraitFnDecl fd -> (
          List.exists (fun (p : Ast.param) -> ty_has_self p.p_ty) fd.fn_params
          || match fd.fn_ret with Some t -> ty_has_self t | None -> false))
    items

(* ---------- collect type info from program ---------- *)

let collect_env (prog : Ast.program) : cg_env =
  let shared =
    {
      tmp_counter = 0;
      needs_option_struct = false;
      needs_rgo_repeat = false;
      needs_fmt = false;
      needs_errors = false;
      needs_math = false;
      needs_imported_pkgs = false;
      needs_result_struct = false;
      self_ref_traits = [];
    }
  in
  (* Build imported_packages from program imports.
     Maps alias -> (go_import_path, go_package_name).
     e.g. "http" -> ("net/http", "http") *)
  let imported_pkgs =
    List.fold_left
      (fun acc (imp : Ast.import_path) ->
        let segments =
          List.map (fun (s : ident located) -> s.node) imp.imp_segments
        in
        let alias =
          match Interop.alias_for_path segments with
          | Some a -> a
          | None -> List.nth segments (List.length segments - 1)
        in
        (* Resolve Go import path and package name via Interop registry,
           falling back to segment-derived values for future packages. *)
        let go_import_path =
          match Interop.go_import_path alias with
          | Some p -> p
          | None -> String.concat "/" segments
        in
        let go_pkg_name =
          match Interop.go_pkg_name alias with
          | Some n -> n
          | None -> List.nth segments (List.length segments - 1)
        in
        SMap.add alias (go_import_path, go_pkg_name) acc)
      SMap.empty prog.imports
  in
  let env =
    {
      structs = SMap.empty;
      enums = SMap.empty;
      fns = SMap.empty;
      impls = SMap.empty;
      traits = SMap.empty;
      trait_impls = SMap.empty;
      ret_ty = None;
      self_type_name = None;
      self_type = None;
      impl_type_params = [];
      type_params = [];
      values = [ SMap.empty ];
      imported_packages = imported_pkgs;
      drop_guards = SMap.empty;
      scope_drops = [];
      loop_body_depths = [];
      result_decomps = SMap.empty;
      in_iife = false;
      shared;
    }
  in
  List.fold_left
    (fun env item ->
      match item with
      | ItemStruct { s_name; s_fields; s_generics; _ } ->
          ignore s_generics;
          let si = { si_fields = s_fields } in
          { env with structs = SMap.add s_name.node si env.structs }
      | ItemEnum { e_name; e_variants; e_generics; _ } ->
          let variants =
            List.map
              (fun (v : variant) ->
                let shape =
                  match v.var_fields with
                  | None -> VUnit
                  | Some (TupleFields tys) -> VTuple tys
                  | Some (StructFields fields) -> VStruct fields
                in
                (v.var_name.node, shape))
              e_variants
          in
          let tparams =
            List.map (fun (tp : type_param) -> tp.tp_name.node) e_generics
          in
          let ei = { ei_variants = variants; ei_tparams = tparams } in
          { env with enums = SMap.add e_name.node ei env.enums }
      | ItemFn fd ->
          let fi = { fi_ret = fd.fn_ret } in
          { env with fns = SMap.add fd.fn_name.node fi env.fns }
      | ItemImpl { i_ty; i_items; _ } ->
          let type_name =
            match i_ty with
            | TyName n -> n.node
            | TyGeneric (n, _) -> n.node
            | _ -> "unknown"
          in
          let existing =
            match SMap.find_opt type_name env.impls with
            | Some ii -> ii
            | None -> { ii_methods = SMap.empty; ii_assoc_fns = SMap.empty }
          in
          let updated =
            List.fold_left
              (fun ii (fd : fn_decl) ->
                match fd.fn_self with
                | Some self_p ->
                    let mi =
                      {
                        mi_self = Some self_p;
                        mi_params = fd.fn_params;
                        mi_ret = fd.fn_ret;
                      }
                    in
                    {
                      ii with
                      ii_methods = SMap.add fd.fn_name.node mi ii.ii_methods;
                    }
                | None ->
                    let fi = { fi_ret = fd.fn_ret } in
                    {
                      ii with
                      ii_assoc_fns = SMap.add fd.fn_name.node fi ii.ii_assoc_fns;
                    })
              existing i_items
          in
          { env with impls = SMap.add type_name updated env.impls }
      | ItemTraitImpl { ti_trait; ti_ty; ti_items; _ } ->
          let type_name =
            match ti_ty with
            | TyName n -> n.node
            | TyGeneric (n, _) -> n.node
            | _ -> "unknown"
          in
          let existing =
            match SMap.find_opt type_name env.impls with
            | Some ii -> ii
            | None -> { ii_methods = SMap.empty; ii_assoc_fns = SMap.empty }
          in
          let updated =
            List.fold_left
              (fun ii (fd : fn_decl) ->
                match fd.fn_self with
                | Some self_p ->
                    let mi =
                      {
                        mi_self = Some self_p;
                        mi_params = fd.fn_params;
                        mi_ret = fd.fn_ret;
                      }
                    in
                    {
                      ii with
                      ii_methods = SMap.add fd.fn_name.node mi ii.ii_methods;
                    }
                | None ->
                    let fi = { fi_ret = fd.fn_ret } in
                    {
                      ii with
                      ii_assoc_fns = SMap.add fd.fn_name.node fi ii.ii_assoc_fns;
                    })
              existing ti_items
          in
          let impl_key = ti_trait.node ^ ":" ^ type_name in
          {
            env with
            impls = SMap.add type_name updated env.impls;
            trait_impls = SMap.add impl_key () env.trait_impls;
          }
      | ItemTrait { t_name; t_items; _ } ->
          let tic = { tic_items = t_items } in
          if trait_uses_self t_items then
            env.shared.self_ref_traits <-
              t_name.node :: env.shared.self_ref_traits;
          { env with traits = SMap.add t_name.node tic env.traits }
      | ItemLet { pat; ty; init; is_mut } -> (
          match pat with
          | PatBind name ->
              let binding_ty =
                match ty with
                | Some t -> Some t
                | None -> infer_expr_type env init
              in
              add_value name.node binding_ty ~is_mut env
          | _ -> env))
    env prog.items

(* ---------- expression context: statement vs expression position ---------- *)

type expr_ctx = CtxStmt | CtxExpr

(* ---------- string escaping ---------- *)

let escape_string s =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '"';
  String.iter
    (fun c ->
      match c with
      | '\\' -> Buffer.add_string buf "\\\\"
      | '"' -> Buffer.add_string buf "\\\""
      | '\n' -> Buffer.add_string buf "\\n"
      | '\t' -> Buffer.add_string buf "\\t"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"';
  Buffer.contents buf

(* ---------- Go name generation ---------- *)

let go_fn_name is_pub name =
  if name = "main" then "main"
  else
    let base = escape_ident name in
    if is_pub then capitalize base else base

let go_field_name is_pub name =
  let base = escape_ident name in
  if is_pub then capitalize base else base

let go_method_name is_pub name =
  let base = escape_ident name in
  if is_pub then capitalize base else base

(* Type name for generics suffix: e.g. Box -> Box[T any] *)
let go_generics_decl ?(self_ref_traits = []) (tps : type_param list) : string =
  if tps = [] then ""
  else
    let params =
      List.map
        (fun (tp : type_param) ->
          let bound =
            match tp.tp_bound with
            | None | Some [] -> "any"
            | Some [ single ] ->
                let name = capitalize single.node in
                (* If the trait is self-referential, instantiate with the type param *)
                if List.mem single.node self_ref_traits then
                  name ^ "[" ^ tp.tp_name.node ^ "]"
                else name
            | Some bounds ->
                let bound_strs =
                  List.map
                    (fun b ->
                      let name = capitalize b.node in
                      if List.mem b.node self_ref_traits then
                        name ^ "[" ^ tp.tp_name.node ^ "]"
                      else name)
                    bounds
                in
                "interface {\n"
                ^ String.concat ""
                    (List.map (fun s -> "\t" ^ s ^ "\n") bound_strs)
                ^ "}"
          in
          tp.tp_name.node ^ " " ^ bound)
        tps
    in
    "[" ^ String.concat ", " params ^ "]"

let go_generics_use (tps : type_param list) : string =
  if tps = [] then ""
  else
    "["
    ^ String.concat ", "
        (List.map (fun (tp : type_param) -> tp.tp_name.node) tps)
    ^ "]"

(* ---------- code generation ---------- *)

let gen_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Ne -> "!="
  | Lt -> "<"
  | Gt -> ">"
  | Le -> "<="
  | Ge -> ">="
  | And -> "&&"
  | Or -> "||"

let gen_assign_op = function
  | Assign -> "="
  | AddAssign -> "+="
  | SubAssign -> "-="
  | MulAssign -> "*="
  | DivAssign -> "/="

let rec gen_expr env buf indent (ctx : expr_ctx) (e : Ast.expr) : unit =
  match e with
  | ExprLit (LitInt s) -> Buffer.add_string buf s
  | ExprLit (LitFloat s) -> Buffer.add_string buf s
  | ExprLit (LitString s) -> Buffer.add_string buf (escape_string s)
  | ExprLit (LitBool true) -> Buffer.add_string buf "true"
  | ExprLit (LitBool false) -> Buffer.add_string buf "false"
  | ExprIdent { node = "None"; _ } -> gen_none env buf
  | ExprIdent name -> Buffer.add_string buf (escape_ident name.node)
  | ExprSelf -> Buffer.add_string buf "self"
  | ExprUnary (Neg, e) ->
      Buffer.add_char buf '-';
      gen_expr_parens env buf indent e
  | ExprUnary (Not, e) ->
      Buffer.add_char buf '!';
      gen_expr_parens env buf indent e
  | ExprBinary (op, l, r) ->
      gen_expr_parens env buf indent l;
      Buffer.add_char buf ' ';
      Buffer.add_string buf (gen_binop op);
      Buffer.add_char buf ' ';
      gen_expr_parens env buf indent r
  | ExprCall (ExprIdent { node = "println"; _ }, args) ->
      env.shared.needs_fmt <- true;
      Buffer.add_string buf "fmt.Println(";
      gen_args env buf indent args;
      Buffer.add_char buf ')'
  | ExprCall (ExprIdent { node = "print"; _ }, args) ->
      env.shared.needs_fmt <- true;
      Buffer.add_string buf "fmt.Print(";
      gen_args env buf indent args;
      Buffer.add_char buf ')'
  | ExprCall (ExprIdent { node = "panic"; _ }, args) ->
      Buffer.add_string buf "panic(";
      gen_args env buf indent args;
      Buffer.add_char buf ')'
  | ExprCall (ExprIdent { node = "sqrt"; _ }, args) ->
      env.shared.needs_math <- true;
      Buffer.add_string buf "math.Sqrt(";
      gen_args env buf indent args;
      Buffer.add_char buf ')'
  | ExprCall (ExprIdent { node = "abs"; _ }, args) ->
      env.shared.needs_math <- true;
      Buffer.add_string buf "math.Abs(";
      gen_args env buf indent args;
      Buffer.add_char buf ')'
  | ExprCall (ExprIdent { node = "Some"; _ }, [ arg ]) ->
      gen_some env buf indent arg
  | ExprCall (ExprIdent { node = "Ok"; _ }, [ arg ]) -> (
      let ok_ty = infer_expr_type env arg in
      let result_ty =
        match (ok_ty, env.ret_ty) with
        | Some ok_ty, Some (TyGeneric ({ node = "Result"; _ }, [ _; err_ty ]))
          ->
            Some (TyGeneric (dummy_loc "Result", [ ok_ty; err_ty ]))
        | Some ok_ty, _ ->
            Some
              (TyGeneric
                 (dummy_loc "Result", [ ok_ty; TyName (dummy_loc "str") ]))
        | _ -> env.ret_ty
      in
      match result_ty with
      | Some (TyGeneric ({ node = "Result"; _ }, [ ok_ty; err_ty ])) ->
          env.shared.needs_result_struct <- true;
          Printf.bprintf buf "Result[%s, %s]{ok: true, value: "
            (go_type env ok_ty) (go_type env err_ty);
          gen_expr env buf indent CtxExpr arg;
          Buffer.add_string buf "}"
      | _ ->
          (* Fallback for cases where type inference fails *)
          gen_expr env buf indent CtxExpr arg)
  | ExprCall (ExprIdent { node = "Err"; _ }, [ arg ]) -> (
      let err_ty = infer_expr_type env arg in
      let result_ty =
        match (err_ty, env.ret_ty) with
        | Some err_ty, Some (TyGeneric ({ node = "Result"; _ }, [ ok_ty; _ ]))
          ->
            Some (TyGeneric (dummy_loc "Result", [ ok_ty; err_ty ]))
        | Some err_ty, _ ->
            Some
              (TyGeneric
                 (dummy_loc "Result", [ TyName (dummy_loc "i64"); err_ty ]))
        | _ -> env.ret_ty
      in
      match result_ty with
      | Some (TyGeneric ({ node = "Result"; _ }, [ ok_ty; err_ty ])) ->
          env.shared.needs_result_struct <- true;
          Printf.bprintf buf "Result[%s, %s]{err: " (go_type env ok_ty)
            (go_type env err_ty);
          gen_expr env buf indent CtxExpr arg;
          Buffer.add_string buf "}"
      | _ ->
          env.shared.needs_errors <- true;
          Buffer.add_string buf "errors.New(";
          gen_expr env buf indent CtxExpr arg;
          Buffer.add_char buf ')')
  | ExprCall (ExprPath (type_name, fn_name), args) ->
      gen_path_call env buf indent type_name fn_name args
  | ExprCall (callee, args) ->
      gen_expr env buf indent CtxExpr callee;
      Buffer.add_char buf '(';
      gen_args env buf indent args;
      Buffer.add_char buf ')'
  | ExprMethodCall (recv, method_name, args) ->
      gen_method_call env buf indent recv method_name args
  | ExprFieldAccess (e, field) -> (
      let recv_ty = infer_expr_type env e in
      match recv_ty with
      | Some (TyPath (pkg_alias, tname))
        when is_imported_package pkg_alias.node env ->
          (* Stdlib field access: resolve Go name via Interop registry *)
          env.shared.needs_imported_pkgs <- true;
          gen_expr env buf indent CtxExpr e;
          Buffer.add_char buf '.';
          let go_field =
            match
              Interop.go_receiver_field_name pkg_alias.node tname.node
                field.node
            with
            | Some name -> name
            | None -> snake_to_pascal field.node
          in
          Buffer.add_string buf go_field
      | _ ->
          gen_expr env buf indent CtxExpr e;
          Buffer.add_char buf '.';
          (* Field names: capitalize for pub fields *)
          let is_pub = field_is_pub env e field.node in
          Buffer.add_string buf (go_field_name is_pub field.node))
  | ExprPath (type_name, member_name) ->
      if is_imported_package type_name.node env then begin
        (* Imported stdlib member in value position: resolve via Interop registry *)
        env.shared.needs_imported_pkgs <- true;
        let go_pkg =
          match SMap.find_opt type_name.node env.imported_packages with
          | Some (_, go_pkg_name) -> go_pkg_name
          | None -> type_name.node
        in
        let go_name =
          match Interop.go_member_name type_name.node member_name.node with
          | Some name -> name
          | None -> snake_to_pascal member_name.node
        in
        Buffer.add_string buf (go_pkg ^ "." ^ go_name)
      end
      else begin
        (* Unit enum variant: EnumVariant{} or EnumVariant[T]{} *)
        let type_suffix =
          match SMap.find_opt type_name.node env.enums with
          | Some ei when ei.ei_tparams <> [] ->
              let tparam_types =
                List.map
                  (fun tp -> if List.mem tp env.type_params then tp else "any")
                  ei.ei_tparams
              in
              "[" ^ String.concat ", " tparam_types ^ "]"
          | _ -> ""
        in
        Buffer.add_string buf
          (type_name.node ^ member_name.node ^ type_suffix ^ "{}")
      end
  | ExprStruct (ty, fields) -> gen_struct_literal env buf indent ty fields
  | ExprStructVariant (type_name, variant_name, fields) ->
      gen_struct_variant_literal env buf indent type_name variant_name fields
  | ExprIf (cond, then_blk, else_blk) ->
      gen_if env buf indent ctx cond then_blk else_blk
  | ExprMatch (scrutinee, arms) -> gen_match env buf indent ctx scrutinee arms
  | ExprBlock blk -> gen_block_expr env buf indent ctx blk
  | ExprReturn (Some (ExprCall (ExprIdent { node = "Ok"; _ }, [ arg ]))) -> (
      (* Ownership: if the Ok argument is a Drop-type binding, suppress its
         guard BEFORE emit_return_cleanup so ownership transfers to the caller. *)
      (match arg with
      | ExprIdent { node = n; _ } -> (
          match SMap.find_opt n env.drop_guards with
          | Some guard -> Printf.bprintf buf "%s%s = false\n" indent guard
          | None -> ())
      | _ -> ());
      let _cleaned = emit_return_cleanup env buf indent arg in
      Buffer.add_string buf indent;
      match env.ret_ty with
      | Some (TyGeneric ({ node = "Result"; _ }, _))
        when should_flatten_result_return env ->
          Buffer.add_string buf "return ";
          gen_expr env buf indent CtxExpr arg;
          Buffer.add_string buf ", nil"
      | _ ->
          Buffer.add_string buf "return ";
          gen_expr env buf indent CtxExpr arg)
  | ExprReturn (Some (ExprCall (ExprIdent { node = "Err"; _ }, [ arg ]))) -> (
      (* Ownership: if the Err argument is a Drop-type binding, suppress its
         guard BEFORE emit_return_cleanup so ownership transfers to the caller. *)
      (match arg with
      | ExprIdent { node = n; _ } -> (
          match SMap.find_opt n env.drop_guards with
          | Some guard -> Printf.bprintf buf "%s%s = false\n" indent guard
          | None -> ())
      | _ -> ());
      let _cleaned = emit_return_cleanup env buf indent arg in
      Buffer.add_string buf indent;
      match env.ret_ty with
      | Some (TyGeneric ({ node = "Result"; _ }, [ ok_ty; _ ]))
        when should_flatten_result_return env -> (
          env.shared.needs_errors <- true;
          Buffer.add_string buf "return ";
          Buffer.add_string buf (go_zero_value env ok_ty);
          Buffer.add_string buf ", ";
          match arg with
          | ExprLit (LitString _) ->
              Buffer.add_string buf "errors.New(";
              gen_expr env buf indent CtxExpr arg;
              Buffer.add_char buf ')'
          | _ ->
              Buffer.add_string buf "errors.New(";
              gen_expr env buf indent CtxExpr arg;
              Buffer.add_char buf ')')
      | _ ->
          Buffer.add_string buf "return ";
          gen_expr env buf indent CtxExpr
            (ExprCall
               ( ExprIdent { node = "Err"; span = (dummy_loc "Err").span },
                 [ arg ] )))
  | ExprReturn (Some (ExprCall (ExprIdent { node = "Some"; _ }, [ arg ]))) -> (
      (* Ownership: if the Some argument is a Drop-type binding, suppress its
         guard BEFORE emit_return_cleanup so ownership transfers to the caller. *)
      (match arg with
      | ExprIdent { node = n; _ } -> (
          match SMap.find_opt n env.drop_guards with
          | Some guard -> Printf.bprintf buf "%s%s = false\n" indent guard
          | None -> ())
      | _ -> ());
      let _cleaned = emit_return_cleanup env buf indent arg in
      Buffer.add_string buf indent;
      match env.ret_ty with
      | Some (TyGeneric ({ node = "Option"; _ }, [ inner ])) ->
          Buffer.add_string buf "return ";
          gen_some_for_type env buf indent inner arg
      | _ ->
          Buffer.add_string buf "return ";
          gen_some env buf indent arg)
  | ExprReturn (Some (ExprIdent { node = "None"; _ })) ->
      let cleaned = has_nested_drops env in
      if cleaned then begin
        Buffer.add_char buf '\n';
        emit_all_nested_cleanup env buf indent
      end;
      Buffer.add_string buf indent;
      gen_return_none env buf
  | ExprReturn (Some (ExprIdent { node = name; _ } as e)) ->
      (* Ownership: suppress the returned binding's guard BEFORE nested cleanup
         so the returned value is not cleaned up by emit_all_nested_cleanup,
         then clean up remaining nested scope drops. *)
      (match SMap.find_opt name env.drop_guards with
      | Some guard -> Printf.bprintf buf "%s%s = false\n" indent guard
      | None -> ());
      let cleaned = has_nested_drops env in
      if cleaned then begin
        Buffer.add_char buf '\n';
        emit_all_nested_cleanup env buf indent
      end;
      Buffer.add_string buf indent;
      Buffer.add_string buf "return ";
      gen_expr env buf indent CtxExpr e
  | ExprReturn (Some e) ->
      (* Ownership: emit cleanup for nested scope drops before returning,
         and suppress guards for consumed identifiers in the return expr. *)
      let _cleaned = emit_return_cleanup env buf indent e in
      Buffer.add_string buf indent;
      Buffer.add_string buf "return ";
      gen_expr env buf indent CtxExpr e
  | ExprReturn None ->
      let cleaned = has_nested_drops env in
      if cleaned then begin
        Buffer.add_char buf '\n';
        emit_all_nested_cleanup env buf indent
      end;
      Buffer.add_string buf indent;
      Buffer.add_string buf "return"
  | ExprBreak ->
      let has_loop_drops =
        match env.loop_body_depths with
        | target_depth :: _ ->
            let current_depth = List.length env.values in
            let scopes_to_clean = current_depth - target_depth in
            scopes_to_clean > 0
        | [] -> false
      in
      if has_loop_drops then begin
        Buffer.add_char buf '\n';
        emit_loop_cleanup env buf indent
      end;
      Buffer.add_string buf indent;
      Buffer.add_string buf "break"
  | ExprContinue ->
      let has_loop_drops =
        match env.loop_body_depths with
        | target_depth :: _ ->
            let current_depth = List.length env.values in
            let scopes_to_clean = current_depth - target_depth in
            scopes_to_clean > 0
        | [] -> false
      in
      if has_loop_drops then begin
        Buffer.add_char buf '\n';
        emit_loop_cleanup env buf indent
      end;
      Buffer.add_string buf indent;
      Buffer.add_string buf "continue"
  | ExprAssign (op, lhs, rhs) ->
      gen_expr env buf indent CtxExpr lhs;
      Buffer.add_char buf ' ';
      Buffer.add_string buf (gen_assign_op op);
      Buffer.add_char buf ' ';
      gen_expr env buf indent CtxExpr rhs
  | ExprQuestion e -> gen_question env buf indent e
  | ExprArray elems -> gen_array_literal env buf indent elems
  | ExprRepeat (elem, count) -> gen_repeat env buf indent elem count
  | ExprIndex (arr, idx) ->
      gen_expr env buf indent CtxExpr arr;
      Buffer.add_char buf '[';
      gen_expr env buf indent CtxExpr idx;
      Buffer.add_char buf ']'
  | ExprCast (e, ty) -> (
      match (e, ty) with
      | ( ExprCall (ExprIdent { node = "Ok"; _ }, [ arg ]),
          TyGeneric ({ node = "Result"; _ }, [ ok_ty; err_ty ]) ) ->
          env.shared.needs_result_struct <- true;
          Printf.bprintf buf "Result[%s, %s]{ok: true, value: "
            (go_type env ok_ty) (go_type env err_ty);
          gen_expr env buf indent CtxExpr arg;
          Buffer.add_string buf "}"
      | ( ExprCall (ExprIdent { node = "Err"; _ }, [ arg ]),
          TyGeneric ({ node = "Result"; _ }, [ ok_ty; err_ty ]) ) ->
          env.shared.needs_result_struct <- true;
          Printf.bprintf buf "Result[%s, %s]{err: " (go_type env ok_ty)
            (go_type env err_ty);
          gen_expr env buf indent CtxExpr arg;
          Buffer.add_string buf "}"
      | ( ExprCall (ExprIdent { node = "Some"; _ }, [ arg ]),
          TyGeneric ({ node = "Option"; _ }, [ inner_ty ]) ) ->
          if is_nullable_ty env inner_ty then begin
            env.shared.needs_option_struct <- true;
            Printf.bprintf buf "rgo_some[%s](" (go_type env inner_ty);
            gen_expr env buf indent CtxExpr arg;
            Buffer.add_char buf ')'
          end
          else gen_new_expr env buf indent (Some inner_ty) arg
      | ( ExprIdent { node = "None"; _ },
          TyGeneric ({ node = "Option"; _ }, [ inner_ty ]) ) ->
          gen_none_for_type env buf inner_ty
      | _ ->
          Buffer.add_string buf (go_type env ty);
          Buffer.add_char buf '(';
          gen_expr env buf indent CtxExpr e;
          Buffer.add_char buf ')')
  | ExprLoop (None, blk) ->
      Buffer.add_string buf "for {\n";
      let loop_body_depth = List.length env.values in
      let inner =
        { env with loop_body_depths = loop_body_depth :: env.loop_body_depths }
      in
      gen_block_stmts inner buf (indent ^ "\t") blk;
      Buffer.add_string buf indent;
      Buffer.add_char buf '}'
  | ExprLoop (Some cond, blk) ->
      Buffer.add_string buf "for ";
      gen_expr env buf indent CtxExpr cond;
      Buffer.add_string buf " {\n";
      let loop_body_depth = List.length env.values in
      let inner =
        { env with loop_body_depths = loop_body_depth :: env.loop_body_depths }
      in
      gen_block_stmts inner buf (indent ^ "\t") blk;
      Buffer.add_string buf indent;
      Buffer.add_char buf '}'
  | ExprWhile (cond, blk) ->
      Buffer.add_string buf "for ";
      gen_expr env buf indent CtxExpr cond;
      Buffer.add_string buf " {\n";
      let loop_body_depth = List.length env.values in
      let inner =
        { env with loop_body_depths = loop_body_depth :: env.loop_body_depths }
      in
      gen_block_stmts inner buf (indent ^ "\t") blk;
      Buffer.add_string buf indent;
      Buffer.add_char buf '}'
  | ExprFor (binding, iter_expr, blk) ->
      Buffer.add_string buf "for _, ";
      Buffer.add_string buf (escape_ident binding.node);
      Buffer.add_string buf " := range ";
      gen_expr env buf indent CtxExpr iter_expr;
      Buffer.add_string buf " {\n";
      let inner = push_scope ~is_function:false env in
      let loop_body_depth = List.length inner.values in
      let inner =
        {
          inner with
          loop_body_depths = loop_body_depth :: inner.loop_body_depths;
        }
      in
      let inner =
        add_value binding.node
          (infer_iter_elem_type env iter_expr)
          ~is_mut:false inner
      in
      gen_block_stmts inner buf (indent ^ "\t") blk;
      Buffer.add_string buf indent;
      Buffer.add_char buf '}'
  | ExprLambda (params, ret_ty, body) ->
      (* Zero-capture anonymous function → Go func literal *)
      Buffer.add_string buf "func(";
      let inner = push_scope ~is_function:true env in
      List.iteri
        (fun i (p : Ast.param) ->
          if i > 0 then Buffer.add_string buf ", ";
          Buffer.add_string buf (escape_ident p.p_name.node);
          Buffer.add_string buf " ";
          Buffer.add_string buf (go_type inner p.p_ty))
        params;
      Buffer.add_string buf ")";
      (match ret_ty with
      | Some t ->
          Buffer.add_string buf " ";
          Buffer.add_string buf (go_type inner t)
      | None -> ());
      Buffer.add_string buf " {\n";
      let inner =
        List.fold_left
          (fun e (p : Ast.param) ->
            add_value p.p_name.node (Some p.p_ty) ~is_mut:p.p_mut e)
          inner params
      in
      let inner =
        {
          inner with
          ret_ty = (match ret_ty with Some _ -> ret_ty | None -> env.ret_ty);
        }
      in
      let is_nonvoid =
        match ret_ty with
        | Some (TyName { node = "void"; _ }) -> false
        | Some _ -> true
        | None -> false
      in
      if is_nonvoid then
        gen_function_body inner buf (indent ^ "\t") body inner.ret_ty
      else gen_block_stmts inner buf (indent ^ "\t") body;
      Buffer.add_string buf indent;
      Buffer.add_char buf '}'

and infer_iter_elem_type env iter_expr =
  match infer_expr_type env iter_expr with
  | Some (TyGeneric ({ node = "Vec"; _ }, [ inner ])) -> Some inner
  | _ -> None

and gen_expr_parens env buf indent e =
  match e with
  | ExprBinary _ | ExprUnary _ ->
      Buffer.add_char buf '(';
      gen_expr env buf indent CtxExpr e;
      Buffer.add_char buf ')'
  | _ -> gen_expr env buf indent CtxExpr e

and gen_args env buf indent args =
  List.iteri
    (fun i a ->
      if i > 0 then Buffer.add_string buf ", ";
      gen_expr env buf indent CtxExpr a)
    args

and gen_none_for_type env buf (inner_ty : Ast.ty) =
  if is_nullable_ty env inner_ty then begin
    env.shared.needs_option_struct <- true;
    Printf.bprintf buf "rgo_none[%s]()" (go_type env inner_ty)
  end
  else Printf.bprintf buf "(*%s)(nil)" (go_type env inner_ty)

and gen_none env buf =
  match env.ret_ty with
  | Some (TyGeneric ({ node = "Option"; _ }, [ inner ])) ->
      gen_none_for_type env buf inner
  | _ -> Buffer.add_string buf "nil"

and gen_return_none env buf =
  match env.ret_ty with
  | Some (TyGeneric ({ node = "Option"; _ }, [ inner ])) ->
      Buffer.add_string buf "return ";
      gen_none_for_type env buf inner
  | _ -> Buffer.add_string buf "return nil"

and gen_new_expr env buf indent (inner_ty : Ast.ty option) arg =
  (* Emit new(ty(lit)) for literal args to avoid Go untyped constant issues
     (e.g., new(0) produces *int, but we need *int64). Also handles negative
     literals like -1, -1.5 which are ExprUnary(Neg, ExprLit ...). *)
  Buffer.add_string buf "new(";
  let is_int_literal = function
    | Ast.ExprLit (LitInt _) -> true
    | Ast.ExprUnary (Neg, ExprLit (LitInt _)) -> true
    | _ -> false
  in
  let is_float_literal = function
    | Ast.ExprLit (LitFloat _) -> true
    | Ast.ExprUnary (Neg, ExprLit (LitFloat _)) -> true
    | _ -> false
  in
  let needs_cast =
    match inner_ty with
    | Some ty when is_int_literal arg ->
        let gt = go_type env ty in
        if gt <> "int" then begin
          Buffer.add_string buf gt;
          Buffer.add_char buf '(';
          true
        end
        else false
    | Some ty when is_float_literal arg ->
        let gt = go_type env ty in
        if gt <> "float64" then begin
          Buffer.add_string buf gt;
          Buffer.add_char buf '(';
          true
        end
        else false
    | _ -> false
  in
  gen_expr env buf indent CtxExpr arg;
  if needs_cast then Buffer.add_char buf ')';
  Buffer.add_char buf ')'

and gen_some env buf indent arg =
  match infer_expr_type env arg with
  | Some inner when is_nullable_ty env inner ->
      env.shared.needs_option_struct <- true;
      Printf.bprintf buf "rgo_some[%s](" (go_type env inner);
      gen_expr env buf indent CtxExpr arg;
      Buffer.add_char buf ')'
  | inferred -> gen_new_expr env buf indent inferred arg

and gen_some_for_type env buf indent (inner_ty : Ast.ty) arg =
  if is_nullable_ty env inner_ty then begin
    env.shared.needs_option_struct <- true;
    Printf.bprintf buf "rgo_some[%s](" (go_type env inner_ty);
    gen_expr env buf indent CtxExpr arg;
    Buffer.add_char buf ')'
  end
  else gen_new_expr env buf indent (Some inner_ty) arg

(* ---------- stdlib codegen helpers ---------- *)

and gen_stdlib_call env buf indent type_name fn_name args =
  env.shared.needs_imported_pkgs <- true;
  let go_pkg =
    match SMap.find_opt type_name.node env.imported_packages with
    | Some (_, go_pkg_name) -> go_pkg_name
    | None -> type_name.node
  in
  let go_name =
    match Interop.go_member_name type_name.node fn_name.node with
    | Some name -> name
    | None -> snake_to_pascal fn_name.node
  in
  (* Special-case listen_and_serve so port-in-use failures surface via panic
     rather than silently exiting with code 0. *)
  if type_name.node = "http" && fn_name.node = "listen_and_serve" then (
    Printf.bprintf buf "if err := %s.%s(" go_pkg go_name;
    gen_args env buf indent args;
    Printf.bprintf buf "); err != nil {\n%s\tpanic(err)\n%s}" indent indent)
  else (
    Printf.bprintf buf "%s.%s(" go_pkg go_name;
    gen_args env buf indent args;
    Buffer.add_char buf ')')

and gen_path_call env buf indent type_name fn_name args =
  (* Check if it's an imported stdlib package call *)
  if is_imported_package type_name.node env then
    gen_stdlib_call env buf indent type_name fn_name args
  else
    (* Check if it's an enum variant constructor *)
    match SMap.find_opt type_name.node env.enums with
    | Some ei -> (
        match List.assoc_opt fn_name.node ei.ei_variants with
        | Some (VTuple field_tys) ->
            let type_suffix =
              match ei.ei_tparams with
              | [] -> ""
              | tparams ->
                  (* Build tparam -> Go type mapping from field types + args *)
                  let bindings = Hashtbl.create 4 in
                  List.iter2
                    (fun ft arg ->
                      match infer_expr_type env arg with
                      | Some t -> unify_ast_tparams tparams bindings ft t
                      | None -> ())
                    field_tys args;
                  (* Convert AST type bindings to Go type strings *)
                  let go_bindings = Hashtbl.create 4 in
                  Hashtbl.iter
                    (fun k v -> Hashtbl.replace go_bindings k (go_type env v))
                    bindings;
                  let tparam_types =
                    List.map
                      (fun tp ->
                        match Hashtbl.find_opt go_bindings tp with
                        | Some t -> t
                        | None ->
                            (* If tp is an in-scope type parameter, use it
                               directly instead of erasing to any *)
                            if List.mem tp env.type_params then tp else "any")
                      tparams
                  in
                  "[" ^ String.concat ", " tparam_types ^ "]"
            in
            Buffer.add_string buf
              (type_name.node ^ fn_name.node ^ type_suffix ^ "{");
            List.iteri
              (fun i a ->
                if i > 0 then Buffer.add_string buf ", ";
                Printf.bprintf buf "Field%d: " i;
                gen_expr env buf indent CtxExpr a)
              args;
            Buffer.add_char buf '}'
        | Some VUnit ->
            let type_suffix =
              match ei.ei_tparams with
              | [] -> ""
              | tparams ->
                  let tparam_types =
                    List.map
                      (fun tp ->
                        if List.mem tp env.type_params then tp else "any")
                      tparams
                  in
                  "[" ^ String.concat ", " tparam_types ^ "]"
            in
            Buffer.add_string buf
              (type_name.node ^ fn_name.node ^ type_suffix ^ "{}")
        | Some (VStruct _) ->
            (* Should not happen: struct variants are constructed differently *)
            Buffer.add_string buf (type_name.node ^ fn_name.node ^ "{}")
        | None ->
            (* Check impl assoc functions *)
            gen_assoc_fn_call env buf indent type_name fn_name args)
    | None ->
        if not (gen_builtin_path_call buf type_name fn_name) then
          gen_assoc_fn_call env buf indent type_name fn_name args

and gen_builtin_path_call buf type_name fn_name =
  match (type_name.node, fn_name.node) with
  | ("HashMap" | "Vec"), "new" ->
      (* Handled specially in gen_stmt for let bindings with type annotations.
         If we reach here (e.g., expression context without let), emit nil. *)
      Buffer.add_string buf "nil";
      true
  | _ -> false

and gen_assoc_fn_call env buf indent type_name fn_name args =
  let is_pub = fn_name_is_pub fn_name.node env type_name.node in
  let go_name = type_name.node ^ go_method_name is_pub fn_name.node in
  Buffer.add_string buf go_name;
  Buffer.add_char buf '(';
  gen_args env buf indent args;
  Buffer.add_char buf ')'

and fn_name_is_pub name env type_name =
  match SMap.find_opt type_name env.impls with
  | Some ii -> (
      match SMap.find_opt name ii.ii_assoc_fns with
      | Some _ -> true (* we don't store pub info, check the original AST *)
      | None -> true)
  | None -> true

and field_is_pub env recv field_name =
  let sname =
    match infer_expr_type env recv with
    | Some (TyName { node = n; _ }) -> Some n
    | Some (TyGeneric ({ node = n; _ }, _)) -> Some n
    | Some TySelf -> env.self_type_name
    | _ -> None
  in
  match sname with
  | Some sn -> (
      match SMap.find_opt sn env.structs with
      | Some si -> (
          match
            List.find_opt
              (fun (f : Ast.field) -> f.fd_name.node = field_name)
              si.si_fields
          with
          | Some f -> f.fd_pub
          | None -> true (* default to pub *))
      | None -> true)
  | None -> true (* default to pub for unknown types *)

and gen_method_call env buf indent recv method_name args =
  (* Handle built-in methods on containers *)
  let recv_ty = infer_expr_type env recv in
  match recv_ty with
  | Some (TyGeneric ({ node = "Vec"; _ }, [ _elem_ty ])) ->
      gen_vec_method env buf indent recv method_name args
  | Some (TyGeneric ({ node = "HashMap"; _ }, [ _k; v ])) ->
      gen_hashmap_method env buf indent recv method_name args v
  | Some (TyName { node = "str" | "String"; _ }) ->
      gen_string_method env buf indent recv method_name
  | Some (TyPath (pkg_alias, _type_name))
    when is_imported_package pkg_alias.node env ->
      gen_stdlib_method_call env buf indent recv method_name args
  | _ -> gen_user_method_call env buf indent recv method_name args

and gen_vec_method env buf indent recv method_name args =
  match method_name.node with
  | "len" ->
      Buffer.add_string buf "int64(len(";
      gen_expr env buf indent CtxExpr recv;
      Buffer.add_string buf "))"
  | "push" -> (
      match args with
      | [ arg ] ->
          (* v = append(v, x) -- requires receiver to be an lvalue *)
          gen_expr env buf indent CtxExpr recv;
          Buffer.add_string buf " = append(";
          gen_expr env buf indent CtxExpr recv;
          Buffer.add_string buf ", ";
          gen_expr env buf indent CtxExpr arg;
          Buffer.add_char buf ')'
      | _ -> failwith "codegen: push expects 1 argument")
  | "pop" ->
      (* Inline pop: func() *T { if len(v) == 0 { return nil }; x := v[len(v)-1]; v = v[:len(v)-1]; return new(x) }() *)
      (* This is complex; for now, emit a helper call pattern *)
      Buffer.add_string buf "func() ";
      let elem_ty =
        match infer_expr_type env recv with
        | Some (TyGeneric ({ node = "Vec"; _ }, [ inner ])) -> inner
        | _ -> TyName (dummy_loc "any")
      in
      let gt = go_type env elem_ty in
      if is_nullable_ty env elem_ty then begin
        env.shared.needs_option_struct <- true;
        Buffer.add_string buf ("Option[" ^ gt ^ "]");
        Buffer.add_string buf " {\n";
        let ni = indent ^ "\t" in
        Printf.bprintf buf "%sif len(" ni;
        gen_expr env buf indent CtxExpr recv;
        Buffer.add_string buf ") == 0 {\n";
        Printf.bprintf buf "%s\treturn rgo_none[%s]()\n" ni gt;
        Printf.bprintf buf "%s}\n" ni;
        Printf.bprintf buf "%sx := " ni;
        gen_expr env buf indent CtxExpr recv;
        Buffer.add_string buf "[len(";
        gen_expr env buf indent CtxExpr recv;
        Buffer.add_string buf ")-1]\n";
        Printf.bprintf buf "%s" ni;
        gen_expr env buf indent CtxExpr recv;
        Buffer.add_string buf " = ";
        gen_expr env buf indent CtxExpr recv;
        Buffer.add_string buf "[:len(";
        gen_expr env buf indent CtxExpr recv;
        Buffer.add_string buf ")-1]\n";
        Printf.bprintf buf "%sreturn rgo_some(x)\n" ni;
        Printf.bprintf buf "%s}()" indent
      end
      else begin
        Buffer.add_string buf ("*" ^ gt);
        Buffer.add_string buf " {\n";
        let ni = indent ^ "\t" in
        Printf.bprintf buf "%sif len(" ni;
        gen_expr env buf indent CtxExpr recv;
        Buffer.add_string buf ") == 0 {\n";
        Printf.bprintf buf "%s\treturn nil\n" ni;
        Printf.bprintf buf "%s}\n" ni;
        Printf.bprintf buf "%sx := " ni;
        gen_expr env buf indent CtxExpr recv;
        Buffer.add_string buf "[len(";
        gen_expr env buf indent CtxExpr recv;
        Buffer.add_string buf ")-1]\n";
        Printf.bprintf buf "%s" ni;
        gen_expr env buf indent CtxExpr recv;
        Buffer.add_string buf " = ";
        gen_expr env buf indent CtxExpr recv;
        Buffer.add_string buf "[:len(";
        gen_expr env buf indent CtxExpr recv;
        Buffer.add_string buf ")-1]\n";
        Printf.bprintf buf "%sreturn new(x)\n" ni;
        Printf.bprintf buf "%s}()" indent
      end
  | _ -> gen_user_method_call env buf indent recv method_name args

and gen_hashmap_method env buf indent recv method_name args v =
  match method_name.node with
  | "len" ->
      Buffer.add_string buf "int64(len(";
      gen_expr env buf indent CtxExpr recv;
      Buffer.add_string buf "))"
  | "insert" -> (
      match args with
      | [ k; v_arg ] ->
          gen_expr env buf indent CtxExpr recv;
          Buffer.add_char buf '[';
          gen_expr env buf indent CtxExpr k;
          Buffer.add_string buf "] = ";
          gen_expr env buf indent CtxExpr v_arg
      | _ -> failwith "codegen: insert expects 2 arguments")
  | "get" -> (
      match args with
      | [ k ] ->
          let gt = go_type env v in
          if is_nullable_ty env v then begin
            env.shared.needs_option_struct <- true;
            Printf.bprintf buf "func() Option[%s] {\n" gt;
            let ni = indent ^ "\t" in
            Printf.bprintf buf "%sv, ok := " ni;
            gen_expr env buf indent CtxExpr recv;
            Buffer.add_char buf '[';
            gen_expr env buf indent CtxExpr k;
            Buffer.add_string buf "]\n";
            Printf.bprintf buf "%sif !ok {\n" ni;
            Printf.bprintf buf "%s\treturn rgo_none[%s]()\n" ni gt;
            Printf.bprintf buf "%s}\n" ni;
            Printf.bprintf buf "%sreturn rgo_some(v)\n" ni;
            Printf.bprintf buf "%s}()" indent
          end
          else begin
            Printf.bprintf buf "func() *%s {\n" gt;
            let ni = indent ^ "\t" in
            Printf.bprintf buf "%sv, ok := " ni;
            gen_expr env buf indent CtxExpr recv;
            Buffer.add_char buf '[';
            gen_expr env buf indent CtxExpr k;
            Buffer.add_string buf "]\n";
            Printf.bprintf buf "%sif !ok {\n" ni;
            Printf.bprintf buf "%s\treturn nil\n" ni;
            Printf.bprintf buf "%s}\n" ni;
            Printf.bprintf buf "%sreturn &v\n" ni;
            Printf.bprintf buf "%s}()" indent
          end
      | _ -> failwith "codegen: get expects 1 argument")
  | "contains_key" -> (
      match args with
      | [ k ] ->
          Buffer.add_string buf "func() bool {\n";
          let ni = indent ^ "\t" in
          Printf.bprintf buf "%s_, ok := " ni;
          gen_expr env buf indent CtxExpr recv;
          Buffer.add_char buf '[';
          gen_expr env buf indent CtxExpr k;
          Buffer.add_string buf "]\n";
          Printf.bprintf buf "%sreturn ok\n" ni;
          Printf.bprintf buf "%s}()" indent
      | _ -> failwith "codegen: contains_key expects 1 argument")
  | "remove" -> (
      match args with
      | [ k ] ->
          Buffer.add_string buf "delete(";
          gen_expr env buf indent CtxExpr recv;
          Buffer.add_string buf ", ";
          gen_expr env buf indent CtxExpr k;
          Buffer.add_char buf ')'
      | _ -> failwith "codegen: remove expects 1 argument")
  | _ -> gen_user_method_call env buf indent recv method_name args

and gen_string_method env buf indent recv method_name =
  match method_name.node with
  | "len" ->
      Buffer.add_string buf "int64(len(";
      gen_expr env buf indent CtxExpr recv;
      Buffer.add_string buf "))"
  | _ -> failwith ("codegen: unsupported string method " ^ method_name.node)

and gen_stdlib_method_call env buf indent recv method_name args =
  (* Stdlib receiver methods: resolve Go name via Interop registry *)
  env.shared.needs_imported_pkgs <- true;
  let recv_pkg_type =
    match infer_expr_type env recv with
    | Some (TyPath (pkg, tname)) -> Some (pkg.node, tname.node)
    | _ -> None
  in
  let go_method =
    match recv_pkg_type with
    | Some (pkg, tname) -> (
        match Interop.go_receiver_method_name pkg tname method_name.node with
        | Some name -> name
        | None -> snake_to_pascal method_name.node)
    | None -> snake_to_pascal method_name.node
  in
  gen_expr env buf indent CtxExpr recv;
  Buffer.add_char buf '.';
  Buffer.add_string buf go_method;
  Buffer.add_char buf '(';
  (* ResponseWriter.Write takes []byte; wrap string args *)
  (match (go_method, args) with
  | "Write", [ arg ] ->
      Buffer.add_string buf "[]byte(";
      gen_expr env buf indent CtxExpr arg;
      Buffer.add_char buf ')'
  | _ -> gen_args env buf indent args);
  Buffer.add_char buf ')'

and gen_user_method_call env buf indent recv method_name args =
  (* Look up method pub status from impl info *)
  let recv_ty = infer_expr_type env recv in
  let type_name =
    match recv_ty with
    | Some (TyName { node = n; _ }) -> Some n
    | Some (TyGeneric ({ node = n; _ }, _)) -> Some n
    | _ -> None
  in
  let is_pub =
    match type_name with
    | Some tn -> (
        match SMap.find_opt tn env.impls with
        | Some ii -> (
            match SMap.find_opt method_name.node ii.ii_methods with
            | Some _ -> true (* default to pub for now *)
            | None -> true)
        | None -> true)
    | None -> true
  in
  (* Check if the receiver is non-addressable and the method needs pointer receiver *)
  let needs_temp =
    match type_name with
    | Some tn -> (
        match SMap.find_opt tn env.impls with
        | Some ii -> (
            match SMap.find_opt method_name.node ii.ii_methods with
            | Some mi -> (
                match mi.mi_self with
                | Some (SelfRef | SelfMutRef) -> expr_is_non_addressable recv
                | _ -> false)
            | None -> false)
        | None -> false)
    | None -> false
  in
  if needs_temp then begin
    let tmp = fresh_tmp env "recv" in
    (* Look up method return type to decide wrapping strategy *)
    let method_ret =
      match type_name with
      | Some tn -> (
          match SMap.find_opt tn env.impls with
          | Some ii -> (
              match SMap.find_opt method_name.node ii.ii_methods with
              | Some mi -> (
                  match mi.mi_ret with
                  | Some TySelf -> Some (Ast.TyName (dummy_loc tn))
                  | other -> other)
              | None -> None)
          | None -> None)
      | None -> None
    in
    (match method_ret with
    | Some ret_ty ->
        let gt = go_type env ret_ty in
        Printf.bprintf buf "func() %s { %s := " gt tmp
    | None -> Printf.bprintf buf "func() { %s := " tmp);
    gen_expr env buf indent CtxExpr recv;
    Buffer.add_string buf "; ";
    (match method_ret with
    | Some _ -> Buffer.add_string buf "return "
    | None -> ());
    Buffer.add_string buf tmp;
    Buffer.add_char buf '.';
    Buffer.add_string buf (go_method_name is_pub method_name.node);
    Buffer.add_char buf '(';
    gen_args env buf indent args;
    Buffer.add_string buf ") }()"
  end
  else begin
    gen_expr env buf indent CtxExpr recv;
    Buffer.add_char buf '.';
    Buffer.add_string buf (go_method_name is_pub method_name.node);
    Buffer.add_char buf '(';
    gen_args env buf indent args;
    Buffer.add_char buf ')'
  end

and expr_is_non_addressable (e : Ast.expr) : bool =
  match e with
  | ExprCall _ | ExprPath _ | ExprMethodCall _ | ExprStruct _
  | ExprStructVariant _ ->
      true
  | ExprLit _ -> true
  | _ -> false

and gen_struct_literal env buf indent ty fields =
  let type_name =
    match ty with
    | TyName n -> n.node
    | TyGeneric (n, _) -> n.node
    | _ -> "unknown"
  in
  let go_ty_str = go_type env ty in
  Buffer.add_string buf (go_ty_str ^ "{");
  let is_struct = SMap.mem type_name env.structs in
  List.iteri
    (fun i (sf : struct_field_init) ->
      if i > 0 then Buffer.add_string buf ", ";
      let fpub =
        if is_struct then
          match SMap.find_opt type_name env.structs with
          | Some si -> (
              match
                List.find_opt
                  (fun (f : Ast.field) -> f.fd_name.node = sf.sf_name.node)
                  si.si_fields
              with
              | Some f -> f.fd_pub
              | None -> true)
          | None -> true
        else true
      in
      Buffer.add_string buf (go_field_name fpub sf.sf_name.node);
      Buffer.add_string buf ": ";
      gen_expr env buf indent CtxExpr sf.sf_expr)
    fields;
  Buffer.add_char buf '}'

and gen_struct_variant_literal env buf indent type_name variant_name fields =
  let go_name = type_name.node ^ variant_name.node in
  Buffer.add_string buf (go_name ^ "{");
  List.iteri
    (fun i (sf : struct_field_init) ->
      if i > 0 then Buffer.add_string buf ", ";
      Buffer.add_string buf (go_field_name true sf.sf_name.node);
      Buffer.add_string buf ": ";
      gen_expr env buf indent CtxExpr sf.sf_expr)
    fields;
  Buffer.add_char buf '}'

and gen_if env buf indent ctx cond then_blk else_blk =
  match ctx with
  | CtxStmt -> (
      Buffer.add_string buf "if ";
      gen_expr env buf indent CtxExpr cond;
      Buffer.add_string buf " {\n";
      gen_block_stmts env buf (indent ^ "\t") then_blk;
      Buffer.add_string buf indent;
      Buffer.add_char buf '}';
      match else_blk with
      | Some eb ->
          Buffer.add_string buf " else {\n";
          gen_block_stmts env buf (indent ^ "\t") eb;
          Buffer.add_string buf indent;
          Buffer.add_char buf '}'
      | None -> ())
  | CtxExpr ->
      (* IIFE for if-expression *)
      let ret_ty = infer_block_type_with_local_fallback env then_blk in
      let go_sig, inner_env = result_iife_sig_and_env env ret_ty in
      Printf.bprintf buf "func() %s {\n" go_sig;
      let ni = indent ^ "\t" in
      Printf.bprintf buf "%sif " ni;
      gen_expr env buf ni CtxExpr cond;
      Buffer.add_string buf " {\n";
      gen_block_with_return inner_env buf (ni ^ "\t") then_blk;
      Printf.bprintf buf "%s}" ni;
      (match else_blk with
      | Some eb ->
          Buffer.add_string buf " else {\n";
          gen_block_with_return inner_env buf (ni ^ "\t") eb;
          Printf.bprintf buf "%s}\n" ni
      | None -> Buffer.add_char buf '\n');
      Printf.bprintf buf "%s}()" indent

and has_wildcard_or_bind_arm (arms : match_arm list) : bool =
  List.exists
    (fun (arm : match_arm) ->
      match arm.arm_pat with PatWild | PatBind _ -> true | _ -> false)
    arms

and gen_match env buf indent ctx scrutinee arms =
  match infer_expr_type env scrutinee with
  | Some (TyGeneric ({ node = "Result"; _ }, [ ok_ty; err_ty ])) ->
      gen_result_match env buf indent ctx scrutinee arms ok_ty err_ty
  | Some (TyGeneric ({ node = "Option"; _ }, [ inner_ty ])) ->
      gen_option_match env buf indent ctx scrutinee arms inner_ty
  | _ -> (
      let enum_name, enum_targs =
        match infer_expr_type env scrutinee with
        | Some (TyName { node = n; _ }) -> (Some n, [])
        | Some (TyGeneric ({ node = n; _ }, args)) -> (Some n, args)
        | _ -> (None, [])
      in
      (* Suffix for variant case types in generic enum type switches *)
      let targs_suffix =
        match enum_targs with
        | [] -> ""
        | _ ->
            "[" ^ String.concat ", " (List.map (go_type env) enum_targs) ^ "]"
      in
      match ctx with
      | CtxStmt ->
          Buffer.add_string buf "switch __v := ";
          gen_expr env buf indent CtxExpr scrutinee;
          Buffer.add_string buf ".(type) {\n";
          List.iter
            (fun (arm : match_arm) ->
              gen_match_arm env buf indent enum_name ~targs_suffix arm)
            arms;
          if not (has_wildcard_or_bind_arm arms) then begin
            Printf.bprintf buf "%sdefault:\n" indent;
            Printf.bprintf buf
              "%s\tpanic(\"unreachable: non-exhaustive match\")\n" indent
          end;
          Printf.bprintf buf "%s}" indent
      | CtxExpr ->
          let ret_ty =
            match arms with
            | arm :: _ -> infer_arm_expr_type env scrutinee arm
            | [] -> None
          in
          let go_sig, inner_env = result_iife_sig_and_env env ret_ty in
          Printf.bprintf buf "func() %s {\n" go_sig;
          let ni = indent ^ "\t" in
          Printf.bprintf buf "%sswitch __v := " ni;
          gen_expr env buf ni CtxExpr scrutinee;
          Buffer.add_string buf ".(type) {\n";
          List.iter
            (fun (arm : match_arm) ->
              gen_match_arm_with_return inner_env buf ni enum_name ~targs_suffix
                arm)
            arms;
          if not (has_wildcard_or_bind_arm arms) then begin
            Printf.bprintf buf "%sdefault:\n" ni;
            Printf.bprintf buf
              "%s\tpanic(\"unreachable: non-exhaustive match\")\n" ni
          end;
          Printf.bprintf buf "%s}\n" ni;
          Printf.bprintf buf "%s}()" indent)

and gen_match_arm env buf indent enum_name ?(targs_suffix = "")
    (arm : match_arm) =
  let ni = indent ^ "\t" in
  match arm.arm_pat with
  | PatWild ->
      Printf.bprintf buf "%sdefault:\n" indent;
      Printf.bprintf buf "%s_ = __v\n" ni;
      gen_arm_body_stmts env buf ni arm.arm_expr
  | PatBind name ->
      Printf.bprintf buf "%sdefault:\n" indent;
      Printf.bprintf buf "%s%s := __v\n" ni (escape_ident name.node);
      Printf.bprintf buf "%s_ = %s\n" ni (escape_ident name.node);
      gen_arm_body_stmts env buf ni arm.arm_expr
  | PatLit l ->
      Printf.bprintf buf "%scase " indent;
      gen_expr env buf indent CtxExpr (ExprLit l);
      Buffer.add_string buf ":\n";
      Printf.bprintf buf "%s_ = __v\n" ni;
      gen_arm_body_stmts env buf ni arm.arm_expr
  | PatTuple (ename, vname, pats) ->
      let case_type =
        (match enum_name with Some en -> en | None -> ename.node)
        ^ vname.node ^ targs_suffix
      in
      Printf.bprintf buf "%scase %s:\n" indent case_type;
      if pats = [] || List.for_all (fun p -> p = PatWild) pats then
        Printf.bprintf buf "%s_ = __v\n" ni
      else gen_tuple_bindings env buf ni pats;
      gen_arm_body_stmts env buf ni arm.arm_expr
  | PatStruct (ename, vname, field_pats) ->
      let case_type =
        (match enum_name with Some en -> en | None -> ename.node)
        ^ vname.node ^ targs_suffix
      in
      Printf.bprintf buf "%scase %s:\n" indent case_type;
      if field_pats = [] then Printf.bprintf buf "%s_ = __v\n" ni
      else gen_struct_bindings env buf ni field_pats;
      gen_arm_body_stmts env buf ni arm.arm_expr

and gen_match_arm_with_return env buf indent enum_name ?(targs_suffix = "")
    (arm : match_arm) =
  let ni = indent ^ "\t" in
  match arm.arm_pat with
  | PatWild ->
      Printf.bprintf buf "%sdefault:\n" indent;
      Printf.bprintf buf "%s_ = __v\n" ni;
      gen_return_expr env buf ni arm.arm_expr
  | PatBind name ->
      Printf.bprintf buf "%sdefault:\n" indent;
      Printf.bprintf buf "%s%s := __v\n" ni (escape_ident name.node);
      Printf.bprintf buf "%s_ = %s\n" ni (escape_ident name.node);
      gen_return_expr env buf ni arm.arm_expr
  | PatLit l ->
      Printf.bprintf buf "%scase " indent;
      gen_expr env buf indent CtxExpr (ExprLit l);
      Buffer.add_string buf ":\n";
      Printf.bprintf buf "%s_ = __v\n" ni;
      gen_return_expr env buf ni arm.arm_expr
  | PatTuple (ename, vname, pats) ->
      let case_type =
        (match enum_name with Some en -> en | None -> ename.node)
        ^ vname.node ^ targs_suffix
      in
      Printf.bprintf buf "%scase %s:\n" indent case_type;
      if pats = [] || List.for_all (fun p -> p = PatWild) pats then
        Printf.bprintf buf "%s_ = __v\n" ni
      else gen_tuple_bindings env buf ni pats;
      gen_return_expr env buf ni arm.arm_expr
  | PatStruct (ename, vname, field_pats) ->
      let case_type =
        (match enum_name with Some en -> en | None -> ename.node)
        ^ vname.node ^ targs_suffix
      in
      Printf.bprintf buf "%scase %s:\n" indent case_type;
      if field_pats = [] then Printf.bprintf buf "%s_ = __v\n" ni
      else gen_struct_bindings env buf ni field_pats;
      gen_return_expr env buf ni arm.arm_expr

and gen_tuple_bindings env buf indent pats =
  List.iteri
    (fun i p ->
      match p with
      | PatWild -> ()
      | PatBind name ->
          Printf.bprintf buf "%s%s := __v.Field%d\n" indent
            (escape_ident name.node) i
      | _ ->
          let tmp = fresh_tmp env "p" in
          Printf.bprintf buf "%s%s := __v.Field%d\n" indent tmp i;
          ignore tmp)
    pats;
  ignore env

and gen_struct_bindings _env buf indent field_pats =
  List.iter
    (fun (fp : field_pat) ->
      match fp.fp_pat with
      | None ->
          (* Shorthand: just bind field name *)
          Printf.bprintf buf "%s%s := __v.%s\n" indent
            (escape_ident fp.fp_name.node)
            (capitalize fp.fp_name.node)
      | Some PatWild -> ()
      | Some (PatBind name) ->
          Printf.bprintf buf "%s%s := __v.%s\n" indent (escape_ident name.node)
            (capitalize fp.fp_name.node)
      | Some _ -> ())
    field_pats

and gen_arm_body_stmts env buf indent expr =
  match expr with
  | ExprBlock blk -> gen_block_stmts env buf indent blk
  | _ ->
      Buffer.add_string buf indent;
      gen_expr env buf indent CtxStmt expr;
      Buffer.add_char buf '\n'

(* Check whether a final expression will emit a Go `return` statement,
   i.e. whether it counts as an early exit for scope-cleanup purposes. *)
and is_return_like_expr (e : Ast.expr) : bool =
  match e with
  | ExprReturn _ | ExprBreak | ExprContinue -> true
  | ExprCall (ExprIdent { node = "Ok" | "Err" | "Some"; _ }, _) -> true
  | ExprIdent { node = "None"; _ } -> true
  | _ -> false

and gen_final_expr_as_return env buf indent e =
  match e with
  | ExprReturn _ ->
      if contains_question e then begin
        let e' = hoist_question_exprs env buf indent e in
        gen_expr env buf indent CtxStmt e';
        Buffer.add_char buf '\n'
      end
      else begin
        gen_expr env buf indent CtxStmt e;
        Buffer.add_char buf '\n'
      end
  | ExprIf (cond, then_blk, else_blk) -> (
      Buffer.add_string buf indent;
      Buffer.add_string buf "if ";
      gen_expr env buf indent CtxExpr cond;
      Buffer.add_string buf " {\n";
      gen_block_with_return env buf (indent ^ "\t") then_blk;
      Buffer.add_string buf indent;
      Buffer.add_char buf '}';
      match else_blk with
      | Some eb ->
          Buffer.add_string buf " else {\n";
          gen_block_with_return env buf (indent ^ "\t") eb;
          Buffer.add_string buf indent;
          Buffer.add_string buf "}\n"
      | None -> Buffer.add_char buf '\n')
  | ExprMatch (scrutinee, arms) ->
      gen_match_as_return env buf indent scrutinee arms
  | ExprBlock blk -> gen_block_with_return env buf indent blk
  | ExprCall (ExprIdent { node = "Ok"; _ }, [ arg ]) -> (
      (* Ownership: if the Ok argument is a Drop-type binding, suppress its
         guard BEFORE emit_return_cleanup so it is not cleaned up by
         emit_all_nested_cleanup and ownership transfers to the caller. *)
      (match arg with
      | ExprIdent { node = n; _ } -> (
          match SMap.find_opt n env.drop_guards with
          | Some guard -> Printf.bprintf buf "%s%s = false\n" indent guard
          | None -> ())
      | _ -> ());
      let _cleaned = emit_return_cleanup env buf indent arg in
      Buffer.add_string buf indent;
      match env.ret_ty with
      | Some (TyGeneric ({ node = "Result"; _ }, _))
        when should_flatten_result_return env ->
          Buffer.add_string buf "return ";
          gen_expr env buf indent CtxExpr arg;
          Buffer.add_string buf ", nil\n"
      | _ ->
          Buffer.add_string buf "return ";
          gen_expr env buf indent CtxExpr e;
          Buffer.add_char buf '\n')
  | ExprCall (ExprIdent { node = "Err"; _ }, [ arg ]) -> (
      (* Ownership: if the Err argument is a Drop-type binding, suppress its
         guard BEFORE emit_return_cleanup so ownership transfers to the caller. *)
      (match arg with
      | ExprIdent { node = n; _ } -> (
          match SMap.find_opt n env.drop_guards with
          | Some guard -> Printf.bprintf buf "%s%s = false\n" indent guard
          | None -> ())
      | _ -> ());
      let _cleaned = emit_return_cleanup env buf indent arg in
      Buffer.add_string buf indent;
      match env.ret_ty with
      | Some (TyGeneric ({ node = "Result"; _ }, [ ok_ty; _ ]))
        when should_flatten_result_return env ->
          env.shared.needs_errors <- true;
          Printf.bprintf buf "return %s, " (go_zero_value env ok_ty);
          (match arg with
          | ExprLit (LitString _) ->
              Buffer.add_string buf "errors.New(";
              gen_expr env buf indent CtxExpr arg;
              Buffer.add_char buf ')'
          | _ ->
              Buffer.add_string buf "errors.New(";
              gen_expr env buf indent CtxExpr arg;
              Buffer.add_char buf ')');
          Buffer.add_char buf '\n'
      | _ ->
          Buffer.add_string buf "return ";
          gen_expr env buf indent CtxExpr e;
          Buffer.add_char buf '\n')
  | ExprCall (ExprIdent { node = "Some"; _ }, [ arg ]) -> (
      (* Ownership: if the Some argument is a Drop-type binding, suppress its
         guard BEFORE emit_return_cleanup so ownership transfers to the caller. *)
      (match arg with
      | ExprIdent { node = n; _ } -> (
          match SMap.find_opt n env.drop_guards with
          | Some guard -> Printf.bprintf buf "%s%s = false\n" indent guard
          | None -> ())
      | _ -> ());
      let _cleaned = emit_return_cleanup env buf indent arg in
      Buffer.add_string buf indent;
      match env.ret_ty with
      | Some (TyGeneric ({ node = "Option"; _ }, [ inner ])) ->
          Buffer.add_string buf "return ";
          gen_some_for_type env buf indent inner arg;
          Buffer.add_char buf '\n'
      | _ ->
          Buffer.add_string buf "return ";
          gen_some env buf indent arg;
          Buffer.add_char buf '\n')
  | ExprIdent { node = "None"; _ } ->
      let cleaned = has_nested_drops env in
      if cleaned then begin
        Buffer.add_char buf '\n';
        emit_all_nested_cleanup env buf indent
      end;
      Buffer.add_string buf indent;
      gen_return_none env buf;
      Buffer.add_char buf '\n'
  | ExprIdent { node = name; _ } -> (
      (* Ownership: suppress the returned binding's guard BEFORE nested cleanup
         so the returned value is not cleaned up by emit_all_nested_cleanup,
         then clean up remaining nested scope drops. *)
      (match SMap.find_opt name env.drop_guards with
      | Some guard -> Printf.bprintf buf "%s%s = false\n" indent guard
      | None -> ());
      let cleaned = has_nested_drops env in
      if cleaned then begin
        Buffer.add_char buf '\n';
        emit_all_nested_cleanup env buf indent
      end;
      match env.ret_ty with
      | None | Some (TyName { node = "void"; _ }) ->
          Buffer.add_string buf indent;
          gen_expr env buf indent CtxStmt e;
          Buffer.add_char buf '\n'
      | _ ->
          Printf.bprintf buf "%sreturn " indent;
          gen_expr env buf indent CtxExpr e;
          Buffer.add_char buf '\n')
  | _ ->
      if
        (* Ownership: suppress consumed guards and clean up nested Drop scopes
         before returning, routing through emit_return_cleanup for correct
         guard suppression / cleanup ordering. *)
        contains_question e
      then begin
        let e' = hoist_question_exprs env buf indent e in
        let _cleaned = emit_return_cleanup env buf indent e' in
        match env.ret_ty with
        | None | Some (TyName { node = "void"; _ }) ->
            Buffer.add_string buf indent;
            gen_expr env buf indent CtxStmt e';
            Buffer.add_char buf '\n'
        | _ ->
            Printf.bprintf buf "%sreturn " indent;
            gen_expr env buf indent CtxExpr e';
            Buffer.add_char buf '\n'
      end
      else begin
        let _cleaned = emit_return_cleanup env buf indent e in
        match env.ret_ty with
        | None | Some (TyName { node = "void"; _ }) ->
            Buffer.add_string buf indent;
            gen_expr env buf indent CtxStmt e;
            Buffer.add_char buf '\n'
        | _ ->
            Printf.bprintf buf "%sreturn " indent;
            gen_expr env buf indent CtxExpr e;
            Buffer.add_char buf '\n'
      end

and gen_match_as_return env buf indent scrutinee arms =
  match infer_expr_type env scrutinee with
  | Some (TyGeneric ({ node = "Result"; _ }, [ ok_ty; err_ty ])) ->
      gen_result_match_as_return env buf indent scrutinee arms ok_ty err_ty
  | Some (TyGeneric ({ node = "Option"; _ }, [ inner_ty ])) ->
      gen_option_match_as_return env buf indent scrutinee arms inner_ty
  | _ ->
      let enum_name, enum_targs =
        match infer_expr_type env scrutinee with
        | Some (TyName { node = n; _ }) -> (Some n, [])
        | Some (TyGeneric ({ node = n; _ }, args)) -> (Some n, args)
        | _ -> (None, [])
      in
      let targs_suffix =
        match enum_targs with
        | [] -> ""
        | _ ->
            "[" ^ String.concat ", " (List.map (go_type env) enum_targs) ^ "]"
      in
      Printf.bprintf buf "%sswitch __v := " indent;
      gen_expr env buf indent CtxExpr scrutinee;
      Buffer.add_string buf ".(type) {\n";
      List.iter
        (fun (arm : match_arm) ->
          gen_match_arm_with_return env buf indent enum_name ~targs_suffix arm)
        arms;
      if not (has_wildcard_or_bind_arm arms) then begin
        Printf.bprintf buf "%sdefault:\n" indent;
        Printf.bprintf buf "%s\tpanic(\"unreachable: non-exhaustive match\")\n"
          indent
      end;
      Printf.bprintf buf "%s}\n" indent

and find_builtin_variant_arm variant_name arms =
  List.find_opt
    (fun (arm : match_arm) ->
      match arm.arm_pat with
      | PatTuple (_, variant_name', _) -> variant_name'.node = variant_name
      | _ -> false)
    arms

and find_builtin_default_arm arms =
  List.find_opt
    (fun (arm : match_arm) ->
      match arm.arm_pat with PatWild | PatBind _ -> true | _ -> false)
    arms

and gen_result_pattern_binding buf indent value_name err_name is_ok
    (arm : match_arm) =
  match arm.arm_pat with
  | PatTuple (_, _, [ PatBind name ]) ->
      let source = if is_ok then value_name else err_name in
      Printf.bprintf buf "%s%s := %s\n" indent (escape_ident name.node) source
  | PatBind name ->
      let source = if is_ok then value_name else err_name in
      Printf.bprintf buf "%s%s := %s\n" indent (escape_ident name.node) source
  | _ -> ()

and gen_option_pattern_binding env buf indent opt_name (arm : match_arm)
    inner_ty =
  match arm.arm_pat with
  | PatTuple (_, _, [ PatBind name ]) ->
      if is_nullable_ty env inner_ty then
        Printf.bprintf buf "%s%s := %s.value\n" indent (escape_ident name.node)
          opt_name
      else
        Printf.bprintf buf "%s%s := *%s\n" indent (escape_ident name.node)
          opt_name
  | PatBind name ->
      Printf.bprintf buf "%s%s := %s\n" indent (escape_ident name.node) opt_name
  | _ -> ()

(* Return an env with the option-match binding added so nested matches
   can look up the unwrapped type *)
and env_with_option_binding env (arm : match_arm) inner_ty =
  let scope_env = push_scope env in
  match arm.arm_pat with
  | PatTuple (_, _, [ PatBind name ]) ->
      add_value name.node (Some inner_ty) ~is_mut:false scope_env
  | PatBind name -> add_value name.node (Some inner_ty) ~is_mut:false scope_env
  | _ -> scope_env

(* ---------- Nested built-in pattern helpers ---------- *)

(* Extract the inner type from a built-in Option/Result type *)
and inner_type_of_builtin (t : Ast.ty) : Ast.ty option =
  match t with
  | TyGeneric ({ node = "Option"; _ }, [ inner ]) -> Some inner
  | TyGeneric ({ node = "Result"; _ }, [ ok; _ ]) -> Some ok
  | _ -> None

(* Collect all arms with a given variant name at the top level *)
and find_all_builtin_variant_arms variant_name arms =
  List.filter
    (fun (arm : match_arm) ->
      match arm.arm_pat with
      | PatTuple (_, vname, _) -> vname.node = variant_name
      | _ -> false)
    arms

(* For an arm whose top-level pattern is Some/Ok/Err with a nested
   built-in pattern inside, extract the inner pattern.
   E.g. Option::Some(Option::Some(v)) -> Some(Option, Some, [PatBind v]) *)
and extract_inner_builtin_pat (arm : match_arm) :
    (string * string * pat list) option =
  match arm.arm_pat with
  | PatTuple (_, _, [ PatTuple (inner_enum, inner_variant, inner_pats) ])
    when inner_enum.node = "Option" || inner_enum.node = "Result" ->
      Some (inner_enum.node, inner_variant.node, inner_pats)
  | _ -> None

(* Peel one level of built-in nesting from each arm, returning arms whose
   pattern is the inner built-in variant.  Arms whose pattern is not a
   nested built-in (leaf bind/wildcard) are dropped. *)
and peel_one_builtin_level (arms : match_arm list) : match_arm list =
  List.filter_map
    (fun (arm : match_arm) ->
      match arm.arm_pat with
      | PatTuple (_, _, [ PatTuple (inner_enum, inner_variant, inner_pats) ])
        when inner_enum.node = "Option" || inner_enum.node = "Result" ->
          Some
            {
              arm with
              arm_pat = PatTuple (inner_enum, inner_variant, inner_pats);
            }
      | _ -> None)
    arms

(* Safe head option - returns the first element or None *)
and list_head_opt (l : 'a list) : 'a option =
  match l with [] -> None | x :: _ -> Some x

(* Generate a nested Option match for arms with nested Option patterns
   inside an outer Some arm.  This emits code like:

     inner := *__match_opt_0
     if inner.some / inner != nil {
       <Some arm body with inner bindings>
     } else {
       <None arm body>
     }

   some_arms are the outer Some arms whose inner patterns are
   built-in Option patterns.  We find the inner Some/None among them. *)
and gen_nested_option_match_stmt env buf indent outer_var inner_name
    (some_arms : match_arm list) inner_ty nested_inner_ty _ctx default_arm =
  (* Emit the outer binding: unwrap the outer Option value *)
  if is_nullable_ty env inner_ty then
    Printf.bprintf buf "%s%s := %s.value\n" indent inner_name outer_var
  else Printf.bprintf buf "%s%s := *%s\n" indent inner_name outer_var;
  let cond =
    if is_nullable_ty env nested_inner_ty then inner_name ^ ".some"
    else inner_name ^ " != nil"
  in
  (* Collect ALL inner Some arms and inner None arms at this level *)
  let all_inner_some_arms =
    List.filter
      (fun (arm : match_arm) ->
        match extract_inner_builtin_pat arm with
        | Some (_, "Some", _) -> true
        | _ -> false)
      some_arms
  in
  let inner_some_arm = list_head_opt all_inner_some_arms in
  let inner_none_arm =
    List.find_opt
      (fun (arm : match_arm) ->
        match extract_inner_builtin_pat arm with
        | Some (_, "None", _) -> true
        | _ -> false)
      some_arms
  in
  Printf.bprintf buf "%sif %s {\n" indent cond;
  (match inner_some_arm with
  | Some arm -> (
      match arm.arm_pat with
      | PatTuple (_, _, [ PatTuple (_, _, [ PatBind name ]) ]) ->
          let some_env = push_scope env in
          let some_env =
            add_value name.node (Some nested_inner_ty) ~is_mut:false some_env
          in
          if is_nullable_ty env nested_inner_ty then
            Printf.bprintf buf "%s%s := %s.value\n" (indent ^ "\t")
              (escape_ident name.node) inner_name
          else
            Printf.bprintf buf "%s%s := *%s\n" (indent ^ "\t")
              (escape_ident name.node) inner_name;
          gen_arm_body_stmts some_env buf (indent ^ "\t") arm.arm_expr
      | PatTuple (_, _, [ PatTuple (_, _, [ PatWild ]) ]) ->
          (* Wildcard inner: no binding needed, just emit body *)
          gen_arm_body_stmts env buf (indent ^ "\t") arm.arm_expr
      | PatTuple (_, _, [ PatTuple (deeper_enum, _, _deeper_inner_pats) ])
        when deeper_enum.node = "Option" || deeper_enum.node = "Result" -> (
          (* Deeper nesting: pass the current inner struct to the
             recursive function which will unwrap one more level.
             We pass inner_name (the struct) as outer_var, and the
             correct inner_ty so the function's own unwrap extracts
             the Some payload correctly. *)
          let peeled_arms = peel_one_builtin_level all_inner_some_arms in
          match nested_inner_ty with
          | TyGeneric ({ node = "Option"; _ }, [ deeper_inner_ty ]) ->
              gen_nested_option_match_stmt env buf (indent ^ "\t") inner_name
                (fresh_tmp env "inner_opt")
                peeled_arms nested_inner_ty deeper_inner_ty CtxStmt default_arm
          | TyGeneric ({ node = "Result"; _ }, _) ->
              gen_nested_result_inner_match_stmt env buf (indent ^ "\t")
                inner_name peeled_arms nested_inner_ty CtxStmt default_arm
          | _ ->
              Printf.bprintf buf "%spanic(\"unsupported nested match type\")\n"
                (indent ^ "\t"))
      | _ ->
          Printf.bprintf buf "%spanic(\"unsupported nested pattern\")\n"
            (indent ^ "\t"))
  | None -> (
      (* No specific inner Some arm - fall back to default arm *)
      match default_arm with
      | Some arm -> gen_arm_body_stmts env buf (indent ^ "\t") arm.arm_expr
      | None -> Printf.bprintf buf "%spanic(\"unreachable\")\n" (indent ^ "\t")));
  Printf.bprintf buf "%s} else {\n" indent;
  (match inner_none_arm with
  | Some arm -> gen_arm_body_stmts env buf (indent ^ "\t") arm.arm_expr
  | None -> (
      (* No specific inner None arm - fall back to default arm *)
      match default_arm with
      | Some arm -> gen_arm_body_stmts env buf (indent ^ "\t") arm.arm_expr
      | None -> Printf.bprintf buf "%spanic(\"unreachable\")\n" (indent ^ "\t")));
  Printf.bprintf buf "%s}\n" indent

(* Same as above but for expression context (returns a value) *)
and gen_nested_option_match_expr env buf indent outer_var inner_name
    (some_arms : match_arm list) inner_ty nested_inner_ty _ctx default_arm =
  if is_nullable_ty env inner_ty then
    Printf.bprintf buf "%s%s := %s.value\n" indent inner_name outer_var
  else Printf.bprintf buf "%s%s := *%s\n" indent inner_name outer_var;
  let cond =
    if is_nullable_ty env nested_inner_ty then inner_name ^ ".some"
    else inner_name ^ " != nil"
  in
  (* Collect ALL inner Some arms and inner None arms at this level *)
  let all_inner_some_arms =
    List.filter
      (fun (arm : match_arm) ->
        match extract_inner_builtin_pat arm with
        | Some (_, "Some", _) -> true
        | _ -> false)
      some_arms
  in
  let inner_some_arm = list_head_opt all_inner_some_arms in
  let inner_none_arm =
    List.find_opt
      (fun (arm : match_arm) ->
        match extract_inner_builtin_pat arm with
        | Some (_, "None", _) -> true
        | _ -> false)
      some_arms
  in
  Printf.bprintf buf "%sif %s {\n" indent cond;
  (match inner_some_arm with
  | Some arm -> (
      match arm.arm_pat with
      | PatTuple (_, _, [ PatTuple (_, _, [ PatBind name ]) ]) ->
          let some_env = push_scope env in
          let some_env =
            add_value name.node (Some nested_inner_ty) ~is_mut:false some_env
          in
          if is_nullable_ty env nested_inner_ty then
            Printf.bprintf buf "%s%s := %s.value\n" (indent ^ "\t")
              (escape_ident name.node) inner_name
          else
            Printf.bprintf buf "%s%s := *%s\n" (indent ^ "\t")
              (escape_ident name.node) inner_name;
          Printf.bprintf buf "%sreturn " (indent ^ "\t");
          gen_expr some_env buf (indent ^ "\t") CtxExpr arm.arm_expr;
          Buffer.add_char buf '\n'
      | PatTuple (_, _, [ PatTuple (_, _, [ PatWild ]) ]) ->
          (* Wildcard inner: no binding needed, just emit return body *)
          Printf.bprintf buf "%sreturn " (indent ^ "\t");
          gen_expr env buf (indent ^ "\t") CtxExpr arm.arm_expr;
          Buffer.add_char buf '\n'
      | PatTuple (_, _, [ PatTuple (deeper_enum, _, _deeper_inner_pats) ])
        when deeper_enum.node = "Option" || deeper_enum.node = "Result" -> (
          (* Deeper nesting: peel one level and recurse *)
          (* Deeper nesting: pass the current inner struct to the
             recursive function which will unwrap one more level *)
          let peeled_arms = peel_one_builtin_level all_inner_some_arms in
          match nested_inner_ty with
          | TyGeneric ({ node = "Option"; _ }, [ deeper_inner_ty ]) ->
              gen_nested_option_match_expr env buf (indent ^ "\t") inner_name
                (fresh_tmp env "inner_opt")
                peeled_arms nested_inner_ty deeper_inner_ty CtxExpr default_arm
          | TyGeneric ({ node = "Result"; _ }, _) ->
              gen_nested_result_inner_match_expr env buf (indent ^ "\t")
                inner_name peeled_arms nested_inner_ty CtxExpr default_arm
          | _ ->
              Printf.bprintf buf "%spanic(\"unsupported nested match type\")\n"
                (indent ^ "\t"))
      | _ ->
          Printf.bprintf buf "%spanic(\"unsupported nested pattern\")\n"
            (indent ^ "\t"))
  | None -> (
      (* No specific inner Some arm - fall back to default arm *)
      match default_arm with
      | Some arm ->
          Printf.bprintf buf "%sreturn " (indent ^ "\t");
          gen_expr env buf (indent ^ "\t") CtxExpr arm.arm_expr;
          Buffer.add_char buf '\n'
      | None -> Printf.bprintf buf "%spanic(\"unreachable\")\n" (indent ^ "\t")));
  Printf.bprintf buf "%s} else {\n" indent;
  (match inner_none_arm with
  | Some arm ->
      Printf.bprintf buf "%sreturn " (indent ^ "\t");
      gen_expr env buf (indent ^ "\t") CtxExpr arm.arm_expr;
      Buffer.add_char buf '\n'
  | None -> (
      (* No specific inner None arm - fall back to default arm *)
      match default_arm with
      | Some arm ->
          Printf.bprintf buf "%sreturn " (indent ^ "\t");
          gen_expr env buf (indent ^ "\t") CtxExpr arm.arm_expr;
          Buffer.add_char buf '\n'
      | None -> Printf.bprintf buf "%spanic(\"unreachable\")\n" (indent ^ "\t")));
  Printf.bprintf buf "%s}\n" indent

(* Generate a nested Result-then-Option/Result match for arms with nested
   Result patterns inside an outer Ok/Err arm.

   For Result::Ok(Option::Some(v)) => body:
   Emits:
     inner := value_name  (or err_name for Err)
     if inner != nil {
       v := *inner
       <body>
     } else {
       <inner None arm body>
     }

   For Result::Ok(Result::Ok(v)) => body:
   Emits:
     inner := value_name
     inner_val, inner_err := inner
     if inner_err == nil {
       v := inner_val
       <body>
     } else {
       <inner Err arm body>
     } *)
and gen_nested_result_inner_match_stmt env buf indent inner_var
    (inner_arms : match_arm list) inner_ty _ctx default_arm =
  (* Determine the inner type and generate the appropriate nested matching *)
  match inner_ty with
  | TyGeneric ({ node = "Option"; _ }, [ nested_inner_ty ]) ->
      (* Inner is Option: generate nested if/else on some/nil *)
      let cond =
        if is_nullable_ty env nested_inner_ty then inner_var ^ ".some"
        else inner_var ^ " != nil"
      in
      let all_inner_some_arms =
        List.filter
          (fun (arm : match_arm) ->
            match extract_inner_builtin_pat arm with
            | Some ("Option", "Some", _) -> true
            | _ -> false)
          inner_arms
      in
      let inner_some_arm = list_head_opt all_inner_some_arms in
      let inner_none_arm =
        List.find_opt
          (fun (arm : match_arm) ->
            match extract_inner_builtin_pat arm with
            | Some ("Option", "None", _) -> true
            | _ -> false)
          inner_arms
      in
      Printf.bprintf buf "%sif %s {\n" indent cond;
      (match inner_some_arm with
      | Some arm -> (
          match arm.arm_pat with
          | PatTuple (_, _, [ PatTuple (_, _, [ PatBind name ]) ]) ->
              let some_env = push_scope env in
              let some_env =
                add_value name.node (Some nested_inner_ty) ~is_mut:false
                  some_env
              in
              if is_nullable_ty env nested_inner_ty then
                Printf.bprintf buf "%s%s := %s.value\n" (indent ^ "\t")
                  (escape_ident name.node) inner_var
              else
                Printf.bprintf buf "%s%s := *%s\n" (indent ^ "\t")
                  (escape_ident name.node) inner_var;
              gen_arm_body_stmts some_env buf (indent ^ "\t") arm.arm_expr
          | PatTuple (_, _, [ PatTuple (_, _, [ PatWild ]) ]) ->
              (* Wildcard inner: no binding needed, just emit body *)
              gen_arm_body_stmts env buf (indent ^ "\t") arm.arm_expr
          | PatTuple (_, _, [ PatTuple (deeper_enum, _, _deeper_inner_pats) ])
            when deeper_enum.node = "Option" || deeper_enum.node = "Result" -> (
              (* Deeper nesting: pass inner_var to the recursive function
                 which will unwrap one more level itself *)
              let peeled_arms = peel_one_builtin_level all_inner_some_arms in
              match nested_inner_ty with
              | TyGeneric ({ node = "Option"; _ }, [ deeper_inner_ty ]) ->
                  gen_nested_option_match_stmt env buf (indent ^ "\t") inner_var
                    (fresh_tmp env "inner_opt")
                    peeled_arms nested_inner_ty deeper_inner_ty CtxStmt
                    default_arm
              | TyGeneric ({ node = "Result"; _ }, _) ->
                  gen_nested_result_inner_match_stmt env buf (indent ^ "\t")
                    inner_var peeled_arms nested_inner_ty CtxStmt default_arm
              | _ ->
                  Printf.bprintf buf
                    "%spanic(\"unsupported nested match type\")\n"
                    (indent ^ "\t"))
          | _ ->
              Printf.bprintf buf "%spanic(\"unsupported nested pattern\")\n"
                (indent ^ "\t"))
      | None -> (
          (* No specific inner Some arm - fall back to default arm *)
          match default_arm with
          | Some arm -> gen_arm_body_stmts env buf (indent ^ "\t") arm.arm_expr
          | None ->
              Printf.bprintf buf "%spanic(\"unreachable\")\n" (indent ^ "\t")));
      Printf.bprintf buf "%s} else {\n" indent;
      (match inner_none_arm with
      | Some arm -> gen_arm_body_stmts env buf (indent ^ "\t") arm.arm_expr
      | None -> (
          (* No specific inner None arm - fall back to default arm *)
          match default_arm with
          | Some arm -> gen_arm_body_stmts env buf (indent ^ "\t") arm.arm_expr
          | None ->
              Printf.bprintf buf "%spanic(\"unreachable\")\n" (indent ^ "\t")));
      Printf.bprintf buf "%s}" indent
  | TyGeneric ({ node = "Result"; _ }, [ ok_ty; _err_ty ]) ->
      (* Inner Result value: generate nested if/else on .ok field *)
      let nested_val = fresh_tmp env "inner_val" in
      Printf.bprintf buf "%sif %s.ok {\n" indent inner_var;
      Printf.bprintf buf "%s%s := %s.value\n" (indent ^ "\t") nested_val
        inner_var;
      let all_inner_ok_arms =
        List.filter
          (fun (arm : match_arm) ->
            match extract_inner_builtin_pat arm with
            | Some ("Result", "Ok", _) -> true
            | _ -> false)
          inner_arms
      in
      let inner_ok_arm = list_head_opt all_inner_ok_arms in
      let all_inner_err_arms =
        List.filter
          (fun (arm : match_arm) ->
            match extract_inner_builtin_pat arm with
            | Some ("Result", "Err", _) -> true
            | _ -> false)
          inner_arms
      in
      let inner_err_arm = list_head_opt all_inner_err_arms in
      (match inner_ok_arm with
      | Some arm -> (
          match arm.arm_pat with
          | PatTuple (_, _, [ PatTuple (_, _, [ PatBind name ]) ]) ->
              let ok_env = push_scope env in
              let ok_env =
                add_value name.node (Some ok_ty) ~is_mut:false ok_env
              in
              Printf.bprintf buf "%s%s := %s\n" (indent ^ "\t")
                (escape_ident name.node) nested_val;
              gen_arm_body_stmts ok_env buf (indent ^ "\t") arm.arm_expr
          | PatTuple (_, _, [ PatTuple (_, _, [ PatWild ]) ]) ->
              (* Wildcard inner: no binding needed, just emit body *)
              gen_arm_body_stmts env buf (indent ^ "\t") arm.arm_expr
          | PatTuple (_, _, [ PatTuple (deeper_enum, _, _deeper_inner_pats) ])
            when deeper_enum.node = "Option" || deeper_enum.node = "Result" ->
              (* Deeper nesting on Ok side: peel one level and recurse.
                 nested_val holds the Ok value; pass it to the recursive
                 function which handles the next level of matching. *)
              let peeled_arms = peel_one_builtin_level all_inner_ok_arms in
              gen_nested_result_inner_match_stmt env buf (indent ^ "\t")
                nested_val peeled_arms ok_ty CtxStmt default_arm
          | _ ->
              Printf.bprintf buf "%spanic(\"unsupported nested pattern\")\n"
                (indent ^ "\t"))
      | None -> (
          (* No specific inner Ok arm - fall back to default arm *)
          match default_arm with
          | Some arm -> gen_arm_body_stmts env buf (indent ^ "\t") arm.arm_expr
          | None ->
              Printf.bprintf buf "%spanic(\"unreachable\")\n" (indent ^ "\t")));
      Printf.bprintf buf "%s} else {\n" indent;
      let nested_err = fresh_tmp env "inner_err" in
      Printf.bprintf buf "%s%s := %s.err\n" (indent ^ "\t") nested_err inner_var;
      (match inner_err_arm with
      | Some arm -> (
          match arm.arm_pat with
          | PatTuple (_, _, [ PatTuple (_, _, [ PatBind name ]) ]) ->
              Printf.bprintf buf "%s%s := %s\n" (indent ^ "\t")
                (escape_ident name.node) nested_err;
              gen_arm_body_stmts env buf (indent ^ "\t") arm.arm_expr
          | PatTuple (_, _, [ PatTuple (_, _, [ PatWild ]) ]) ->
              (* Wildcard inner: no binding needed, just emit body *)
              gen_arm_body_stmts env buf (indent ^ "\t") arm.arm_expr
          | PatTuple (_, _, [ PatTuple (deeper_enum, _, _deeper_inner_pats) ])
            when deeper_enum.node = "Option" || deeper_enum.node = "Result" ->
              (* Deeper nesting on Err side: peel one level and recurse *)
              let peeled_arms = peel_one_builtin_level all_inner_err_arms in
              gen_nested_result_inner_match_stmt env buf (indent ^ "\t")
                nested_err peeled_arms _err_ty CtxStmt default_arm
          | _ ->
              Printf.bprintf buf "%spanic(\"unsupported nested pattern\")\n"
                (indent ^ "\t"))
      | None -> (
          (* No specific inner Err arm - fall back to default arm *)
          match default_arm with
          | Some arm -> gen_arm_body_stmts env buf (indent ^ "\t") arm.arm_expr
          | None ->
              Printf.bprintf buf "%spanic(\"unreachable\")\n" (indent ^ "\t")));
      Printf.bprintf buf "%s}\n" indent
  | _ ->
      Printf.bprintf buf "%spanic(\"unsupported nested match type\")\n" indent

(* Expression-context version of nested Result inner match *)
and gen_nested_result_inner_match_expr env buf indent inner_var
    (inner_arms : match_arm list) inner_ty _ctx default_arm =
  match inner_ty with
  | TyGeneric ({ node = "Option"; _ }, [ nested_inner_ty ]) ->
      let cond =
        if is_nullable_ty env nested_inner_ty then inner_var ^ ".some"
        else inner_var ^ " != nil"
      in
      let all_inner_some_arms =
        List.filter
          (fun (arm : match_arm) ->
            match extract_inner_builtin_pat arm with
            | Some ("Option", "Some", _) -> true
            | _ -> false)
          inner_arms
      in
      let inner_some_arm = list_head_opt all_inner_some_arms in
      let inner_none_arm =
        List.find_opt
          (fun (arm : match_arm) ->
            match extract_inner_builtin_pat arm with
            | Some ("Option", "None", _) -> true
            | _ -> false)
          inner_arms
      in
      Printf.bprintf buf "%sif %s {\n" indent cond;
      (match inner_some_arm with
      | Some arm -> (
          match arm.arm_pat with
          | PatTuple (_, _, [ PatTuple (_, _, [ PatBind name ]) ]) ->
              let some_env = push_scope env in
              let some_env =
                add_value name.node (Some nested_inner_ty) ~is_mut:false
                  some_env
              in
              if is_nullable_ty env nested_inner_ty then
                Printf.bprintf buf "%s%s := %s.value\n" (indent ^ "\t")
                  (escape_ident name.node) inner_var
              else
                Printf.bprintf buf "%s%s := *%s\n" (indent ^ "\t")
                  (escape_ident name.node) inner_var;
              Printf.bprintf buf "%sreturn " (indent ^ "\t");
              gen_expr some_env buf (indent ^ "\t") CtxExpr arm.arm_expr;
              Buffer.add_char buf '\n'
          | PatTuple (_, _, [ PatTuple (_, _, [ PatWild ]) ]) ->
              (* Wildcard inner: no binding needed, just emit return body *)
              Printf.bprintf buf "%sreturn " (indent ^ "\t");
              gen_expr env buf (indent ^ "\t") CtxExpr arm.arm_expr;
              Buffer.add_char buf '\n'
          | PatTuple (_, _, [ PatTuple (deeper_enum, _, _deeper_inner_pats) ])
            when deeper_enum.node = "Option" || deeper_enum.node = "Result" -> (
              (* Deeper nesting: pass inner_var to the recursive function *)
              let peeled_arms = peel_one_builtin_level all_inner_some_arms in
              match nested_inner_ty with
              | TyGeneric ({ node = "Option"; _ }, [ deeper_inner_ty ]) ->
                  gen_nested_option_match_expr env buf (indent ^ "\t") inner_var
                    (fresh_tmp env "inner_opt")
                    peeled_arms nested_inner_ty deeper_inner_ty CtxExpr
                    default_arm
              | TyGeneric ({ node = "Result"; _ }, _) ->
                  gen_nested_result_inner_match_expr env buf (indent ^ "\t")
                    inner_var peeled_arms nested_inner_ty CtxExpr default_arm
              | _ ->
                  Printf.bprintf buf
                    "%spanic(\"unsupported nested match type\")\n"
                    (indent ^ "\t"))
          | _ ->
              Printf.bprintf buf "%spanic(\"unsupported nested pattern\")\n"
                (indent ^ "\t"))
      | None -> (
          (* No specific inner Some arm - fall back to default arm *)
          match default_arm with
          | Some arm ->
              Printf.bprintf buf "%sreturn " (indent ^ "\t");
              gen_expr env buf (indent ^ "\t") CtxExpr arm.arm_expr;
              Buffer.add_char buf '\n'
          | None ->
              Printf.bprintf buf "%spanic(\"unreachable\")\n" (indent ^ "\t")));
      Printf.bprintf buf "%s} else {\n" indent;
      (match inner_none_arm with
      | Some arm ->
          Printf.bprintf buf "%sreturn " (indent ^ "\t");
          gen_expr env buf (indent ^ "\t") CtxExpr arm.arm_expr;
          Buffer.add_char buf '\n'
      | None -> (
          (* No specific inner None arm - fall back to default arm *)
          match default_arm with
          | Some arm ->
              Printf.bprintf buf "%sreturn " (indent ^ "\t");
              gen_expr env buf (indent ^ "\t") CtxExpr arm.arm_expr;
              Buffer.add_char buf '\n'
          | None ->
              Printf.bprintf buf "%spanic(\"unreachable\")\n" (indent ^ "\t")));
      Printf.bprintf buf "%s}" indent
  | TyGeneric ({ node = "Result"; _ }, [ ok_ty; _err_ty ]) ->
      (* Inner Result value: generate nested if/else on .ok field *)
      let nested_val = fresh_tmp env "inner_val" in
      Printf.bprintf buf "%sif %s.ok {\n" indent inner_var;
      Printf.bprintf buf "%s%s := %s.value\n" (indent ^ "\t") nested_val
        inner_var;
      let all_inner_ok_arms =
        List.filter
          (fun (arm : match_arm) ->
            match extract_inner_builtin_pat arm with
            | Some ("Result", "Ok", _) -> true
            | _ -> false)
          inner_arms
      in
      let inner_ok_arm = list_head_opt all_inner_ok_arms in
      let all_inner_err_arms =
        List.filter
          (fun (arm : match_arm) ->
            match extract_inner_builtin_pat arm with
            | Some ("Result", "Err", _) -> true
            | _ -> false)
          inner_arms
      in
      let inner_err_arm = list_head_opt all_inner_err_arms in
      (match inner_ok_arm with
      | Some arm -> (
          match arm.arm_pat with
          | PatTuple (_, _, [ PatTuple (_, _, [ PatBind name ]) ]) ->
              let ok_env = push_scope env in
              let ok_env =
                add_value name.node (Some ok_ty) ~is_mut:false ok_env
              in
              Printf.bprintf buf "%s%s := %s\n" (indent ^ "\t")
                (escape_ident name.node) nested_val;
              Printf.bprintf buf "%sreturn " (indent ^ "\t");
              gen_expr ok_env buf (indent ^ "\t") CtxExpr arm.arm_expr;
              Buffer.add_char buf '\n'
          | PatTuple (_, _, [ PatTuple (_, _, [ PatWild ]) ]) ->
              (* Wildcard inner: no binding needed, just emit return body *)
              Printf.bprintf buf "%sreturn " (indent ^ "\t");
              gen_expr env buf (indent ^ "\t") CtxExpr arm.arm_expr;
              Buffer.add_char buf '\n'
          | PatTuple (_, _, [ PatTuple (deeper_enum, _, _deeper_inner_pats) ])
            when deeper_enum.node = "Option" || deeper_enum.node = "Result" ->
              (* Deeper nesting on Ok side: peel and recurse *)
              let peeled_arms = peel_one_builtin_level all_inner_ok_arms in
              gen_nested_result_inner_match_expr env buf (indent ^ "\t")
                nested_val peeled_arms ok_ty CtxExpr default_arm
          | _ ->
              Printf.bprintf buf "%spanic(\"unsupported nested pattern\")\n"
                (indent ^ "\t"))
      | None -> (
          (* No specific inner Ok arm - fall back to default arm *)
          match default_arm with
          | Some arm ->
              Printf.bprintf buf "%sreturn " (indent ^ "\t");
              gen_expr env buf (indent ^ "\t") CtxExpr arm.arm_expr;
              Buffer.add_char buf '\n'
          | None ->
              Printf.bprintf buf "%spanic(\"unreachable\")\n" (indent ^ "\t")));
      Printf.bprintf buf "%s} else {\n" indent;
      let nested_err = fresh_tmp env "inner_err" in
      Printf.bprintf buf "%s%s := %s.err\n" (indent ^ "\t") nested_err inner_var;
      (match inner_err_arm with
      | Some arm -> (
          match arm.arm_pat with
          | PatTuple (_, _, [ PatTuple (_, _, [ PatBind name ]) ]) ->
              Printf.bprintf buf "%s%s := %s\n" (indent ^ "\t")
                (escape_ident name.node) nested_err;
              Printf.bprintf buf "%sreturn " (indent ^ "\t");
              gen_expr env buf (indent ^ "\t") CtxExpr arm.arm_expr;
              Buffer.add_char buf '\n'
          | PatTuple (_, _, [ PatTuple (_, _, [ PatWild ]) ]) ->
              (* Wildcard inner: no binding needed, just emit return body *)
              Printf.bprintf buf "%sreturn " (indent ^ "\t");
              gen_expr env buf (indent ^ "\t") CtxExpr arm.arm_expr;
              Buffer.add_char buf '\n'
          | PatTuple (_, _, [ PatTuple (deeper_enum, _, _deeper_inner_pats) ])
            when deeper_enum.node = "Option" || deeper_enum.node = "Result" ->
              (* Deeper nesting on Err side: peel and recurse *)
              let peeled_arms = peel_one_builtin_level all_inner_err_arms in
              gen_nested_result_inner_match_expr env buf (indent ^ "\t")
                nested_err peeled_arms _err_ty CtxExpr default_arm
          | _ ->
              Printf.bprintf buf "%spanic(\"unsupported nested pattern\")\n"
                (indent ^ "\t"))
      | None -> (
          (* No specific inner Err arm - fall back to default arm *)
          match default_arm with
          | Some arm ->
              Printf.bprintf buf "%sreturn " (indent ^ "\t");
              gen_expr env buf (indent ^ "\t") CtxExpr arm.arm_expr;
              Buffer.add_char buf '\n'
          | None ->
              Printf.bprintf buf "%spanic(\"unreachable\")\n" (indent ^ "\t")));
      Printf.bprintf buf "%s}\n" indent
  | _ ->
      Printf.bprintf buf "%spanic(\"unsupported nested match type\")\n" indent

and gen_match_stmt_branch env buf indent arm default_arm bind_pattern =
  match arm with
  | Some arm ->
      bind_pattern arm;
      gen_arm_body_stmts env buf indent arm.arm_expr
  | None -> (
      match default_arm with
      | Some arm ->
          bind_pattern arm;
          gen_arm_body_stmts env buf indent arm.arm_expr
      | None ->
          Printf.bprintf buf "%spanic(\"unreachable: non-exhaustive match\")\n"
            indent)

(* Emit `return expr` for a match arm, handling Result constructors specially
   when the enclosing function/IIFE returns a Go tuple for Result types. *)
and gen_return_expr env buf indent (e : Ast.expr) =
  let go () =
    Buffer.add_string buf indent;
    Buffer.add_string buf "return ";
    gen_expr env buf indent CtxExpr e;
    Buffer.add_char buf '\n'
  in
  match e with
  | ExprCall (ExprIdent { node = "Ok"; _ }, [ arg ])
  | ExprCast (ExprCall (ExprIdent { node = "Ok"; _ }, [ arg ]), _) -> (
      match env.ret_ty with
      | Some (TyGeneric ({ node = "Result"; _ }, _))
        when should_flatten_result_return env ->
          Buffer.add_string buf indent;
          Buffer.add_string buf "return ";
          gen_expr env buf indent CtxExpr arg;
          Buffer.add_string buf ", nil\n"
      | _ -> go ())
  | ExprCall (ExprIdent { node = "Err"; _ }, [ arg ])
  | ExprCast (ExprCall (ExprIdent { node = "Err"; _ }, [ arg ]), _) -> (
      match env.ret_ty with
      | Some (TyGeneric ({ node = "Result"; _ }, [ ok_ty; _ ]))
        when should_flatten_result_return env ->
          env.shared.needs_errors <- true;
          Buffer.add_string buf indent;
          Printf.bprintf buf "return %s, errors.New(" (go_zero_value env ok_ty);
          gen_expr env buf indent CtxExpr arg;
          Buffer.add_string buf ")\n"
      | _ -> go ())
  | _ -> go ()

and gen_match_return_branch env buf indent arm default_arm bind_pattern =
  let emit_ok_return arg =
    Buffer.add_string buf indent;
    Buffer.add_string buf "return ";
    gen_expr env buf indent CtxExpr arg;
    Buffer.add_string buf ", nil\n"
  in
  let emit_err_return arg =
    match env.ret_ty with
    | Some (TyGeneric ({ node = "Result"; _ }, [ ok_ty; _ ]))
      when should_flatten_result_return env ->
        env.shared.needs_errors <- true;
        Buffer.add_string buf indent;
        Printf.bprintf buf "return %s, errors.New(" (go_zero_value env ok_ty);
        gen_expr env buf indent CtxExpr arg;
        Buffer.add_string buf ")\n"
    | _ ->
        Buffer.add_string buf indent;
        Buffer.add_string buf "return ";
        gen_expr env buf indent CtxExpr arg;
        Buffer.add_char buf '\n'
  in
  let return_expr e =
    match e with
    | ExprCall (ExprIdent { node = "Ok"; _ }, [ arg ])
    | ExprCast (ExprCall (ExprIdent { node = "Ok"; _ }, [ arg ]), _) -> (
        match env.ret_ty with
        | Some (TyGeneric ({ node = "Result"; _ }, _))
          when should_flatten_result_return env ->
            emit_ok_return arg
        | _ ->
            Buffer.add_string buf indent;
            Buffer.add_string buf "return ";
            gen_expr env buf indent CtxExpr e;
            Buffer.add_char buf '\n')
    | ExprCall (ExprIdent { node = "Err"; _ }, [ arg ])
    | ExprCast (ExprCall (ExprIdent { node = "Err"; _ }, [ arg ]), _) -> (
        match env.ret_ty with
        | Some (TyGeneric ({ node = "Result"; _ }, _))
          when should_flatten_result_return env ->
            emit_err_return arg
        | _ ->
            Buffer.add_string buf indent;
            Buffer.add_string buf "return ";
            gen_expr env buf indent CtxExpr e;
            Buffer.add_char buf '\n')
    | _ ->
        Buffer.add_string buf indent;
        Buffer.add_string buf "return ";
        gen_expr env buf indent CtxExpr e;
        Buffer.add_char buf '\n'
  in
  match arm with
  | Some arm ->
      bind_pattern arm;
      return_expr arm.arm_expr
  | None -> (
      match default_arm with
      | Some arm ->
          bind_pattern arm;
          return_expr arm.arm_expr
      | None ->
          Printf.bprintf buf "%spanic(\"unreachable: non-exhaustive match\")\n"
            indent)

(* If [scrutinee] is an identifier bound to a Result type with a known
   decomposition, return the associated error variable name. *)
and result_var_decomp env scrutinee =
  match scrutinee with
  | ExprIdent { node = name; _ } -> (
      match infer_expr_type env scrutinee with
      | Some (TyGeneric ({ node = "Result"; _ }, _)) ->
          SMap.find_opt name env.result_decomps
      | _ -> None)
  | _ -> None

and gen_result_match env buf indent ctx scrutinee arms _ok_ty _err_ty =
  let ok_arm = find_builtin_variant_arm "Ok" arms in
  let err_arm = find_builtin_variant_arm "Err" arms in
  let default_arm = find_builtin_default_arm arms in
  let is_result_var = result_var_decomp env scrutinee in
  let is_struct_result =
    match is_result_var with
    | Some _ -> false
    | None -> (
        match infer_expr_type env scrutinee with
        | Some ty when is_flat_result_ty ty -> (
            (* Flat Result: struct result only for direct constructors/idents *)
            match scrutinee with
            | ExprCall (ExprIdent { node = "Ok" | "Err"; _ }, _) -> true
            | ExprCast (e, _) when is_result_constructor_expr e -> true
            | ExprIdent _ -> true
            | _ -> false)
        | Some (TyGeneric ({ node = "Result"; _ }, _)) ->
            (* Nested Result: always treat as struct result *)
            true
        | _ -> false)
  in
  let res_name =
    match (is_result_var, is_struct_result) with
    | Some _, _ -> None
    | None, true ->
        Some
          (match scrutinee with
          | ExprIdent { node = n; _ } -> escape_ident n
          | _ -> fresh_tmp env "match_res")
    | None, false -> None
  in
  let value_name, err_name =
    match is_result_var with
    | Some err_name ->
        ( escape_ident
            (match scrutinee with ExprIdent { node = n; _ } -> n | _ -> ""),
          err_name )
    | None -> (
        match res_name with
        | Some r -> (r ^ ".value", r ^ ".err")
        | None ->
            let v = fresh_tmp env "match_val" in
            let e = fresh_tmp env "match_err" in
            (v, e))
  in
  (* Detect nested built-in patterns inside Ok/Err arms *)
  let has_nested_ok =
    match ok_arm with
    | Some arm -> (
        match arm.arm_pat with
        | PatTuple (_, _, [ PatTuple (inner_enum, _, _) ])
          when inner_enum.node = "Option" || inner_enum.node = "Result" ->
            true
        | _ -> false)
    | None -> false
  in
  let has_nested_err =
    match err_arm with
    | Some arm -> (
        match arm.arm_pat with
        | PatTuple (_, _, [ PatTuple (inner_enum, _, _) ])
          when inner_enum.node = "Option" || inner_enum.node = "Result" ->
            true
        | _ -> false)
    | None -> false
  in
  match ctx with
  | CtxStmt ->
      (match (is_result_var, res_name) with
      | Some _, _ -> Printf.bprintf buf "if %s == nil {\n" err_name
      | None, Some r ->
          (match scrutinee with
          | ExprIdent _ -> ()
          | _ ->
              Printf.bprintf buf "%s := " r;
              gen_expr env buf indent CtxExpr scrutinee;
              Printf.bprintf buf "\n%s" indent);
          Printf.bprintf buf "if %s.ok {\n" r
      | None, None ->
          Printf.bprintf buf "if %s, %s := " value_name err_name;
          gen_expr env buf indent CtxExpr scrutinee;
          Printf.bprintf buf "; %s == nil {\n" err_name);
      if has_nested_ok then
        let ok_arms = find_all_builtin_variant_arms "Ok" arms in
        gen_nested_result_inner_match_stmt env buf (indent ^ "\t") value_name
          ok_arms _ok_ty CtxStmt default_arm
      else
        gen_match_stmt_branch env buf (indent ^ "\t") ok_arm default_arm
          (gen_result_pattern_binding buf (indent ^ "\t") value_name err_name
             true);
      if has_nested_ok then Printf.bprintf buf "\n%s} else {\n" indent
      else Printf.bprintf buf "%s} else {\n" indent;
      if has_nested_err then
        let err_arms = find_all_builtin_variant_arms "Err" arms in
        gen_nested_result_inner_match_stmt env buf (indent ^ "\t") err_name
          err_arms _err_ty CtxStmt default_arm
      else
        gen_match_stmt_branch env buf (indent ^ "\t") err_arm default_arm
          (gen_result_pattern_binding buf (indent ^ "\t") value_name err_name
             false);
      Printf.bprintf buf "%s}" indent
  | CtxExpr ->
      let ret_ty =
        match arms with
        | arm :: _ -> infer_arm_expr_type env scrutinee arm
        | [] -> None
      in
      let go_sig, inner_env = result_iife_sig_and_env env ret_ty in
      Printf.bprintf buf "func() %s {\n" go_sig;
      let ni = indent ^ "\t" in
      (match (is_result_var, res_name) with
      | Some _, _ -> Printf.bprintf buf "%sif %s == nil {\n" ni err_name
      | None, Some r ->
          (match scrutinee with
          | ExprIdent _ -> ()
          | _ ->
              Printf.bprintf buf "%s%s := " ni r;
              gen_expr env buf ni CtxExpr scrutinee;
              Printf.bprintf buf "\n%s" ni);
          Printf.bprintf buf "%sif %s.ok {\n" ni r
      | None, None ->
          Printf.bprintf buf "%sif %s, %s := " ni value_name err_name;
          gen_expr env buf ni CtxExpr scrutinee;
          Printf.bprintf buf "; %s == nil {\n" err_name);
      if has_nested_ok then
        let ok_arms = find_all_builtin_variant_arms "Ok" arms in
        gen_nested_result_inner_match_expr inner_env buf (ni ^ "\t") value_name
          ok_arms _ok_ty CtxExpr default_arm
      else
        gen_result_match_return_branch inner_env buf (ni ^ "\t") ok_arm
          default_arm value_name err_name true _ok_ty _err_ty;
      if has_nested_ok then Printf.bprintf buf "\n%s} else {\n" ni
      else Printf.bprintf buf "%s} else {\n" ni;
      if has_nested_err then
        let err_arms = find_all_builtin_variant_arms "Err" arms in
        gen_nested_result_inner_match_expr inner_env buf (ni ^ "\t") err_name
          err_arms _err_ty CtxExpr default_arm
      else
        gen_result_match_return_branch inner_env buf (ni ^ "\t") err_arm
          default_arm value_name err_name false _ok_ty _err_ty;
      Printf.bprintf buf "%s}\n" ni;
      Printf.bprintf buf "%s}()" indent

and gen_result_match_return_branch env buf indent arm default_arm value_name
    err_name is_ok ok_ty _err_ty =
  let arm_to_use = match arm with Some _ -> arm | None -> default_arm in
  match arm_to_use with
  | Some arm -> (
      gen_result_pattern_binding buf indent value_name err_name is_ok arm;
      match arm.arm_expr with
      | ExprCall (ExprIdent { node = "Ok"; _ }, [ arg ]) ->
          (* Ownership: suppress consumed Drop guards before cleanup *)
          (match arg with
          | ExprIdent { node = n; _ } -> (
              match SMap.find_opt n env.drop_guards with
              | Some guard -> Printf.bprintf buf "%s%s = false\n" indent guard
              | None -> ())
          | _ -> ());
          let _cleaned = emit_return_cleanup env buf indent arg in
          Buffer.add_string buf indent;
          if should_flatten_result_return env then begin
            Buffer.add_string buf "return ";
            gen_expr env buf indent CtxExpr arg;
            Buffer.add_string buf ", nil\n"
          end
          else begin
            Buffer.add_string buf "return ";
            gen_expr env buf indent CtxExpr arm.arm_expr;
            Buffer.add_char buf '\n'
          end
      | ExprCast (ExprCall (ExprIdent { node = "Ok"; _ }, [ arg ]), _) ->
          (* Ownership: suppress consumed Drop guards before cleanup *)
          (match arg with
          | ExprIdent { node = n; _ } -> (
              match SMap.find_opt n env.drop_guards with
              | Some guard -> Printf.bprintf buf "%s%s = false\n" indent guard
              | None -> ())
          | _ -> ());
          let _cleaned = emit_return_cleanup env buf indent arg in
          Buffer.add_string buf indent;
          if should_flatten_result_return env then begin
            Buffer.add_string buf "return ";
            gen_expr env buf indent CtxExpr arg;
            Buffer.add_string buf ", nil\n"
          end
          else begin
            Buffer.add_string buf "return ";
            gen_expr env buf indent CtxExpr arm.arm_expr;
            Buffer.add_char buf '\n'
          end
      | ExprCall (ExprIdent { node = "Err"; _ }, [ arg ]) ->
          (* Ownership: suppress consumed Drop guards before cleanup *)
          (match arg with
          | ExprIdent { node = n; _ } -> (
              match SMap.find_opt n env.drop_guards with
              | Some guard -> Printf.bprintf buf "%s%s = false\n" indent guard
              | None -> ())
          | _ -> ());
          let _cleaned = emit_return_cleanup env buf indent arg in
          Buffer.add_string buf indent;
          if should_flatten_result_return env then begin
            env.shared.needs_errors <- true;
            Printf.bprintf buf "return %s, errors.New("
              (go_zero_value env ok_ty);
            gen_expr env buf indent CtxExpr arg;
            Buffer.add_string buf ")\n"
          end
          else begin
            Buffer.add_string buf "return ";
            gen_expr env buf indent CtxExpr arm.arm_expr;
            Buffer.add_char buf '\n'
          end
      | ExprCast (ExprCall (ExprIdent { node = "Err"; _ }, [ arg ]), _) ->
          (* Ownership: suppress consumed Drop guards before cleanup *)
          (match arg with
          | ExprIdent { node = n; _ } -> (
              match SMap.find_opt n env.drop_guards with
              | Some guard -> Printf.bprintf buf "%s%s = false\n" indent guard
              | None -> ())
          | _ -> ());
          let _cleaned = emit_return_cleanup env buf indent arg in
          Buffer.add_string buf indent;
          if should_flatten_result_return env then begin
            env.shared.needs_errors <- true;
            Printf.bprintf buf "return %s, errors.New("
              (go_zero_value env ok_ty);
            gen_expr env buf indent CtxExpr arg;
            Buffer.add_string buf ")\n"
          end
          else begin
            Buffer.add_string buf "return ";
            gen_expr env buf indent CtxExpr arm.arm_expr;
            Buffer.add_char buf '\n'
          end
      | _ ->
          let _cleaned = emit_return_cleanup env buf indent arm.arm_expr in
          Buffer.add_string buf indent;
          Buffer.add_string buf "return ";
          gen_expr env buf indent CtxExpr arm.arm_expr;
          Buffer.add_char buf '\n')
  | None ->
      Printf.bprintf buf "%spanic(\"unreachable: non-exhaustive match\")\n"
        indent

and gen_result_match_as_return env buf indent scrutinee arms ok_ty err_ty =
  let ok_arm = find_builtin_variant_arm "Ok" arms in
  let err_arm = find_builtin_variant_arm "Err" arms in
  let default_arm = find_builtin_default_arm arms in
  let is_result_var = result_var_decomp env scrutinee in
  let is_struct_result =
    match is_result_var with
    | Some _ -> false
    | None -> (
        match infer_expr_type env scrutinee with
        | Some ty when is_flat_result_ty ty -> (
            (* Flat Result: struct result only for direct constructors/idents *)
            match scrutinee with
            | ExprCall (ExprIdent { node = "Ok" | "Err"; _ }, _) -> true
            | ExprCast (e, _) when is_result_constructor_expr e -> true
            | ExprIdent _ -> true
            | _ -> false)
        | Some (TyGeneric ({ node = "Result"; _ }, _)) ->
            (* Nested Result: always treat as struct result *)
            true
        | _ -> false)
  in
  let res_name =
    match (is_result_var, is_struct_result) with
    | Some _, _ -> None
    | None, true ->
        Some
          (match scrutinee with
          | ExprIdent { node = n; _ } -> escape_ident n
          | _ -> fresh_tmp env "match_res")
    | None, false -> None
  in
  let value_name, err_name =
    match is_result_var with
    | Some err_name ->
        ( escape_ident
            (match scrutinee with ExprIdent { node = n; _ } -> n | _ -> ""),
          err_name )
    | None -> (
        match res_name with
        | Some r -> (r ^ ".value", r ^ ".err")
        | None ->
            let v = fresh_tmp env "match_val" in
            let e = fresh_tmp env "match_err" in
            (v, e))
  in
  let has_nested_ok =
    match ok_arm with
    | Some arm -> (
        match arm.arm_pat with
        | PatTuple (_, _, [ PatTuple (inner_enum, _, _) ])
          when inner_enum.node = "Option" || inner_enum.node = "Result" ->
            true
        | _ -> false)
    | None -> false
  in
  (match (is_result_var, res_name) with
  | Some _, _ -> Printf.bprintf buf "%sif %s == nil {\n" indent err_name
  | None, Some r ->
      (match scrutinee with
      | ExprIdent _ -> ()
      | _ ->
          Printf.bprintf buf "%s%s := " indent r;
          gen_expr env buf indent CtxExpr scrutinee;
          Printf.bprintf buf "\n");
      Printf.bprintf buf "%sif %s.ok {\n" indent r
  | None, None ->
      Printf.bprintf buf "%sif %s, %s := " indent value_name err_name;
      gen_expr env buf indent CtxExpr scrutinee;
      Printf.bprintf buf "; %s == nil {\n" err_name);
  if has_nested_ok then
    let ok_arms = find_all_builtin_variant_arms "Ok" arms in
    gen_nested_result_inner_match_expr env buf (indent ^ "\t") value_name
      ok_arms ok_ty CtxExpr default_arm
  else
    gen_result_match_return_branch env buf (indent ^ "\t") ok_arm default_arm
      value_name err_name true ok_ty err_ty;
  if has_nested_ok then Printf.bprintf buf "\n%s} else {\n" indent
  else Printf.bprintf buf "%s} else {\n" indent;
  gen_result_match_return_branch env buf (indent ^ "\t") err_arm default_arm
    value_name err_name false ok_ty err_ty;
  Printf.bprintf buf "%s}\n" indent

and gen_option_match env buf indent ctx scrutinee arms inner_ty =
  let some_arm = find_builtin_variant_arm "Some" arms in
  let none_arm = find_builtin_variant_arm "None" arms in
  let default_arm = find_builtin_default_arm arms in
  let opt_name = fresh_tmp env "match_opt" in
  let cond =
    if is_nullable_ty env inner_ty then opt_name ^ ".some"
    else opt_name ^ " != nil"
  in
  (* Detect nested built-in patterns: if the Some arm has an inner pattern
     that is itself Option/Result, we need recursive matching. *)
  let has_nested_builtin =
    match some_arm with
    | Some arm -> (
        match arm.arm_pat with
        | PatTuple (_, _, [ PatTuple (inner_enum, _, _) ])
          when inner_enum.node = "Option" || inner_enum.node = "Result" ->
            true
        | _ -> false)
    | None -> false
  in
  match ctx with
  | CtxStmt ->
      Printf.bprintf buf "if %s := " opt_name;
      gen_expr env buf indent CtxExpr scrutinee;
      Printf.bprintf buf "; %s {\n" cond;
      if has_nested_builtin then
        (* Collect all Some arms for nested matching *)
        let some_arms = find_all_builtin_variant_arms "Some" arms in
        let nested_inner_ty =
          match inner_type_of_builtin inner_ty with
          | Some ty -> ty
          | None -> inner_ty
        in
        gen_nested_option_match_stmt env buf (indent ^ "\t") opt_name
          (fresh_tmp env "inner_opt")
          some_arms inner_ty nested_inner_ty CtxStmt default_arm
      else begin
        let some_env =
          match some_arm with
          | Some arm -> env_with_option_binding env arm inner_ty
          | None -> (
              match default_arm with
              | Some arm -> env_with_option_binding env arm inner_ty
              | None -> env)
        in
        gen_match_stmt_branch some_env buf (indent ^ "\t") some_arm default_arm
          (fun arm ->
            gen_option_pattern_binding env buf (indent ^ "\t") opt_name arm
              inner_ty)
      end;
      if has_nested_builtin then Printf.bprintf buf "\n%s} else {\n" indent
      else Printf.bprintf buf "%s} else {\n" indent;
      gen_match_stmt_branch env buf (indent ^ "\t") none_arm default_arm
        (fun _ -> ());
      Printf.bprintf buf "%s}" indent
  | CtxExpr ->
      let ret_ty =
        match arms with
        | arm :: _ -> infer_arm_expr_type env scrutinee arm
        | [] -> None
      in
      let gt = match ret_ty with Some t -> go_type env t | None -> "any" in
      Printf.bprintf buf "func() %s {\n" gt;
      let iife_env = { env with ret_ty; in_iife = true } in
      let ni = indent ^ "\t" in
      Printf.bprintf buf "%sif %s := " ni opt_name;
      gen_expr env buf ni CtxExpr scrutinee;
      Printf.bprintf buf "; %s {\n" cond;
      if has_nested_builtin then
        let some_arms = find_all_builtin_variant_arms "Some" arms in
        let nested_inner_ty =
          match inner_type_of_builtin inner_ty with
          | Some ty -> ty
          | None -> inner_ty
        in
        gen_nested_option_match_expr iife_env buf (ni ^ "\t") opt_name
          (fresh_tmp iife_env "inner_opt")
          some_arms inner_ty nested_inner_ty CtxExpr default_arm
      else begin
        let some_env =
          match some_arm with
          | Some arm -> env_with_option_binding iife_env arm inner_ty
          | None -> (
              match default_arm with
              | Some arm -> env_with_option_binding iife_env arm inner_ty
              | None -> iife_env)
        in
        gen_match_return_branch some_env buf (ni ^ "\t") some_arm default_arm
          (fun arm ->
            gen_option_pattern_binding iife_env buf (ni ^ "\t") opt_name arm
              inner_ty)
      end;
      if has_nested_builtin then Printf.bprintf buf "\n%s} else {\n" ni
      else Printf.bprintf buf "%s} else {\n" ni;
      gen_match_return_branch iife_env buf (ni ^ "\t") none_arm default_arm
        (fun _ -> ());
      Printf.bprintf buf "%s}\n" ni;
      Printf.bprintf buf "%s}()" indent

and gen_option_match_as_return env buf indent scrutinee arms inner_ty =
  let some_arm = find_builtin_variant_arm "Some" arms in
  let none_arm = find_builtin_variant_arm "None" arms in
  let default_arm = find_builtin_default_arm arms in
  let opt_name = fresh_tmp env "match_opt" in
  let cond =
    if is_nullable_ty env inner_ty then opt_name ^ ".some"
    else opt_name ^ " != nil"
  in
  let has_nested_builtin =
    match some_arm with
    | Some arm -> (
        match arm.arm_pat with
        | PatTuple (_, _, [ PatTuple (inner_enum, _, _) ])
          when inner_enum.node = "Option" || inner_enum.node = "Result" ->
            true
        | _ -> false)
    | None -> false
  in
  Printf.bprintf buf "%sif %s := " indent opt_name;
  gen_expr env buf indent CtxExpr scrutinee;
  Printf.bprintf buf "; %s {\n" cond;
  if has_nested_builtin then
    let some_arms = find_all_builtin_variant_arms "Some" arms in
    let nested_inner_ty =
      match inner_type_of_builtin inner_ty with
      | Some ty -> ty
      | None -> inner_ty
    in
    gen_nested_option_match_expr env buf (indent ^ "\t") opt_name
      (fresh_tmp env "inner_opt")
      some_arms inner_ty nested_inner_ty CtxExpr default_arm
  else begin
    let some_env =
      match some_arm with
      | Some arm -> env_with_option_binding env arm inner_ty
      | None -> (
          match default_arm with
          | Some arm -> env_with_option_binding env arm inner_ty
          | None -> env)
    in
    gen_match_return_branch some_env buf (indent ^ "\t") some_arm default_arm
      (fun arm ->
        gen_option_pattern_binding env buf (indent ^ "\t") opt_name arm inner_ty)
  end;
  if has_nested_builtin then Printf.bprintf buf "\n%s} else {\n" indent
  else Printf.bprintf buf "%s} else {\n" indent;
  gen_match_return_branch env buf (indent ^ "\t") none_arm default_arm (fun _ ->
      ());
  Printf.bprintf buf "%s}\n" indent

and gen_block_with_return env buf indent blk =
  let inner = push_scope ~is_function:false env in
  let inner =
    List.fold_left
      (fun env s ->
        let needs_stmt_indent =
          match s with
          | StmtExpr (ExprReturn _ | ExprBreak | ExprContinue) -> false
          | _ -> true
        in
        if needs_stmt_indent then Buffer.add_string buf indent;
        let env = gen_stmt env buf indent s in
        Buffer.add_char buf '\n';
        env)
      inner blk.stmts
  in
  (match blk.final_expr with
  | Some e -> gen_final_expr_as_return inner buf indent e
  | None -> ());
  let ends_with_early_exit =
    match blk.final_expr with
    | Some e when is_return_like_expr e -> true
    (* gen_final_expr_as_return handles nested cleanup for all final
       expressions that generate a return statement, so scope cleanup
       after the final expression is not needed. *)
    | Some _ -> true
    | None -> (
        match List.rev blk.stmts with
        | StmtExpr (ExprReturn _ | ExprBreak | ExprContinue) :: _ -> true
        | _ -> false)
  in
  if not ends_with_early_exit then emit_scope_cleanup inner buf indent

and gen_block_stmts env buf indent blk =
  let inner = push_scope ~is_function:false env in
  let inner =
    List.fold_left
      (fun env s ->
        let needs_stmt_indent =
          match s with
          | StmtExpr (ExprReturn _ | ExprBreak | ExprContinue) -> false
          | _ -> true
        in
        if needs_stmt_indent then Buffer.add_string buf indent;
        let env = gen_stmt env buf indent s in
        Buffer.add_char buf '\n';
        env)
      inner blk.stmts
  in
  let ends_with_early_exit =
    match blk.final_expr with
    | Some e when is_return_like_expr e -> true
    | _ -> (
        match List.rev blk.stmts with
        | StmtExpr (ExprReturn _ | ExprBreak | ExprContinue) :: _ -> true
        | _ -> false)
  in
  match blk.final_expr with
  | Some e ->
      (* Early-exit expressions (return, break, continue) manage their own
         indentation via emit_return_cleanup / emit_loop_cleanup, so do not
         add indent here to avoid double-indenting. *)
      let needs_indent =
        match e with
        | ExprReturn _ | ExprBreak | ExprContinue -> false
        | _ -> true
      in
      if contains_question e then begin
        let e' = hoist_question_exprs inner buf indent e in
        if needs_indent then Buffer.add_string buf indent;
        gen_expr inner buf indent CtxStmt e';
        Buffer.add_char buf '\n'
      end
      else begin
        if needs_indent then Buffer.add_string buf indent;
        gen_expr inner buf indent CtxStmt e;
        Buffer.add_char buf '\n'
      end;
      if not ends_with_early_exit then emit_scope_cleanup inner buf indent
  | None -> if not ends_with_early_exit then emit_scope_cleanup inner buf indent

and gen_block_expr env buf indent ctx blk =
  match ctx with
  | CtxStmt -> gen_block_stmts env buf indent blk
  | CtxExpr ->
      let ret_ty = infer_block_type_with_local_fallback env blk in
      let go_sig, inner_env = result_iife_sig_and_env env ret_ty in
      Printf.bprintf buf "func() %s {\n" go_sig;
      gen_block_with_return inner_env buf (indent ^ "\t") blk;
      Printf.bprintf buf "%s}()" indent

(* Check if an expression contains any nested ExprQuestion *)
and contains_question (e : Ast.expr) : bool =
  match e with
  | ExprQuestion _ -> true
  | ExprLit _ | ExprIdent _ | ExprSelf | ExprPath _ | ExprBreak | ExprContinue
    ->
      false
  | ExprUnary (_, e1) -> contains_question e1
  | ExprBinary (_, l, r) -> contains_question l || contains_question r
  | ExprCall (callee, args) ->
      contains_question callee || List.exists contains_question args
  | ExprMethodCall (recv, _, args) ->
      contains_question recv || List.exists contains_question args
  | ExprFieldAccess (e1, _) -> contains_question e1
  | ExprIndex (arr, idx) -> contains_question arr || contains_question idx
  | ExprCast (e1, _) -> contains_question e1
  | ExprAssign (_, lhs, rhs) -> contains_question lhs || contains_question rhs
  | ExprReturn (Some e1) -> contains_question e1
  | ExprReturn None -> false
  | ExprArray elems -> List.exists contains_question elems
  | ExprRepeat (elem, cnt) -> contains_question elem || contains_question cnt
  | ExprStruct (_, fields) ->
      List.exists
        (fun (sf : Ast.struct_field_init) -> contains_question sf.sf_expr)
        fields
  | ExprStructVariant (_, _, fields) ->
      List.exists
        (fun (sf : Ast.struct_field_init) -> contains_question sf.sf_expr)
        fields
  | ExprIf _ | ExprMatch _ | ExprBlock _ | ExprLoop _ | ExprWhile _ | ExprFor _
  | ExprLambda _ ->
      false

(* Hoist ExprQuestion subexpressions out of an expression tree.
   Emits temp variable assignments and early-return checks as statements
   before the containing expression, returns a rewritten expression with
   ExprIdent temps in place of the ExprQuestion nodes.
   The caller must ensure the buffer is at the start of a line with correct
   indent before calling. The hoisted code is emitted as full indented lines. *)
and hoist_question_exprs env buf indent (e : Ast.expr) : Ast.expr =
  match e with
  | ExprQuestion inner ->
      let tmp_name = fresh_tmp env "qm" in
      gen_question_let env buf indent tmp_name inner;
      Buffer.add_char buf '\n';
      Printf.bprintf buf "%s_ = %s\n" indent tmp_name;
      ExprIdent (dummy_loc tmp_name)
  | ExprLit _ | ExprIdent _ | ExprSelf | ExprPath _ | ExprBreak | ExprContinue
  | ExprReturn None ->
      e
  | ExprUnary (op, e1) ->
      let e1' = hoist_question_exprs env buf indent e1 in
      ExprUnary (op, e1')
  | ExprBinary (op, l, r) ->
      let l' = hoist_question_exprs env buf indent l in
      let r' = hoist_question_exprs env buf indent r in
      ExprBinary (op, l', r')
  | ExprCall (callee, args) ->
      let callee' = hoist_question_exprs env buf indent callee in
      let args' = List.map (hoist_question_exprs env buf indent) args in
      ExprCall (callee', args')
  | ExprMethodCall (recv, meth, args) ->
      let recv' = hoist_question_exprs env buf indent recv in
      let args' = List.map (hoist_question_exprs env buf indent) args in
      ExprMethodCall (recv', meth, args')
  | ExprFieldAccess (e1, f) ->
      let e1' = hoist_question_exprs env buf indent e1 in
      ExprFieldAccess (e1', f)
  | ExprIndex (arr, idx) ->
      let arr' = hoist_question_exprs env buf indent arr in
      let idx' = hoist_question_exprs env buf indent idx in
      ExprIndex (arr', idx')
  | ExprCast (e1, ty) ->
      let e1' = hoist_question_exprs env buf indent e1 in
      ExprCast (e1', ty)
  | ExprAssign (op, lhs, rhs) ->
      let lhs' = hoist_question_exprs env buf indent lhs in
      let rhs' = hoist_question_exprs env buf indent rhs in
      ExprAssign (op, lhs', rhs')
  | ExprReturn (Some e1) ->
      let e1' = hoist_question_exprs env buf indent e1 in
      ExprReturn (Some e1')
  | ExprArray elems ->
      let elems' = List.map (hoist_question_exprs env buf indent) elems in
      ExprArray elems'
  | ExprRepeat (elem, cnt) ->
      let elem' = hoist_question_exprs env buf indent elem in
      let cnt' = hoist_question_exprs env buf indent cnt in
      ExprRepeat (elem', cnt')
  | ExprStruct (ty, fields) ->
      let fields' =
        List.map
          (fun (sf : Ast.struct_field_init) ->
            { sf with sf_expr = hoist_question_exprs env buf indent sf.sf_expr })
          fields
      in
      ExprStruct (ty, fields')
  | ExprStructVariant (tn, vn, fields) ->
      let fields' =
        List.map
          (fun (sf : Ast.struct_field_init) ->
            { sf with sf_expr = hoist_question_exprs env buf indent sf.sf_expr })
          fields
      in
      ExprStructVariant (tn, vn, fields')
  | ExprIf _ | ExprMatch _ | ExprBlock _ | ExprLoop _ | ExprWhile _ | ExprFor _
  | ExprLambda _ ->
      (* Don't descend into blocks/control flow - ? inside those is handled
         at their own statement boundaries *)
      e

and gen_question env buf indent e =
  (* This should only be reached for top-level ? in expression context.
     For nested ?, the hoist_question_exprs pass should have already
     extracted it. Use the same pattern as gen_question_stmt but inline. *)
  let tmp_name = fresh_tmp env "qm" in
  gen_question_let env buf indent tmp_name e;
  Buffer.add_char buf '\n';
  Printf.bprintf buf "%s_ = %s\n" indent tmp_name;
  (* Now emit just the temp name as the expression value *)
  Buffer.add_string buf indent;
  Buffer.add_string buf tmp_name

and gen_array_literal env buf indent elems =
  match elems with
  | [] -> Buffer.add_string buf "nil" (* should have type annotation *)
  | first :: _ ->
      let elem_ty =
        match infer_expr_type env first with
        | Some t -> go_type env t
        | None -> "any"
      in
      Printf.bprintf buf "[]%s{" elem_ty;
      List.iteri
        (fun i e ->
          if i > 0 then Buffer.add_string buf ", ";
          gen_expr env buf indent CtxExpr e)
        elems;
      Buffer.add_char buf '}'

and gen_repeat env buf indent elem count =
  (* Check if count is a small constant for inline expansion *)
  let inline_count =
    match count with
    | ExprLit (LitInt s) -> (
        try
          let n = int_of_string s in
          if n <= 16 then Some n else None
        with _ -> None)
    | _ -> None
  in
  match inline_count with
  | Some n ->
      let elem_ty =
        match infer_expr_type env elem with
        | Some t -> go_type env t
        | None -> "any"
      in
      Printf.bprintf buf "[]%s{" elem_ty;
      for i = 0 to n - 1 do
        if i > 0 then Buffer.add_string buf ", ";
        gen_expr env buf indent CtxExpr elem
      done;
      Buffer.add_char buf '}'
  | None ->
      env.shared.needs_rgo_repeat <- true;
      let elem_ty =
        match infer_expr_type env elem with
        | Some t -> go_type env t
        | None -> "any"
      in
      Printf.bprintf buf "rgo_repeat[%s](" elem_ty;
      gen_expr env buf indent CtxExpr elem;
      Buffer.add_string buf ", ";
      gen_expr env buf indent CtxExpr count;
      Buffer.add_char buf ')'

(* Ownership: after emitting a statement expression, suppress guards for any
   Drop-type identifiers that were consumed by by-value calls. *)
and suppress_consumed_guards env buf indent (e : Ast.expr) : unit =
  Codegen_ownership.suppress_consumed_guards
    ~lookup_guard:(fun n -> SMap.find_opt n env.drop_guards)
    ~is_user_fn:(fun n -> SMap.mem n env.fns)
    ~is_user_method:(fun tn mn ->
      match SMap.find_opt tn env.impls with
      | Some ii -> SMap.mem mn ii.ii_methods
      | None -> false)
    ~is_stdlib_call:(fun pkg _member ->
      is_imported_package pkg env || SMap.mem pkg env.enums)
    ~lookup_value_ty:(fun n -> lookup_value_ty n env)
    buf indent e

and gen_stmt env buf indent (s : Ast.stmt) : cg_env =
  match s with
  | StmtLet { pat = PatBind name; ty; init; is_mut } -> (
      let binding_ty =
        match ty with Some t -> Some t | None -> infer_expr_type env init
      in
      let esc_name = escape_ident name.node in
      let env =
        match (ty, init) with
        | Some t, ExprArray [] ->
            Printf.bprintf buf "var %s %s" esc_name (go_type env t);
            env
        | ( Some (TyGeneric ({ node = "HashMap"; _ }, _) as t),
            ExprCall
              (ExprPath ({ node = "HashMap"; _ }, { node = "new"; _ }), []) ) ->
            Printf.bprintf buf "%s := make(%s)" esc_name (go_type env t);
            env
        | ( Some (TyGeneric ({ node = "Vec"; _ }, _) as t),
            ExprCall (ExprPath ({ node = "Vec"; _ }, { node = "new"; _ }), []) )
          ->
            Printf.bprintf buf "%s := make(%s, 0)" esc_name (go_type env t);
            env
        | Some t, _ when needs_explicit_type t init ->
            Buffer.add_string buf "var ";
            Buffer.add_string buf esc_name;
            Buffer.add_char buf ' ';
            Buffer.add_string buf (go_type env t);
            Buffer.add_string buf " = ";
            gen_expr env buf indent CtxExpr init;
            env
        | _, _ when init_is_enum env init ->
            let enum_name = infer_enum_name env init in
            Printf.bprintf buf "var %s %s = " esc_name enum_name;
            gen_expr env buf indent CtxExpr init;
            env
        | _, _ when init_is_result env init ->
            let err_name =
              gen_result_let_binding env buf indent esc_name init
            in
            {
              env with
              result_decomps = SMap.add esc_name err_name env.result_decomps;
            }
        | _, ExprQuestion inner_e ->
            gen_question_let env buf indent esc_name inner_e;
            env
        | _ when contains_question init ->
            let init' = hoist_question_exprs env buf indent init in
            Buffer.add_string buf indent;
            Buffer.add_string buf esc_name;
            Buffer.add_string buf " := ";
            gen_let_init env buf indent ty init';
            env
        | _ ->
            Buffer.add_string buf esc_name;
            Buffer.add_string buf " := ";
            gen_let_init env buf indent ty init;
            env
      in
      Printf.bprintf buf "\n%s_ = %s\n" indent esc_name;
      (* Ownership: suppress guard for source binding if assignment moves a Drop value *)
      (match init with
      | ExprIdent { node = src_name; _ } ->
          Codegen_ownership.suppress_move_guard
            ~lookup_guard:(fun n -> SMap.find_opt n env.drop_guards)
            buf indent src_name
      | _ -> ());
      (* Ownership: suppress guards for identifiers consumed by calls in init *)
      Codegen_ownership.suppress_consumed_guards_inline
        ~lookup_guard:(fun n -> SMap.find_opt n env.drop_guards)
        ~is_user_fn:(fun n -> SMap.mem n env.fns)
        ~is_user_method:(fun tn mn ->
          match SMap.find_opt tn env.impls with
          | Some ii -> SMap.mem mn ii.ii_methods
          | None -> false)
        ~is_stdlib_call:(fun pkg _member ->
          is_imported_package pkg env || SMap.mem pkg env.enums)
        ~lookup_value_ty:(fun n -> lookup_value_ty n env)
        buf indent init;
      (* Ownership: emit defer cleanup for Drop-type bindings at function scope,
         or track for explicit cleanup at nested scope exit. *)
      match binding_ty with
      | Some t when is_drop_type_cg env t ->
          let guard = fresh_tmp env "live" in
          let env = add_value name.node binding_ty ~is_mut env in
          let env =
            { env with drop_guards = SMap.add name.node guard env.drop_guards }
          in
          if env.scope_drops = [] then (
            (* Function scope: use defer *)
            Codegen_ownership.emit_drop_defer buf ~indent ~guard
              ~binding:esc_name;
            env)
          else (
            (* Nested scope: declare guard and track for explicit cleanup.
               Track the source binding name (not the guard name) so that
               emit_scope_cleanup can look up the guard via drop_guards. *)
            Printf.bprintf buf "%s%s := true\n" indent guard;
            match env.scope_drops with
            | current_drops :: rest ->
                { env with scope_drops = (name.node :: current_drops) :: rest }
            | [] -> env)
      | _ -> add_value name.node binding_ty ~is_mut env)
  | StmtLet { pat = PatWild; init = ExprQuestion inner_e; _ } ->
      gen_question_stmt env buf indent inner_e;
      env
  | StmtLet { pat = PatWild; init; _ } when contains_question init ->
      let init' = hoist_question_exprs env buf indent init in
      Buffer.add_string buf indent;
      if init_is_result env init then Buffer.add_string buf "_, _ = "
      else Buffer.add_string buf "_ = ";
      gen_expr env buf indent CtxExpr init';
      env
  | StmtLet { pat = PatWild; init; _ } ->
      if init_is_result env init then Buffer.add_string buf "_, _ = "
      else Buffer.add_string buf "_ = ";
      gen_expr env buf indent CtxExpr init;
      env
  | StmtLet _ -> failwith "codegen: unsupported let pattern"
  | StmtExpr (ExprQuestion e) ->
      gen_question_stmt env buf indent e;
      env
  | StmtExpr (ExprReturn _ as ret) ->
      if contains_question ret then begin
        let ret' = hoist_question_exprs env buf indent ret in
        Buffer.add_string buf indent;
        gen_expr env buf indent CtxStmt ret'
      end
      else gen_expr env buf indent CtxStmt ret;
      env
  | StmtExpr (ExprAssign (Assign, ExprIdent { node = lhs_name; _ }, _rhs) as e)
    when SMap.mem lhs_name env.drop_guards ->
      (* Ownership: overwrite cleanup — drop the old value before reassigning *)
      let esc = escape_ident lhs_name in
      Codegen_ownership.emit_overwrite_drop buf indent esc;
      gen_expr env buf indent CtxStmt e;
      env
  | StmtExpr (ExprBlock blk as e) ->
      (* Block expressions in statement context: the outer fold_stmts has
         already written [indent] to the buffer, but gen_block_stmts adds
         its own [indent] per inner statement.  Remove the stray outer
         indent to avoid double-indenting the first line of the block.
         After processing, restore outer scope so subsequent function-level
         Drop bindings still use defer rather than scope-based tracking. *)
      Buffer.truncate buf (Buffer.length buf - String.length indent);
      let outer_scope_drops = env.scope_drops in
      let outer_values = env.values in
      let outer_result_decomps = env.result_decomps in
      let inner = push_scope ~is_function:false env in
      let block_indent = indent ^ "\t" in
      Printf.bprintf buf "%s{\n" indent;
      let inner =
        List.fold_left
          (fun env s ->
            let needs_stmt_indent =
              match s with
              | StmtExpr (ExprReturn _ | ExprBreak | ExprContinue) -> false
              | _ -> true
            in
            if needs_stmt_indent then Buffer.add_string buf block_indent;
            let env = gen_stmt env buf block_indent s in
            Buffer.add_char buf '\n';
            env)
          inner blk.stmts
      in
      let ends_with_early_exit =
        match blk.final_expr with
        | Some e when is_return_like_expr e -> true
        | _ -> (
            match List.rev blk.stmts with
            | StmtExpr (ExprReturn _ | ExprBreak | ExprContinue) :: _ -> true
            | _ -> false)
      in
      (match blk.final_expr with
      | Some e ->
          let needs_indent =
            match e with
            | ExprReturn _ | ExprBreak | ExprContinue -> false
            | _ -> true
          in
          if contains_question e then begin
            let e' = hoist_question_exprs inner buf block_indent e in
            if needs_indent then Buffer.add_string buf block_indent;
            gen_expr inner buf block_indent CtxStmt e';
            Buffer.add_char buf '\n'
          end
          else begin
            if needs_indent then Buffer.add_string buf block_indent;
            gen_expr inner buf block_indent CtxStmt e;
            Buffer.add_char buf '\n'
          end;
          if not ends_with_early_exit then
            emit_scope_cleanup inner buf block_indent
      | None ->
          if not ends_with_early_exit then
            emit_scope_cleanup inner buf block_indent);
      Printf.bprintf buf "%s}\n" indent;
      suppress_consumed_guards env buf indent e;
      (* Restore outer scope, but keep inner scope's updates to
         drop_guards (which may have been suppressed by inner moves)
         and shared mutable state. *)
      {
        inner with
        scope_drops = outer_scope_drops;
        values = outer_values;
        result_decomps = outer_result_decomps;
      }
  | StmtExpr e ->
      if contains_question e then begin
        let e' = hoist_question_exprs env buf indent e in
        Buffer.add_string buf indent;
        gen_expr env buf indent CtxStmt e'
      end
      else gen_expr env buf indent CtxStmt e;
      (* Ownership: suppress guards for Drop-type identifiers consumed by calls *)
      suppress_consumed_guards env buf indent e;
      env

and init_is_enum env (init : Ast.expr) : bool =
  match init with
  | ExprPath (type_name, _) -> SMap.mem type_name.node env.enums
  | ExprCall (ExprPath (type_name, _), _) -> SMap.mem type_name.node env.enums
  | _ -> false

and infer_enum_name env (init : Ast.expr) : string =
  match infer_expr_type env init with
  | Some t -> go_type env t
  | None -> (
      match init with
      | ExprPath (type_name, _) -> type_name.node
      | ExprCall (ExprPath (type_name, _), _) -> type_name.node
      | _ -> "any")

and is_result_constructor_expr (e : Ast.expr) : bool =
  match e with
  | ExprCall (ExprIdent { node = "Ok" | "Err"; _ }, _) -> true
  | ExprCast (inner, _) -> is_result_constructor_expr inner
  | _ -> false

and init_is_result env (init : Ast.expr) : bool =
  match infer_expr_type env init with
  | Some (TyGeneric ({ node = "Result"; _ }, _)) -> (
      match init with
      | ExprQuestion _ -> false
      | ExprCall (ExprIdent { node = "Ok" | "Err"; _ }, _) -> false
      | ExprCast (inner, _) -> not (is_result_constructor_expr inner)
      | ExprIdent { node = name; _ } -> SMap.mem name env.result_decomps
      | ExprBlock _ | ExprIf _ | ExprMatch _ -> false
      | _ -> (
          match infer_expr_type env init with
          | Some t -> is_flat_result_ty t
          | None -> false))
  | _ -> false

and gen_result_let_binding env buf indent name init =
  let err_name = fresh_tmp env "err" in
  Printf.bprintf buf "%s, %s := " name err_name;
  gen_expr env buf indent CtxExpr init;
  Printf.bprintf buf "\n%s_ = %s" indent err_name;
  err_name

and gen_question_let env buf indent name inner_e =
  let expr_ty = infer_expr_type env inner_e in
  match expr_ty with
  | Some (TyGeneric ({ node = "Result"; _ }, [ _ok_ty; _err_ty ])) ->
      if is_flat_result_ty (Option.get expr_ty) then begin
        let err_name = fresh_tmp env "err" in
        Printf.bprintf buf "%s, %s := " name err_name;
        gen_expr env buf indent CtxExpr inner_e;
        Printf.bprintf buf "\n%sif %s != nil {\n" indent err_name;
        (* Ownership: emit cleanup for nested scope drops before the
           early return triggered by the ? propagation. *)
        emit_all_nested_cleanup env buf (indent ^ "\t");
        (match env.ret_ty with
        | Some (TyGeneric ({ node = "Result"; _ }, [ ret_ok_ty; _err_ty ]))
          when should_flatten_result_return env ->
            Printf.bprintf buf "%s\treturn %s, %s\n" indent
              (go_zero_value env ret_ok_ty)
              err_name
        | Some (TyGeneric ({ node = "Result"; _ }, [ ret_ok_ty; ret_err_ty ]))
          ->
            env.shared.needs_result_struct <- true;
            Printf.bprintf buf "%s\treturn Result[%s, %s]{err: %s}\n" indent
              (go_type env ret_ok_ty) (go_type env ret_err_ty) err_name
        | _ -> Printf.bprintf buf "%s\treturn %s\n" indent err_name);
        Printf.bprintf buf "%s}" indent
      end
      else begin
        let tmp_res = fresh_tmp env "res" in
        Printf.bprintf buf "%s := " tmp_res;
        gen_expr env buf indent CtxExpr inner_e;
        Printf.bprintf buf "\n%sif !%s.ok {\n" indent tmp_res;
        (* Ownership: emit cleanup for nested scope drops before the
           early return triggered by the ? propagation. *)
        emit_all_nested_cleanup env buf (indent ^ "\t");
        (match env.ret_ty with
        | Some (TyGeneric ({ node = "Result"; _ }, [ ret_ok_ty; _err_ty ]))
          when should_flatten_result_return env ->
            Printf.bprintf buf "%s\treturn %s, %s\n" indent
              (go_zero_value env ret_ok_ty)
              (tmp_res ^ ".err")
        | Some (TyGeneric ({ node = "Result"; _ }, [ ret_ok_ty; ret_err_ty ]))
          ->
            env.shared.needs_result_struct <- true;
            Printf.bprintf buf "%s\treturn Result[%s, %s]{err: %s}\n" indent
              (go_type env ret_ok_ty) (go_type env ret_err_ty) (tmp_res ^ ".err")
        | _ -> Printf.bprintf buf "%s\treturn %s\n" indent (tmp_res ^ ".err"));
        Printf.bprintf buf "%s}\n" indent;
        Printf.bprintf buf "%s%s := %s.value" indent name tmp_res
      end
  | Some (TyGeneric ({ node = "Option"; _ }, [ inner ])) ->
      if is_nullable_ty env inner then begin
        env.shared.needs_option_struct <- true;
        let tmp = fresh_tmp env "opt" in
        Printf.bprintf buf "%s := " tmp;
        gen_expr env buf indent CtxExpr inner_e;
        Printf.bprintf buf "\n%sif !%s.some {\n" indent tmp;
        (* Ownership: emit cleanup for nested scope drops before the
           early return triggered by the ? propagation. *)
        emit_all_nested_cleanup env buf (indent ^ "\t");
        (match env.ret_ty with
        | Some (TyGeneric ({ node = "Option"; _ }, [ ret_inner ])) ->
            if is_nullable_ty env ret_inner then begin
              env.shared.needs_option_struct <- true;
              Printf.bprintf buf "%s\treturn rgo_none[%s]()\n" indent
                (go_type env ret_inner)
            end
            else Printf.bprintf buf "%s\treturn nil\n" indent
        | _ -> Printf.bprintf buf "%s\treturn nil\n" indent);
        Printf.bprintf buf "%s}\n" indent;
        Printf.bprintf buf "%s%s := %s.value" indent name tmp
      end
      else begin
        let tmp = fresh_tmp env "opt" in
        Printf.bprintf buf "%s := " tmp;
        gen_expr env buf indent CtxExpr inner_e;
        Printf.bprintf buf "\n%sif %s == nil {\n" indent tmp;
        (* Ownership: emit cleanup for nested scope drops before the
           early return triggered by the ? propagation. *)
        emit_all_nested_cleanup env buf (indent ^ "\t");
        (match env.ret_ty with
        | Some (TyGeneric ({ node = "Option"; _ }, [ ret_inner ])) ->
            if is_nullable_ty env ret_inner then begin
              env.shared.needs_option_struct <- true;
              Printf.bprintf buf "%s\treturn rgo_none[%s]()\n" indent
                (go_type env ret_inner)
            end
            else Printf.bprintf buf "%s\treturn nil\n" indent
        | _ -> Printf.bprintf buf "%s\treturn nil\n" indent);
        Printf.bprintf buf "%s}\n" indent;
        Printf.bprintf buf "%s%s := *%s" indent name tmp
      end
  | _ -> failwith "codegen: ? on non-Result/Option in let binding"

and needs_explicit_type (ty : Ast.ty) (init : Ast.expr) : bool =
  (* We need var decl when the init's Go default type differs from the annotated type *)
  match (ty, init) with
  | ( TyName
        {
          node = "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64";
          _;
        },
      ExprLit (LitInt _) ) ->
      true
  | TyName { node = "f32" | "f64"; _ }, ExprLit (LitFloat _) -> true
  | TyGeneric ({ node = "Vec"; _ }, _), ExprArray _ ->
      false (* handled by gen_let_init *)
  | TyGeneric ({ node = "HashMap"; _ }, _), _ -> true
  | _ -> false

and gen_let_init env buf indent ty init =
  match (ty, init) with
  | Some (TyGeneric ({ node = "Vec"; _ }, [ elem_ty ])), ExprArray [] ->
      (* Empty array with type annotation *)
      Printf.bprintf buf "[]%s{}" (go_type env elem_ty)
  | _, ExprArray [] -> Buffer.add_string buf "nil"
  | _ -> gen_expr env buf indent CtxExpr init

(* Generate ? as a statement (allows multi-line expansion) *)
and gen_question_stmt env buf indent e =
  let expr_ty = infer_expr_type env e in
  match expr_ty with
  | Some (TyGeneric ({ node = "Result"; _ }, _)) ->
      if is_flat_result_ty (Option.get expr_ty) then begin
        let tmp_val = fresh_tmp env "val" in
        let tmp_err = fresh_tmp env "err" in
        Printf.bprintf buf "%s, %s := " tmp_val tmp_err;
        gen_expr env buf indent CtxExpr e;
        Buffer.add_char buf '\n';
        Printf.bprintf buf "%sif %s != nil {\n" indent tmp_err;
        (* Ownership: emit cleanup for nested scope drops before the
           early return triggered by the ? propagation. *)
        emit_all_nested_cleanup env buf (indent ^ "\t");
        (match env.ret_ty with
        | Some (TyGeneric ({ node = "Result"; _ }, [ ok_ty; _err_ty ]))
          when should_flatten_result_return env ->
            Printf.bprintf buf "%s\treturn %s, %s\n" indent
              (go_zero_value env ok_ty) tmp_err
        | Some (TyGeneric ({ node = "Result"; _ }, [ ok_ty; err_ty ])) ->
            env.shared.needs_result_struct <- true;
            Printf.bprintf buf "%s\treturn Result[%s, %s]{err: %s}\n" indent
              (go_type env ok_ty) (go_type env err_ty) tmp_err
        | _ -> Printf.bprintf buf "%s\treturn %s\n" indent tmp_err);
        Printf.bprintf buf "%s}\n" indent;
        Printf.bprintf buf "%s_ = %s" indent tmp_val
      end
      else begin
        let tmp_res = fresh_tmp env "res" in
        Printf.bprintf buf "%s := " tmp_res;
        gen_expr env buf indent CtxExpr e;
        Buffer.add_char buf '\n';
        Printf.bprintf buf "%sif !%s.ok {\n" indent tmp_res;
        (* Ownership: emit cleanup for nested scope drops before the
           early return triggered by the ? propagation. *)
        emit_all_nested_cleanup env buf (indent ^ "\t");
        (match env.ret_ty with
        | Some (TyGeneric ({ node = "Result"; _ }, [ ok_ty; _err_ty ]))
          when should_flatten_result_return env ->
            Printf.bprintf buf "%s\treturn %s, %s\n" indent
              (go_zero_value env ok_ty) (tmp_res ^ ".err")
        | Some (TyGeneric ({ node = "Result"; _ }, [ ok_ty; err_ty ])) ->
            env.shared.needs_result_struct <- true;
            Printf.bprintf buf "%s\treturn Result[%s, %s]{err: %s}\n" indent
              (go_type env ok_ty) (go_type env err_ty) (tmp_res ^ ".err")
        | _ -> Printf.bprintf buf "%s\treturn %s\n" indent (tmp_res ^ ".err"));
        Printf.bprintf buf "%s}\n" indent;
        Printf.bprintf buf "%s_ = %s.value" indent tmp_res
      end
  | _ -> gen_expr env buf indent CtxStmt (ExprQuestion e)

and gen_function_body env buf indent body _ret_ty =
  let inner = push_scope ~is_function:true env in
  let has_return_type =
    match env.ret_ty with
    | None -> false
    | Some (TyName { node = "void"; _ }) -> false
    | _ -> true
  in
  let fold_stmts env stmts =
    List.fold_left
      (fun env s ->
        let needs_stmt_indent =
          match s with
          | StmtExpr (ExprReturn _ | ExprBreak | ExprContinue) -> false
          | _ -> true
        in
        if needs_stmt_indent then Buffer.add_string buf indent;
        let env = gen_stmt env buf indent s in
        Buffer.add_char buf '\n';
        env)
      env stmts
  in
  match body.final_expr with
  | Some e ->
      let inner = fold_stmts inner body.stmts in
      gen_final_expr_as_return inner buf indent e
  | None ->
      (* Check if last stmt is an expr that should be returned *)
      let stmts = body.stmts in
      let n = List.length stmts in
      if has_return_type && n > 0 then begin
        let front = List.filteri (fun i _ -> i < n - 1) stmts in
        let last = List.nth stmts (n - 1) in
        let inner = fold_stmts inner front in
        match last with
        | StmtExpr e -> gen_final_expr_as_return inner buf indent e
        | s ->
            Buffer.add_string buf indent;
            ignore (gen_stmt inner buf indent s);
            Buffer.add_char buf '\n'
      end
      else ignore (fold_stmts inner stmts)

(* ---------- top-level items ---------- *)

let rec gen_fn_decl env buf (fd : fn_decl) =
  let name = go_fn_name fd.fn_pub fd.fn_name.node in
  let generics =
    go_generics_decl ~self_ref_traits:env.shared.self_ref_traits fd.fn_generics
  in
  let fn_env =
    {
      env with
      ret_ty = fd.fn_ret;
      type_params =
        List.map (fun (tp : type_param) -> tp.tp_name.node) fd.fn_generics
        @ env.type_params;
    }
  in
  let fn_env = push_scope ~is_function:true fn_env in
  (* Add params to scope *)
  let fn_env =
    List.fold_left
      (fun e (p : param) ->
        add_value p.p_name.node (Some p.p_ty) ~is_mut:p.p_mut e)
      fn_env fd.fn_params
  in
  Printf.bprintf buf "func %s%s(" name generics;
  List.iteri
    (fun i (p : param) ->
      if i > 0 then Buffer.add_string buf ", ";
      Buffer.add_string buf (escape_ident p.p_name.node);
      Buffer.add_string buf " ";
      Buffer.add_string buf (go_type fn_env p.p_ty))
    fd.fn_params;
  Buffer.add_char buf ')';
  Buffer.add_string buf (go_ret_sig fn_env fd.fn_ret);
  Buffer.add_string buf " {\n";
  (* Ownership: emit defer cleanup for Drop-type by-value parameters *)
  let fn_env =
    List.fold_left
      (fun e (p : param) ->
        if is_drop_type_cg e p.p_ty then begin
          let esc = escape_ident p.p_name.node in
          let guard = fresh_tmp e "live" in
          Codegen_ownership.emit_param_drop_defer buf ~guard ~binding:esc;
          { e with drop_guards = SMap.add p.p_name.node guard e.drop_guards }
        end
        else e)
      fn_env fd.fn_params
  in
  gen_fn_body fn_env buf fd.fn_body fd.fn_ret;
  Buffer.add_string buf "}\n"

and gen_fn_body env buf body ret_ty = gen_function_body env buf "\t" body ret_ty

and gen_struct_decl env buf s_name s_generics s_fields =
  let generics = go_generics_decl s_generics in
  Printf.bprintf buf "type %s%s struct {\n" s_name.node generics;
  (* Compute max field name length for alignment *)
  let names =
    List.map (fun (f : field) -> go_field_name f.fd_pub f.fd_name.node) s_fields
  in
  let max_len =
    List.fold_left (fun acc n -> max acc (String.length n)) 0 names
  in
  List.iter2
    (fun (f : field) fname ->
      let padding = String.make (max_len - String.length fname + 1) ' ' in
      Printf.bprintf buf "\t%s%s%s\n" fname padding (go_type env f.fd_ty))
    s_fields names;
  Buffer.add_string buf "}\n"

and gen_enum_decl env buf e_name e_generics e_variants _impl_methods =
  let generics_decl = go_generics_decl e_generics in
  let _generics_use = go_generics_use e_generics in
  (* Sealed interface *)
  Printf.bprintf buf "type %s%s interface {\n" e_name.node generics_decl;
  Printf.bprintf buf "\tis%s()\n" e_name.node;
  (* Add method signatures from impl blocks.
     Build a self_type so TySelf resolves to the concrete enum type. *)
  let self_enum_ty =
    match e_generics with
    | [] -> Ast.TyName e_name
    | _ ->
        Ast.TyGeneric
          ( e_name,
            List.map (fun (tp : type_param) -> Ast.TyName tp.tp_name) e_generics
          )
  in
  let iface_env =
    {
      env with
      self_type = Some self_enum_ty;
      self_type_name = Some e_name.node;
    }
  in
  (match SMap.find_opt e_name.node env.impls with
  | Some ii ->
      SMap.iter
        (fun mname mi ->
          let is_pub = true in
          let go_name = go_method_name is_pub mname in
          Printf.bprintf buf "\t%s(" go_name;
          List.iteri
            (fun i (p : param) ->
              if i > 0 then Buffer.add_string buf ", ";
              Buffer.add_string buf (go_type iface_env p.p_ty))
            mi.mi_params;
          Buffer.add_char buf ')';
          (match mi.mi_ret with
          | Some t -> Printf.bprintf buf " %s" (go_type iface_env t)
          | None -> ());
          Buffer.add_char buf '\n')
        ii.ii_methods
  | None -> ());
  Buffer.add_string buf "}\n";
  (* Variant types *)
  let generics_use = go_generics_use e_generics in
  List.iter
    (fun (v : variant) ->
      let vtype = e_name.node ^ v.var_name.node in
      (* For generic enums, variant structs carry the same type params.
         Unit variants also need generics when the enum has self-referential
         methods (e.g. Clone() -> Self) that require the type params. *)
      let needs_generics = e_generics <> [] in
      let vtype_decl =
        if needs_generics then vtype ^ generics_decl else vtype
      in
      let vtype_recv = if needs_generics then vtype ^ generics_use else vtype in
      (match v.var_fields with
      | None -> Printf.bprintf buf "\ntype %s struct{}\n" vtype_decl
      | Some (TupleFields tys) ->
          Printf.bprintf buf "\ntype %s struct {\n" vtype_decl;
          let n = List.length tys in
          let max_field_len =
            String.length (Printf.sprintf "Field%d" (n - 1))
          in
          List.iteri
            (fun i t ->
              let fname = Printf.sprintf "Field%d" i in
              let padding =
                String.make (max_field_len - String.length fname + 1) ' '
              in
              Printf.bprintf buf "\t%s%s%s\n" fname padding (go_type env t))
            tys;
          Buffer.add_string buf "}\n"
      | Some (StructFields fields) ->
          Printf.bprintf buf "\ntype %s struct {\n" vtype_decl;
          let names =
            List.map (fun (f : field) -> capitalize f.fd_name.node) fields
          in
          let max_len =
            List.fold_left (fun acc n -> max acc (String.length n)) 0 names
          in
          List.iter2
            (fun (f : field) fname ->
              let padding =
                String.make (max_len - String.length fname + 1) ' '
              in
              Printf.bprintf buf "\t%s%s%s\n" fname padding
                (go_type env f.fd_ty))
            fields names;
          Buffer.add_string buf "}\n");
      Printf.bprintf buf "\nfunc (%s) is%s() {}\n" vtype_recv e_name.node)
    e_variants

and gen_impl_methods ?(is_trait_impl = false) env buf type_name generics_decl
    _generics_use items is_enum impl_type_params =
  let self_type_name =
    match type_name with
    | TyName n -> n.node
    | TyGeneric (n, _) -> n.node
    | _ -> "unknown"
  in
  let impl_tp_names =
    List.map (fun (tp : type_param) -> tp.tp_name.node) impl_type_params
  in
  let env =
    {
      env with
      self_type_name = Some self_type_name;
      self_type = Some type_name;
      impl_type_params;
      type_params = impl_tp_names @ env.type_params;
    }
  in
  (* For trait impls, force methods to be exported (capitalized) *)
  let items =
    if is_trait_impl then
      List.map (fun (fd : fn_decl) -> { fd with fn_pub = true }) items
    else items
  in
  List.iter
    (fun (fd : fn_decl) ->
      match fd.fn_self with
      | Some self_param when is_enum ->
          gen_enum_method env buf self_type_name generics_decl fd self_param
      | Some self_param ->
          (* For trait impls, &self uses value receiver for interface satisfaction *)
          let effective_self =
            if is_trait_impl then
              match self_param with SelfRef -> SelfValue | other -> other
            else self_param
          in
          gen_struct_method env buf type_name generics_decl fd effective_self
      | None -> gen_assoc_fn env buf self_type_name generics_decl fd)
    items

and gen_struct_method env buf type_name _generics_decl fd self_param =
  let self_type_name =
    match type_name with
    | TyName n -> n.node
    | TyGeneric (n, _) -> n.node
    | _ -> "unknown"
  in
  let receiver_type = go_type env type_name in
  let is_ptr =
    match self_param with SelfRef | SelfMutRef -> true | SelfValue -> false
  in
  let recv_str =
    if is_ptr then Printf.sprintf "(self *%s)" receiver_type
    else Printf.sprintf "(self %s)" receiver_type
  in
  let method_name = go_method_name fd.fn_pub fd.fn_name.node in
  let fn_generics =
    go_generics_decl ~self_ref_traits:env.shared.self_ref_traits fd.fn_generics
  in
  let fn_env =
    {
      env with
      ret_ty = fd.fn_ret;
      self_type_name = Some self_type_name;
      type_params =
        List.map (fun (tp : type_param) -> tp.tp_name.node) fd.fn_generics
        @ env.type_params;
    }
  in
  let fn_env = push_scope ~is_function:true fn_env in
  let fn_env =
    add_value "self" (Some type_name)
      ~is_mut:(match self_param with SelfMutRef -> true | _ -> false)
      fn_env
  in
  let fn_env =
    List.fold_left
      (fun e (p : param) ->
        add_value p.p_name.node (Some p.p_ty) ~is_mut:p.p_mut e)
      fn_env fd.fn_params
  in
  Printf.bprintf buf "\nfunc %s %s%s(" recv_str method_name fn_generics;
  List.iteri
    (fun i (p : param) ->
      if i > 0 then Buffer.add_string buf ", ";
      Buffer.add_string buf (escape_ident p.p_name.node);
      Buffer.add_string buf " ";
      Buffer.add_string buf (go_type fn_env p.p_ty))
    fd.fn_params;
  Buffer.add_char buf ')';
  Buffer.add_string buf (go_ret_sig fn_env fd.fn_ret);
  Buffer.add_string buf " {\n";
  (* Ownership: emit defer cleanup for Drop-type by-value parameters *)
  let fn_env =
    List.fold_left
      (fun e (p : param) ->
        if is_drop_type_cg e p.p_ty then begin
          let esc = escape_ident p.p_name.node in
          let guard = fresh_tmp e "live" in
          Codegen_ownership.emit_param_drop_defer buf ~guard ~binding:esc;
          { e with drop_guards = SMap.add p.p_name.node guard e.drop_guards }
        end
        else e)
      fn_env fd.fn_params
  in
  gen_fn_body fn_env buf fd.fn_body fd.fn_ret;
  Buffer.add_string buf "}\n"

and gen_enum_method env buf enum_name _generics_decl fd _self_param =
  let method_name = go_method_name fd.fn_pub fd.fn_name.node in
  let helper_name =
    uncapitalize enum_name ^ capitalize fd.fn_name.node ^ "Impl"
  in
  (* Determine if the enum is generic from the codegen env *)
  let enum_tparams =
    match SMap.find_opt enum_name env.enums with
    | Some ei -> ei.ei_tparams
    | None -> []
  in
  let helper_generics_decl =
    match enum_tparams with
    | [] -> ""
    | tps ->
        "[" ^ String.concat ", " (List.map (fun tp -> tp ^ " any") tps) ^ "]"
  in
  let helper_generics_use =
    match enum_tparams with
    | [] -> ""
    | tps -> "[" ^ String.concat ", " tps ^ "]"
  in
  (* Build concrete Self type for the enum (e.g. Wrapper[T]) *)
  let self_enum_ty =
    match enum_tparams with
    | [] -> Ast.TyName (dummy_loc enum_name)
    | _ ->
        Ast.TyGeneric
          ( dummy_loc enum_name,
            List.map (fun tp -> Ast.TyName (dummy_loc tp)) enum_tparams )
  in
  let fn_env =
    {
      env with
      ret_ty = fd.fn_ret;
      self_type_name = Some enum_name;
      self_type = Some self_enum_ty;
      type_params =
        enum_tparams
        @ List.map (fun (tp : type_param) -> tp.tp_name.node) fd.fn_generics
        @ env.type_params;
    }
  in
  let fn_env = push_scope fn_env in
  let fn_env = add_value "self" (Some self_enum_ty) ~is_mut:false fn_env in
  let fn_env =
    List.fold_left
      (fun e (p : param) ->
        add_value p.p_name.node (Some p.p_ty) ~is_mut:p.p_mut e)
      fn_env fd.fn_params
  in
  (* Shared helper *)
  Printf.bprintf buf "\nfunc %s%s(self %s%s" helper_name helper_generics_decl
    enum_name helper_generics_use;
  List.iter
    (fun (p : param) ->
      Printf.bprintf buf ", %s %s"
        (escape_ident p.p_name.node)
        (go_type fn_env p.p_ty))
    fd.fn_params;
  Buffer.add_char buf ')';
  Buffer.add_string buf (go_ret_sig fn_env fd.fn_ret);
  Buffer.add_string buf " {\n";
  gen_fn_body fn_env buf fd.fn_body fd.fn_ret;
  Buffer.add_string buf "}\n";
  (* Delegators for each variant *)
  match SMap.find_opt enum_name env.enums with
  | Some ei ->
      List.iter
        (fun (vname, _vshape) ->
          let vtype = enum_name ^ vname in
          (* All variants of generic enums carry the enum's type params *)
          let variant_has_generics = enum_tparams <> [] in
          let vtype_recv =
            if variant_has_generics then vtype ^ helper_generics_use else vtype
          in
          Printf.bprintf buf "\nfunc (self %s) %s(" vtype_recv method_name;
          List.iteri
            (fun i (p : param) ->
              if i > 0 then Buffer.add_string buf ", ";
              Buffer.add_string buf (escape_ident p.p_name.node);
              Buffer.add_string buf " ";
              Buffer.add_string buf (go_type fn_env p.p_ty))
            fd.fn_params;
          Buffer.add_char buf ')';
          Buffer.add_string buf (go_ret_sig fn_env fd.fn_ret);
          Buffer.add_string buf " {\n";
          (* For generic helper, pass type args; use return for non-void *)
          let has_return =
            match fd.fn_ret with None -> false | Some _ -> true
          in
          Printf.bprintf buf "\t%s%s%s(self"
            (if has_return then "return " else "")
            helper_name
            (if enum_tparams <> [] then helper_generics_use else "");
          List.iter
            (fun (p : param) ->
              Printf.bprintf buf ", %s" (escape_ident p.p_name.node))
            fd.fn_params;
          Buffer.add_string buf ")\n";
          Buffer.add_string buf "}\n")
        ei.ei_variants
  | None -> ()

and gen_assoc_fn env buf type_name _generics_decl fd =
  let fn_name_go = type_name ^ go_method_name fd.fn_pub fd.fn_name.node in
  (* Merge impl type params with fn-level generics for the declaration *)
  let all_generics = env.impl_type_params @ fd.fn_generics in
  let fn_generics =
    go_generics_decl ~self_ref_traits:env.shared.self_ref_traits all_generics
  in
  let fn_env =
    {
      env with
      ret_ty = fd.fn_ret;
      self_type_name = Some type_name;
      type_params =
        List.map (fun (tp : type_param) -> tp.tp_name.node) fd.fn_generics
        @ env.type_params;
    }
  in
  let fn_env = push_scope fn_env in
  let fn_env =
    List.fold_left
      (fun e (p : param) ->
        add_value p.p_name.node (Some p.p_ty) ~is_mut:p.p_mut e)
      fn_env fd.fn_params
  in
  Printf.bprintf buf "\nfunc %s%s(" fn_name_go fn_generics;
  List.iteri
    (fun i (p : param) ->
      if i > 0 then Buffer.add_string buf ", ";
      Buffer.add_string buf (escape_ident p.p_name.node);
      Buffer.add_string buf " ";
      Buffer.add_string buf (go_type fn_env p.p_ty))
    fd.fn_params;
  Buffer.add_char buf ')';
  Buffer.add_string buf (go_ret_sig fn_env fd.fn_ret);
  Buffer.add_string buf " {\n";
  gen_fn_body fn_env buf fd.fn_body fd.fn_ret;
  Buffer.add_string buf "}\n"

(* ---------- trait interface generation ---------- *)

(* Check whether a trait uses Self in any method signature *)
(* Emit a Go type for a trait method param/return, substituting Self with
   the Self type parameter name when the trait is self-referential. *)
let rec go_type_trait env (self_name : string option) (t : Ast.ty) : string =
  match t with
  | TySelf -> ( match self_name with Some s -> s | None -> "any")
  | TyRef inner -> "*" ^ go_type_trait env self_name inner
  | _ -> go_type env t

let gen_trait_decl env buf t_name t_generics t_items =
  let uses_self = trait_uses_self t_items in
  (* Build generics: include Self if self-referential, plus user-declared type params *)
  let self_param_name = if uses_self then Some "Self" else None in
  let all_generics_parts =
    (if uses_self then [ "Self any" ] else [])
    @ List.map
        (fun (tp : Ast.type_param) ->
          let bound =
            match tp.tp_bound with
            | None | Some [] -> "any"
            | Some bounds ->
                String.concat " "
                  (List.map
                     (fun (b : Ast.ident Ast.located) -> capitalize b.node)
                     bounds)
          in
          tp.tp_name.node ^ " " ^ bound)
        t_generics
  in
  let generics_str =
    if all_generics_parts = [] then ""
    else "[" ^ String.concat ", " all_generics_parts ^ "]"
  in
  Printf.bprintf buf "type %s%s interface {\n" (capitalize t_name.node)
    generics_str;
  List.iter
    (fun (ti : Ast.trait_item) ->
      let name, params, ret =
        match ti with
        | TraitFnSig sig_ -> (sig_.sig_name.node, sig_.sig_params, sig_.sig_ret)
        | TraitFnDecl fd -> (fd.fn_name.node, fd.fn_params, fd.fn_ret)
      in
      let go_name = capitalize name in
      Printf.bprintf buf "\t%s(" go_name;
      List.iteri
        (fun i (p : Ast.param) ->
          if i > 0 then Buffer.add_string buf ", ";
          Buffer.add_string buf (escape_ident p.p_name.node);
          Buffer.add_char buf ' ';
          Buffer.add_string buf (go_type_trait env self_param_name p.p_ty))
        params;
      Buffer.add_char buf ')';
      (match ret with
      | None -> ()
      | Some (TyGeneric ({ node = "Result"; _ }, [ ok; _err ]))
        when is_flat_result_ty (Option.get ret) ->
          Printf.bprintf buf " (%s, error)"
            (go_type_trait env self_param_name ok)
      | Some t -> Printf.bprintf buf " %s" (go_type_trait env self_param_name t));
      Buffer.add_char buf '\n')
    t_items;
  Buffer.add_string buf "}\n"

(* ---------- prelude generation ---------- *)

let gen_prelude buf env =
  if env.shared.needs_option_struct then begin
    Buffer.add_string buf "\ntype Option[T any] struct {\n";
    Buffer.add_string buf "\tsome  bool\n";
    Buffer.add_string buf "\tvalue T\n";
    Buffer.add_string buf "}\n";
    Buffer.add_string buf
      "\n\
       func rgo_some[T any](v T) Option[T] { return Option[T]{some: true, \
       value: v} }\n";
    Buffer.add_string buf
      "func rgo_none[T any]() Option[T]    { return Option[T]{} }\n"
  end;
  if env.shared.needs_result_struct then begin
    Buffer.add_string buf "\ntype Result[T any, E any] struct {\n";
    Buffer.add_string buf "\tok    bool\n";
    Buffer.add_string buf "\tvalue T\n";
    Buffer.add_string buf "\terr   E\n";
    Buffer.add_string buf "}\n"
  end;
  if env.shared.needs_rgo_repeat then begin
    Buffer.add_string buf "\nfunc rgo_repeat[T any](x T, n int64) []T {\n";
    Buffer.add_string buf "\tresult := make([]T, n)\n";
    Buffer.add_string buf "\tfor i := range result {\n";
    Buffer.add_string buf "\t\tresult[i] = x\n";
    Buffer.add_string buf "\t}\n";
    Buffer.add_string buf "\treturn result\n";
    Buffer.add_string buf "}\n"
  end

let gen_imports buf env =
  let imports = ref [] in
  if env.shared.needs_fmt then imports := "\"fmt\"" :: !imports;
  if env.shared.needs_errors then imports := "\"errors\"" :: !imports;
  if env.shared.needs_math then imports := "\"math\"" :: !imports;
  (* Add imports from resolved import metadata, only if actually used *)
  if env.shared.needs_imported_pkgs then
    SMap.iter
      (fun _alias (go_path, _go_pkg) ->
        imports := ("\"" ^ go_path ^ "\"") :: !imports)
      env.imported_packages;
  let sorted = List.sort String.compare !imports in
  match sorted with
  | [] -> ()
  | [ one ] -> Printf.bprintf buf "\nimport %s\n" one
  | _ ->
      Buffer.add_string buf "\nimport (\n";
      List.iter (fun s -> Printf.bprintf buf "\t%s\n" s) sorted;
      Buffer.add_string buf ")\n"

(* ---------- main entry point ---------- *)

let generate (prog : Ast.program) : string =
  let env = collect_env prog in
  (* First pass: generate the body to discover what prelude pieces are needed *)
  let body_buf = Buffer.create 2048 in
  (* Collect all impl items grouped by type for enum interface injection *)
  let impl_items_for_type : fn_decl list SMap.t ref = ref SMap.empty in
  List.iter
    (fun item ->
      match item with
      | ItemImpl { i_ty; i_items; _ }
      | ItemTraitImpl { ti_ty = i_ty; ti_items = i_items; _ } ->
          let type_name =
            match i_ty with
            | TyName n -> n.node
            | TyGeneric (n, _) -> n.node
            | _ -> "unknown"
          in
          let existing =
            match SMap.find_opt type_name !impl_items_for_type with
            | Some l -> l
            | None -> []
          in
          impl_items_for_type :=
            SMap.add type_name (existing @ i_items) !impl_items_for_type
      | _ -> ())
    prog.items;
  (* Track which items need pub info from AST *)
  let fn_pub_map : bool SMap.t ref = ref SMap.empty in
  List.iter
    (fun item ->
      match item with
      | ItemImpl { i_items; _ } | ItemTraitImpl { ti_items = i_items; _ } ->
          List.iter
            (fun (fd : fn_decl) ->
              fn_pub_map := SMap.add fd.fn_name.node fd.fn_pub !fn_pub_map)
            i_items
      | _ -> ())
    prog.items;
  (* Now generate body *)
  ignore
    (List.fold_left
       (fun env (item : Ast.item) ->
         match item with
         | ItemFn fd ->
             Buffer.add_char body_buf '\n';
             gen_fn_decl env body_buf fd;
             env
         | ItemLet { is_mut; pat; ty; init } -> (
             match pat with
             | PatBind name ->
                 let esc_name = escape_ident name.node in
                 (match (ty, init) with
                 | ( Some (TyGeneric ({ node = "Vec"; _ }, _) as t),
                     ExprCall
                       (ExprPath ({ node = "Vec"; _ }, { node = "new"; _ }), [])
                   ) ->
                     Printf.bprintf body_buf "\nvar %s = make(%s, 0)\n" esc_name
                       (go_type env t)
                 | ( Some (TyGeneric ({ node = "HashMap"; _ }, _) as t),
                     ExprCall
                       ( ExprPath ({ node = "HashMap"; _ }, { node = "new"; _ }),
                         [] ) ) ->
                     Printf.bprintf body_buf "\nvar %s = make(%s)\n" esc_name
                       (go_type env t)
                 | Some t, ExprArray [] ->
                     Printf.bprintf body_buf "\nvar %s %s\n" esc_name
                       (go_type env t)
                 | Some t, _ ->
                     Buffer.add_string body_buf "\nvar ";
                     Buffer.add_string body_buf esc_name;
                     Buffer.add_char body_buf ' ';
                     Buffer.add_string body_buf (go_type env t);
                     Buffer.add_string body_buf " = ";
                     gen_expr env body_buf "" CtxExpr init;
                     Buffer.add_char body_buf '\n'
                 | None, _ ->
                     Buffer.add_string body_buf "\nvar ";
                     Buffer.add_string body_buf esc_name;
                     Buffer.add_string body_buf " = ";
                     gen_expr env body_buf "" CtxExpr init;
                     Buffer.add_char body_buf '\n');
                 let binding_ty =
                   match ty with
                   | Some t -> Some t
                   | None -> infer_expr_type env init
                 in
                 add_value name.node binding_ty ~is_mut env
             | _ -> env)
         | ItemStruct { s_name; s_generics; s_fields; _ } ->
             Buffer.add_char body_buf '\n';
             gen_struct_decl env body_buf s_name s_generics s_fields;
             env
         | ItemEnum { e_name; e_generics; e_variants; _ } ->
             Buffer.add_char body_buf '\n';
             let impl_methods =
               match SMap.find_opt e_name.node !impl_items_for_type with
               | Some l -> l
               | None -> []
             in
             gen_enum_decl env body_buf e_name e_generics e_variants
               impl_methods;
             env
         | ItemImpl { i_generics; i_ty; i_items } ->
             let type_name_str =
               match i_ty with
               | TyName n -> n.node
               | TyGeneric (n, _) -> n.node
               | _ -> "unknown"
             in
             let is_enum = SMap.mem type_name_str env.enums in
             let gd = go_generics_decl i_generics in
             let gu = go_generics_use i_generics in
             gen_impl_methods env body_buf i_ty gd gu i_items is_enum i_generics;
             env
         | ItemTraitImpl { ti_generics; ti_trait; ti_ty; ti_items } ->
             let type_name_str =
               match ti_ty with
               | TyName n -> n.node
               | TyGeneric (n, _) -> n.node
               | _ -> "unknown"
             in
             let is_enum = SMap.mem type_name_str env.enums in
             let gd = go_generics_decl ti_generics in
             let gu = go_generics_use ti_generics in
             (* Synthesize default methods that are not explicitly provided *)
             let all_items =
               match SMap.find_opt ti_trait.node env.traits with
               | Some tic ->
                   let provided_names =
                     List.map (fun (fd : fn_decl) -> fd.fn_name.node) ti_items
                   in
                   let defaults =
                     List.filter_map
                       (fun (ti_item : Ast.trait_item) ->
                         match ti_item with
                         | TraitFnDecl fd
                           when not (List.mem fd.fn_name.node provided_names) ->
                             Some fd
                         | _ -> None)
                       tic.tic_items
                   in
                   ti_items @ defaults
               | None -> ti_items
             in
             gen_impl_methods ~is_trait_impl:true env body_buf ti_ty gd gu
               all_items is_enum ti_generics;
             env
         | ItemTrait { t_name; t_generics; t_items; _ } ->
             Buffer.add_char body_buf '\n';
             gen_trait_decl env body_buf t_name t_generics t_items;
             env)
       env prog.items);
  (* Assemble final output *)
  let out = Buffer.create 4096 in
  Buffer.add_string out "package main\n";
  gen_imports out env;
  gen_prelude out env;
  Buffer.add_buffer out body_buf;
  Buffer.contents out
