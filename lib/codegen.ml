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
type enum_info = { ei_variants : (string * variant_shape) list }
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
}

type cg_env = {
  structs : struct_info SMap.t;
  enums : enum_info SMap.t;
  fns : fn_info SMap.t;
  impls : impl_info_cg SMap.t;
  (* Per-function context *)
  ret_ty : Ast.ty option;
  self_type_name : string option;
  self_type : Ast.ty option;
  impl_type_params : type_param list;
  type_params : string list;
  (* Value bindings: name -> (ast_ty option, is_mut) *)
  values : (Ast.ty option * bool) SMap.t list;
  (* Shared mutable state *)
  shared : cg_shared;
}

let fresh_tmp env prefix =
  let n = env.shared.tmp_counter in
  env.shared.tmp_counter <- n + 1;
  Printf.sprintf "__%s_%d" prefix n

let push_scope env = { env with values = SMap.empty :: env.values }

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

(* ---------- AST type -> Go type string ---------- *)

(* Determine if an rgo type is nullable in Go (i.e., has a nil zero value) *)
let is_nullable_ty env (t : Ast.ty) : bool =
  match t with
  | TyRef _ -> true
  | TyGeneric ({ node = "Vec"; _ }, _) -> true
  | TyGeneric ({ node = "HashMap"; _ }, _) -> true
  | TyName { node = name; _ } -> SMap.mem name env.enums
  | TyGeneric ({ node = name; _ }, _) -> SMap.mem name env.enums
  | TySelf -> (
      match env.self_type_name with
      | Some n -> SMap.mem n env.enums
      | None -> false)
  | TyTuple _ -> false

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
  | TyGeneric ({ node = "Result"; _ }, [ _ok; _err ]) ->
      (* Result is handled specially in return types *)
      failwith "codegen: Result should not appear as standalone go_type"
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

(* Go return type for printing (with leading space or parens) *)
let go_ret_sig env (ret : Ast.ty option) : string =
  match ret with
  | None -> ""
  | Some (TyGeneric ({ node = "Result"; _ }, [ ok; _err ])) ->
      env.shared.needs_errors <- true;
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
  | TyGeneric ({ node = name; _ }, _) ->
      if SMap.mem name env.enums then "nil" else name ^ "{}"
  | TyRef _ -> "nil"
  | TyTuple _ -> "nil"
  | TySelf -> (
      match env.self_type with
      | Some t -> go_zero_value env t
      | None -> (
          match env.self_type_name with
          | Some n -> if SMap.mem n env.enums then "nil" else n ^ "{}"
          | None -> "nil"))

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
  | ExprCall (ExprIdent { node = "Err"; _ }, _) -> env.ret_ty
  | ExprCall (ExprIdent fn_name, _) -> (
      match SMap.find_opt fn_name.node env.fns with
      | Some fi -> fi.fi_ret
      | None -> None)
  | ExprCall (ExprPath (type_name, fn_name), _) -> (
      (* Enum variant constructor or associated function *)
      match SMap.find_opt type_name.node env.enums with
      | Some _ -> Some (TyName type_name)
      | None -> (
          match SMap.find_opt type_name.node env.impls with
          | Some ii -> (
              match SMap.find_opt fn_name.node ii.ii_assoc_fns with
              | Some fi -> fi.fi_ret
              | None -> None)
          | None -> None))
  | ExprPath (type_name, _variant) -> (
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
      | _ -> None)
  | ExprStruct (ty, _) -> Some ty
  | ExprIf (_, then_blk, _) -> infer_block_type env then_blk
  | ExprMatch (_, arms) -> (
      match arms with
      | arm :: _ -> infer_expr_type env arm.arm_expr
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
  | ExprCall (_, _) -> None

and infer_block_type env blk =
  match blk.final_expr with Some e -> infer_expr_type env e | None -> None

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
  | _ -> None

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
    }
  in
  let env =
    {
      structs = SMap.empty;
      enums = SMap.empty;
      fns = SMap.empty;
      impls = SMap.empty;
      ret_ty = None;
      self_type_name = None;
      self_type = None;
      impl_type_params = [];
      type_params = [];
      values = [ SMap.empty ];
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
          ignore e_generics;
          let ei = { ei_variants = variants } in
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
      | ItemTraitImpl { ti_ty; ti_items; _ } ->
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
          { env with impls = SMap.add type_name updated env.impls }
      | ItemTrait _ -> env)
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
let go_generics_decl (tps : type_param list) : string =
  if tps = [] then ""
  else
    let params =
      List.map
        (fun (tp : type_param) ->
          let bound =
            match tp.tp_bound with
            | None | Some [] -> "any"
            | Some bounds ->
                String.concat " " (List.map (fun b -> capitalize b.node) bounds)
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
  | ExprCall (ExprIdent { node = "Ok"; _ }, [ arg ]) ->
      (* Ok(v) in expression context -> v, error(nil)  *)
      gen_expr env buf indent CtxExpr arg
  | ExprCall (ExprIdent { node = "Err"; _ }, [ arg ]) -> (
      env.shared.needs_errors <- true;
      (* Generate errors.New for string literals, otherwise just the expr *)
      match arg with
      | ExprLit (LitString _) ->
          Buffer.add_string buf "errors.New(";
          gen_expr env buf indent CtxExpr arg;
          Buffer.add_char buf ')'
      | _ ->
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
  | ExprFieldAccess (e, field) ->
      gen_expr env buf indent CtxExpr e;
      Buffer.add_char buf '.';
      (* Field names: capitalize for pub fields *)
      let is_pub = field_is_pub env e field.node in
      Buffer.add_string buf (go_field_name is_pub field.node)
  | ExprPath (type_name, variant_name) ->
      (* Unit enum variant: EnumVariant{} *)
      Buffer.add_string buf (type_name.node ^ variant_name.node ^ "{}")
  | ExprStruct (ty, fields) -> gen_struct_literal env buf indent ty fields
  | ExprIf (cond, then_blk, else_blk) ->
      gen_if env buf indent ctx cond then_blk else_blk
  | ExprMatch (scrutinee, arms) -> gen_match env buf indent ctx scrutinee arms
  | ExprBlock blk -> gen_block_expr env buf indent ctx blk
  | ExprReturn (Some (ExprCall (ExprIdent { node = "Ok"; _ }, [ arg ]))) -> (
      match env.ret_ty with
      | Some (TyGeneric ({ node = "Result"; _ }, _)) ->
          Buffer.add_string buf "return ";
          gen_expr env buf indent CtxExpr arg;
          Buffer.add_string buf ", nil"
      | _ ->
          Buffer.add_string buf "return ";
          gen_expr env buf indent CtxExpr arg)
  | ExprReturn (Some (ExprCall (ExprIdent { node = "Err"; _ }, [ arg ]))) -> (
      match env.ret_ty with
      | Some (TyGeneric ({ node = "Result"; _ }, [ ok_ty; _ ])) -> (
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
      match env.ret_ty with
      | Some (TyGeneric ({ node = "Option"; _ }, [ inner ])) ->
          Buffer.add_string buf "return ";
          gen_some_for_type env buf indent inner arg
      | _ ->
          Buffer.add_string buf "return ";
          gen_some env buf indent arg)
  | ExprReturn (Some (ExprIdent { node = "None"; _ })) ->
      gen_return_none env buf
  | ExprReturn (Some e) ->
      Buffer.add_string buf "return ";
      gen_expr env buf indent CtxExpr e
  | ExprReturn None -> Buffer.add_string buf "return"
  | ExprBreak -> Buffer.add_string buf "break"
  | ExprContinue -> Buffer.add_string buf "continue"
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
  | ExprCast (e, ty) ->
      Buffer.add_string buf (go_type env ty);
      Buffer.add_char buf '(';
      gen_expr env buf indent CtxExpr e;
      Buffer.add_char buf ')'
  | ExprLoop (None, blk) ->
      Buffer.add_string buf "for {\n";
      gen_block_stmts env buf (indent ^ "\t") blk;
      Buffer.add_string buf indent;
      Buffer.add_char buf '}'
  | ExprLoop (Some cond, blk) ->
      Buffer.add_string buf "for ";
      gen_expr env buf indent CtxExpr cond;
      Buffer.add_string buf " {\n";
      gen_block_stmts env buf (indent ^ "\t") blk;
      Buffer.add_string buf indent;
      Buffer.add_char buf '}'
  | ExprWhile (cond, blk) ->
      Buffer.add_string buf "for ";
      gen_expr env buf indent CtxExpr cond;
      Buffer.add_string buf " {\n";
      gen_block_stmts env buf (indent ^ "\t") blk;
      Buffer.add_string buf indent;
      Buffer.add_char buf '}'
  | ExprFor (binding, iter_expr, blk) ->
      Buffer.add_string buf "for _, ";
      Buffer.add_string buf (escape_ident binding.node);
      Buffer.add_string buf " := range ";
      gen_expr env buf indent CtxExpr iter_expr;
      Buffer.add_string buf " {\n";
      let inner = push_scope env in
      let inner =
        add_value binding.node
          (infer_iter_elem_type env iter_expr)
          ~is_mut:false inner
      in
      gen_block_stmts inner buf (indent ^ "\t") blk;
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

and gen_none env buf =
  match env.ret_ty with
  | Some (TyGeneric ({ node = "Option"; _ }, [ inner ])) ->
      if is_nullable_ty env inner then begin
        env.shared.needs_option_struct <- true;
        Buffer.add_string buf ("rgo_none[" ^ go_type env inner ^ "]()")
      end
      else Buffer.add_string buf "nil"
  | _ -> Buffer.add_string buf "nil"

and gen_return_none env buf =
  match env.ret_ty with
  | Some (TyGeneric ({ node = "Option"; _ }, [ inner ])) ->
      if is_nullable_ty env inner then begin
        env.shared.needs_option_struct <- true;
        Buffer.add_string buf ("return rgo_none[" ^ go_type env inner ^ "]()")
      end
      else Buffer.add_string buf "return nil"
  | _ -> Buffer.add_string buf "return nil"

and gen_some env buf indent arg =
  match infer_expr_type env arg with
  | Some inner when is_nullable_ty env inner ->
      env.shared.needs_option_struct <- true;
      Printf.bprintf buf "rgo_some[%s](" (go_type env inner);
      gen_expr env buf indent CtxExpr arg;
      Buffer.add_char buf ')'
  | _ ->
      Buffer.add_string buf "new(";
      gen_expr env buf indent CtxExpr arg;
      Buffer.add_char buf ')'

and gen_some_for_type env buf indent (inner_ty : Ast.ty) arg =
  if is_nullable_ty env inner_ty then begin
    env.shared.needs_option_struct <- true;
    Printf.bprintf buf "rgo_some[%s](" (go_type env inner_ty);
    gen_expr env buf indent CtxExpr arg;
    Buffer.add_char buf ')'
  end
  else begin
    Buffer.add_string buf "new(";
    gen_expr env buf indent CtxExpr arg;
    Buffer.add_char buf ')'
  end

and gen_path_call env buf indent type_name fn_name args =
  (* Check if it's an enum variant constructor *)
  match SMap.find_opt type_name.node env.enums with
  | Some ei -> (
      match List.assoc_opt fn_name.node ei.ei_variants with
      | Some (VTuple _) ->
          Buffer.add_string buf (type_name.node ^ fn_name.node ^ "{");
          List.iteri
            (fun i a ->
              if i > 0 then Buffer.add_string buf ", ";
              Printf.bprintf buf "Field%d: " i;
              gen_expr env buf indent CtxExpr a)
            args;
          Buffer.add_char buf '}'
      | Some VUnit ->
          Buffer.add_string buf (type_name.node ^ fn_name.node ^ "{}")
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
    Buffer.add_string buf (Printf.sprintf "func() { %s := " tmp);
    gen_expr env buf indent CtxExpr recv;
    Buffer.add_string buf "; ";
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
  | ExprCall _ | ExprPath _ | ExprMethodCall _ | ExprStruct _ -> true
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
      let ret_ty = infer_block_type env then_blk in
      let gt = match ret_ty with Some t -> go_type env t | None -> "any" in
      Printf.bprintf buf "func() %s {\n" gt;
      let ni = indent ^ "\t" in
      Printf.bprintf buf "%sif " ni;
      gen_expr env buf ni CtxExpr cond;
      Buffer.add_string buf " {\n";
      gen_block_with_return env buf (ni ^ "\t") then_blk;
      Printf.bprintf buf "%s}" ni;
      (match else_blk with
      | Some eb ->
          Buffer.add_string buf " else {\n";
          gen_block_with_return env buf (ni ^ "\t") eb;
          Printf.bprintf buf "%s}\n" ni
      | None -> Buffer.add_char buf '\n');
      Printf.bprintf buf "%s}()" indent

and gen_match env buf indent ctx scrutinee arms =
  let enum_name =
    match infer_expr_type env scrutinee with
    | Some (TyName { node = n; _ }) -> Some n
    | Some (TyGeneric ({ node = n; _ }, _)) -> Some n
    | _ -> None
  in
  match ctx with
  | CtxStmt ->
      Buffer.add_string buf "switch __v := ";
      gen_expr env buf indent CtxExpr scrutinee;
      Buffer.add_string buf ".(type) {\n";
      List.iter
        (fun (arm : match_arm) -> gen_match_arm env buf indent enum_name arm)
        arms;
      Printf.bprintf buf "%sdefault:\n" indent;
      Printf.bprintf buf "%s\tpanic(\"unreachable: non-exhaustive match\")\n"
        indent;
      Printf.bprintf buf "%s}" indent
  | CtxExpr ->
      let ret_ty =
        match arms with
        | arm :: _ -> infer_expr_type env arm.arm_expr
        | [] -> None
      in
      let gt = match ret_ty with Some t -> go_type env t | None -> "any" in
      Printf.bprintf buf "func() %s {\n" gt;
      let ni = indent ^ "\t" in
      Printf.bprintf buf "%sswitch __v := " ni;
      gen_expr env buf ni CtxExpr scrutinee;
      Buffer.add_string buf ".(type) {\n";
      List.iter
        (fun (arm : match_arm) ->
          gen_match_arm_with_return env buf ni enum_name arm)
        arms;
      Printf.bprintf buf "%sdefault:\n" ni;
      Printf.bprintf buf "%s\tpanic(\"unreachable: non-exhaustive match\")\n" ni;
      Printf.bprintf buf "%s}\n" ni;
      Printf.bprintf buf "%s}()" indent

and gen_match_arm env buf indent enum_name (arm : match_arm) =
  let ni = indent ^ "\t" in
  match arm.arm_pat with
  | PatWild ->
      Printf.bprintf buf "%sdefault:\n" indent;
      Printf.bprintf buf "%s_ = __v\n" ni;
      gen_arm_body_stmts env buf ni arm.arm_expr
  | PatBind name ->
      Printf.bprintf buf "%sdefault:\n" indent;
      Printf.bprintf buf "%s%s := __v\n" ni (escape_ident name.node);
      gen_arm_body_stmts env buf ni arm.arm_expr
  | PatLit l ->
      Printf.bprintf buf "%scase " indent;
      gen_expr env buf indent CtxExpr (ExprLit l);
      Buffer.add_string buf ":\n";
      Printf.bprintf buf "%s_ = __v\n" ni;
      gen_arm_body_stmts env buf ni arm.arm_expr
  | PatTuple (ename, vname, pats) ->
      let case_type =
        (match enum_name with Some en -> en | None -> ename.node) ^ vname.node
      in
      Printf.bprintf buf "%scase %s:\n" indent case_type;
      if pats = [] || List.for_all (fun p -> p = PatWild) pats then
        Printf.bprintf buf "%s_ = __v\n" ni
      else gen_tuple_bindings env buf ni pats;
      gen_arm_body_stmts env buf ni arm.arm_expr
  | PatStruct (ename, vname, field_pats) ->
      let case_type =
        (match enum_name with Some en -> en | None -> ename.node) ^ vname.node
      in
      Printf.bprintf buf "%scase %s:\n" indent case_type;
      if field_pats = [] then Printf.bprintf buf "%s_ = __v\n" ni
      else gen_struct_bindings env buf ni field_pats;
      gen_arm_body_stmts env buf ni arm.arm_expr

and gen_match_arm_with_return env buf indent enum_name (arm : match_arm) =
  let ni = indent ^ "\t" in
  match arm.arm_pat with
  | PatWild ->
      Printf.bprintf buf "%sdefault:\n" indent;
      Printf.bprintf buf "%s_ = __v\n" ni;
      Printf.bprintf buf "%sreturn " ni;
      gen_expr env buf ni CtxExpr arm.arm_expr;
      Buffer.add_char buf '\n'
  | PatBind name ->
      Printf.bprintf buf "%sdefault:\n" indent;
      Printf.bprintf buf "%s%s := __v\n" ni (escape_ident name.node);
      Printf.bprintf buf "%sreturn " ni;
      gen_expr env buf ni CtxExpr arm.arm_expr;
      Buffer.add_char buf '\n'
  | PatLit l ->
      Printf.bprintf buf "%scase " indent;
      gen_expr env buf indent CtxExpr (ExprLit l);
      Buffer.add_string buf ":\n";
      Printf.bprintf buf "%s_ = __v\n" ni;
      Printf.bprintf buf "%sreturn " ni;
      gen_expr env buf ni CtxExpr arm.arm_expr;
      Buffer.add_char buf '\n'
  | PatTuple (ename, vname, pats) ->
      let case_type =
        (match enum_name with Some en -> en | None -> ename.node) ^ vname.node
      in
      Printf.bprintf buf "%scase %s:\n" indent case_type;
      if pats = [] || List.for_all (fun p -> p = PatWild) pats then
        Printf.bprintf buf "%s_ = __v\n" ni
      else gen_tuple_bindings env buf ni pats;
      Printf.bprintf buf "%sreturn " ni;
      gen_expr env buf ni CtxExpr arm.arm_expr;
      Buffer.add_char buf '\n'
  | PatStruct (ename, vname, field_pats) ->
      let case_type =
        (match enum_name with Some en -> en | None -> ename.node) ^ vname.node
      in
      Printf.bprintf buf "%scase %s:\n" indent case_type;
      if field_pats = [] then Printf.bprintf buf "%s_ = __v\n" ni
      else gen_struct_bindings env buf ni field_pats;
      Printf.bprintf buf "%sreturn " ni;
      gen_expr env buf ni CtxExpr arm.arm_expr;
      Buffer.add_char buf '\n'

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

and gen_final_expr_as_return env buf indent e =
  match e with
  | ExprReturn _ ->
      Buffer.add_string buf indent;
      gen_expr env buf indent CtxStmt e;
      Buffer.add_char buf '\n'
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
      match env.ret_ty with
      | Some (TyGeneric ({ node = "Result"; _ }, _)) ->
          Printf.bprintf buf "%sreturn " indent;
          gen_expr env buf indent CtxExpr arg;
          Buffer.add_string buf ", nil\n"
      | _ ->
          Printf.bprintf buf "%sreturn " indent;
          gen_expr env buf indent CtxExpr arg;
          Buffer.add_char buf '\n')
  | ExprCall (ExprIdent { node = "Err"; _ }, [ arg ]) -> (
      match env.ret_ty with
      | Some (TyGeneric ({ node = "Result"; _ }, [ ok_ty; _ ])) ->
          env.shared.needs_errors <- true;
          Printf.bprintf buf "%sreturn %s, " indent (go_zero_value env ok_ty);
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
          Printf.bprintf buf "%sreturn " indent;
          gen_expr env buf indent CtxExpr arg;
          Buffer.add_char buf '\n')
  | ExprCall (ExprIdent { node = "Some"; _ }, [ arg ]) -> (
      match env.ret_ty with
      | Some (TyGeneric ({ node = "Option"; _ }, [ inner ])) ->
          Printf.bprintf buf "%sreturn " indent;
          gen_some_for_type env buf indent inner arg;
          Buffer.add_char buf '\n'
      | _ ->
          Printf.bprintf buf "%sreturn " indent;
          gen_some env buf indent arg;
          Buffer.add_char buf '\n')
  | ExprIdent { node = "None"; _ } ->
      Printf.bprintf buf "%s" indent;
      gen_return_none env buf;
      Buffer.add_char buf '\n'
  | _ -> (
      match env.ret_ty with
      | None | Some (TyName { node = "void"; _ }) ->
          Buffer.add_string buf indent;
          gen_expr env buf indent CtxStmt e;
          Buffer.add_char buf '\n'
      | _ ->
          Printf.bprintf buf "%sreturn " indent;
          gen_expr env buf indent CtxExpr e;
          Buffer.add_char buf '\n')

and gen_match_as_return env buf indent scrutinee arms =
  let enum_name =
    match infer_expr_type env scrutinee with
    | Some (TyName { node = n; _ }) -> Some n
    | Some (TyGeneric ({ node = n; _ }, _)) -> Some n
    | _ -> None
  in
  Printf.bprintf buf "%sswitch __v := " indent;
  gen_expr env buf indent CtxExpr scrutinee;
  Buffer.add_string buf ".(type) {\n";
  List.iter
    (fun (arm : match_arm) ->
      gen_match_arm_with_return env buf indent enum_name arm)
    arms;
  Printf.bprintf buf "%sdefault:\n" indent;
  Printf.bprintf buf "%s\tpanic(\"unreachable: non-exhaustive match\")\n" indent;
  Printf.bprintf buf "%s}\n" indent

and gen_block_with_return env buf indent blk =
  let inner = push_scope env in
  let inner =
    List.fold_left
      (fun env s ->
        Buffer.add_string buf indent;
        let env = gen_stmt env buf indent s in
        Buffer.add_char buf '\n';
        env)
      inner blk.stmts
  in
  match blk.final_expr with
  | Some e -> gen_final_expr_as_return inner buf indent e
  | None -> ()

and gen_block_stmts env buf indent blk =
  let inner = push_scope env in
  let inner =
    List.fold_left
      (fun env s ->
        Buffer.add_string buf indent;
        let env = gen_stmt env buf indent s in
        Buffer.add_char buf '\n';
        env)
      inner blk.stmts
  in
  match blk.final_expr with
  | Some e ->
      Buffer.add_string buf indent;
      gen_expr inner buf indent CtxStmt e;
      Buffer.add_char buf '\n'
  | None -> ()

and gen_block_expr env buf indent ctx blk =
  match ctx with
  | CtxStmt -> gen_block_stmts env buf indent blk
  | CtxExpr ->
      let ret_ty = infer_block_type env blk in
      let gt = match ret_ty with Some t -> go_type env t | None -> "any" in
      Printf.bprintf buf "func() %s {\n" gt;
      gen_block_with_return env buf (indent ^ "\t") blk;
      Printf.bprintf buf "%s}()" indent

and gen_question env buf indent e =
  let expr_ty = infer_expr_type env e in
  match expr_ty with
  | Some (TyGeneric ({ node = "Result"; _ }, _)) ->
      (* Result ? -> emit as inline expression that produces the unwrapped value *)
      (* This is tricky in expression position; we emit as multi-line in gen_stmt *)
      (* For expression context, use a temp variable approach *)
      let tmp_val = fresh_tmp env "val" in
      let tmp_err = fresh_tmp env "err" in
      Printf.bprintf buf "func() %s {\n"
        (match expr_ty with
        | Some (TyGeneric ({ node = "Result"; _ }, [ ok_ty; _ ])) ->
            go_type env ok_ty
        | _ -> "any");
      let ni = indent ^ "\t" in
      Printf.bprintf buf "%s%s, %s := " ni tmp_val tmp_err;
      gen_expr env buf ni CtxExpr e;
      Buffer.add_char buf '\n';
      Printf.bprintf buf "%sif %s != nil {\n" ni tmp_err;
      (match env.ret_ty with
      | Some (TyGeneric ({ node = "Result"; _ }, [ ok_ty; _ ])) ->
          Printf.bprintf buf "%s\treturn %s, %s\n" ni (go_zero_value env ok_ty)
            tmp_err
      | _ -> Printf.bprintf buf "%s\treturn %s\n" ni tmp_err);
      Printf.bprintf buf "%s}\n" ni;
      Printf.bprintf buf "%sreturn %s\n" ni tmp_val;
      Printf.bprintf buf "%s}()" indent
  | Some (TyGeneric ({ node = "Option"; _ }, [ inner ])) ->
      if is_nullable_ty env inner then begin
        env.shared.needs_option_struct <- true;
        let tmp = fresh_tmp env "opt" in
        Printf.bprintf buf "func() %s {\n" (go_type env inner);
        let ni = indent ^ "\t" in
        Printf.bprintf buf "%s%s := " ni tmp;
        gen_expr env buf ni CtxExpr e;
        Buffer.add_char buf '\n';
        Printf.bprintf buf "%sif !%s.some {\n" ni tmp;
        (match env.ret_ty with
        | Some (TyGeneric ({ node = "Option"; _ }, [ ret_inner ])) ->
            if is_nullable_ty env ret_inner then
              Printf.bprintf buf "%s\treturn rgo_none[%s]()\n" ni
                (go_type env ret_inner)
            else Printf.bprintf buf "%s\treturn nil\n" ni
        | _ -> Printf.bprintf buf "%s\treturn nil\n" ni);
        Printf.bprintf buf "%s}\n" ni;
        Printf.bprintf buf "%sreturn %s.value\n" ni tmp;
        Printf.bprintf buf "%s}()" indent
      end
      else begin
        let tmp = fresh_tmp env "opt" in
        Printf.bprintf buf "func() %s {\n" (go_type env inner);
        let ni = indent ^ "\t" in
        Printf.bprintf buf "%s%s := " ni tmp;
        gen_expr env buf ni CtxExpr e;
        Buffer.add_char buf '\n';
        Printf.bprintf buf "%sif %s == nil {\n" ni tmp;
        (match env.ret_ty with
        | Some (TyGeneric ({ node = "Option"; _ }, [ ret_inner ])) ->
            if is_nullable_ty env ret_inner then begin
              env.shared.needs_option_struct <- true;
              Printf.bprintf buf "%s\treturn rgo_none[%s]()\n" ni
                (go_type env ret_inner)
            end
            else Printf.bprintf buf "%s\treturn nil\n" ni
        | _ -> Printf.bprintf buf "%s\treturn nil\n" ni);
        Printf.bprintf buf "%s}\n" ni;
        Printf.bprintf buf "%sreturn *%s\n" ni tmp;
        Printf.bprintf buf "%s}()" indent
      end
  | _ -> failwith "codegen: ? on non-Result/Option"

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

and gen_stmt env buf indent (s : Ast.stmt) : cg_env =
  match s with
  | StmtLet { pat = PatBind name; ty; init; is_mut } ->
      let binding_ty =
        match ty with Some t -> Some t | None -> infer_expr_type env init
      in
      let esc_name = escape_ident name.node in
      (match (ty, init) with
      | Some t, ExprArray [] ->
          Printf.bprintf buf "var %s %s" esc_name (go_type env t)
      | ( Some (TyGeneric ({ node = "HashMap"; _ }, _) as t),
          ExprCall (ExprPath ({ node = "HashMap"; _ }, { node = "new"; _ }), [])
        ) ->
          Printf.bprintf buf "%s := make(%s)" esc_name (go_type env t)
      | ( Some (TyGeneric ({ node = "Vec"; _ }, _) as t),
          ExprCall (ExprPath ({ node = "Vec"; _ }, { node = "new"; _ }), []) )
        ->
          Printf.bprintf buf "%s := make(%s, 0)" esc_name (go_type env t)
      | Some t, _ when needs_explicit_type t init ->
          Buffer.add_string buf "var ";
          Buffer.add_string buf esc_name;
          Buffer.add_char buf ' ';
          Buffer.add_string buf (go_type env t);
          Buffer.add_string buf " = ";
          gen_expr env buf indent CtxExpr init
      | _, _ when init_is_enum env init ->
          let enum_name = infer_enum_name env init in
          Printf.bprintf buf "var %s %s = " esc_name enum_name;
          gen_expr env buf indent CtxExpr init
      | _, _ when init_is_result env init ->
          gen_result_let_binding env buf indent esc_name init
      | _, ExprQuestion inner_e ->
          gen_question_let env buf indent esc_name inner_e
      | _ ->
          Buffer.add_string buf esc_name;
          Buffer.add_string buf " := ";
          gen_let_init env buf indent ty init);
      Printf.bprintf buf "\n%s_ = %s" indent esc_name;
      add_value name.node binding_ty ~is_mut env
  | StmtLet { pat = PatWild; init; _ } ->
      Buffer.add_string buf "_ = ";
      gen_expr env buf indent CtxExpr init;
      env
  | StmtLet _ -> failwith "codegen: unsupported let pattern"
  | StmtExpr (ExprQuestion e) ->
      gen_question_stmt env buf indent e;
      env
  | StmtExpr (ExprReturn _ as ret) ->
      gen_expr env buf indent CtxStmt ret;
      env
  | StmtExpr e ->
      gen_expr env buf indent CtxStmt e;
      env

and init_is_enum env (init : Ast.expr) : bool =
  match init with
  | ExprPath (type_name, _) -> SMap.mem type_name.node env.enums
  | ExprCall (ExprPath (type_name, _), _) -> SMap.mem type_name.node env.enums
  | _ -> false

and infer_enum_name _env (init : Ast.expr) : string =
  match init with
  | ExprPath (type_name, _) -> type_name.node
  | ExprCall (ExprPath (type_name, _), _) -> type_name.node
  | _ -> "any"

and init_is_result env (init : Ast.expr) : bool =
  match infer_expr_type env init with
  | Some (TyGeneric ({ node = "Result"; _ }, _)) -> true
  | _ -> false

and gen_result_let_binding env buf indent name init =
  let err_name = fresh_tmp env "err" in
  Printf.bprintf buf "%s, %s := " name err_name;
  gen_expr env buf indent CtxExpr init;
  Printf.bprintf buf "\n%s_ = %s" indent err_name

and gen_question_let env buf indent name inner_e =
  let expr_ty = infer_expr_type env inner_e in
  match expr_ty with
  | Some (TyGeneric ({ node = "Result"; _ }, [ _ok_ty; _ ])) ->
      let err_name = fresh_tmp env "err" in
      Printf.bprintf buf "%s, %s := " name err_name;
      gen_expr env buf indent CtxExpr inner_e;
      Printf.bprintf buf "\n%sif %s != nil {\n" indent err_name;
      (match env.ret_ty with
      | Some (TyGeneric ({ node = "Result"; _ }, [ ok_ty; _ ])) ->
          Printf.bprintf buf "%s\treturn %s, %s\n" indent
            (go_zero_value env ok_ty) err_name
      | _ -> Printf.bprintf buf "%s\treturn %s\n" indent err_name);
      Printf.bprintf buf "%s}" indent
  | Some (TyGeneric ({ node = "Option"; _ }, [ inner ])) ->
      if is_nullable_ty env inner then begin
        env.shared.needs_option_struct <- true;
        let tmp = fresh_tmp env "opt" in
        Printf.bprintf buf "%s := " tmp;
        gen_expr env buf indent CtxExpr inner_e;
        Printf.bprintf buf "\n%sif !%s.some {\n" indent tmp;
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
      let tmp_val = fresh_tmp env "val" in
      let tmp_err = fresh_tmp env "err" in
      Printf.bprintf buf "%s, %s := " tmp_val tmp_err;
      gen_expr env buf indent CtxExpr e;
      Buffer.add_char buf '\n';
      Printf.bprintf buf "%sif %s != nil {\n" indent tmp_err;
      (match env.ret_ty with
      | Some (TyGeneric ({ node = "Result"; _ }, [ ok_ty; _ ])) ->
          Printf.bprintf buf "%s\treturn %s, %s\n" indent
            (go_zero_value env ok_ty) tmp_err
      | _ -> Printf.bprintf buf "%s\treturn %s\n" indent tmp_err);
      Printf.bprintf buf "%s}\n" indent;
      Printf.bprintf buf "%s_ = %s" indent tmp_val
  | _ -> gen_expr env buf indent CtxStmt (ExprQuestion e)

(* ---------- top-level items ---------- *)

let rec gen_fn_decl env buf (fd : fn_decl) =
  let name = go_fn_name fd.fn_pub fd.fn_name.node in
  let generics = go_generics_decl fd.fn_generics in
  let fn_env =
    {
      env with
      ret_ty = fd.fn_ret;
      type_params =
        List.map (fun (tp : type_param) -> tp.tp_name.node) fd.fn_generics
        @ env.type_params;
    }
  in
  let fn_env = push_scope fn_env in
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
  gen_fn_body fn_env buf fd.fn_body fd.fn_ret;
  Buffer.add_string buf "}\n"

and gen_fn_body env buf body _ret_ty =
  let indent = "\t" in
  let inner = push_scope env in
  let has_return_type =
    match env.ret_ty with
    | None -> false
    | Some (TyName { node = "void"; _ }) -> false
    | _ -> true
  in
  let fold_stmts env stmts =
    List.fold_left
      (fun env s ->
        Buffer.add_string buf indent;
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
  (* Add method signatures from impl blocks *)
  (match SMap.find_opt e_name.node env.impls with
  | Some ii ->
      SMap.iter
        (fun mname mi ->
          let is_pub = true in
          (* methods in impls default to checking the fn_decl *)
          let go_name = go_method_name is_pub mname in
          Printf.bprintf buf "\t%s(" go_name;
          List.iteri
            (fun i (p : param) ->
              if i > 0 then Buffer.add_string buf ", ";
              Buffer.add_string buf (go_type env p.p_ty))
            mi.mi_params;
          Buffer.add_char buf ')';
          (match mi.mi_ret with
          | Some t -> Printf.bprintf buf " %s" (go_type env t)
          | None -> ());
          Buffer.add_char buf '\n')
        ii.ii_methods
  | None -> ());
  Buffer.add_string buf "}\n";
  (* Variant types *)
  List.iter
    (fun (v : variant) ->
      let vtype = e_name.node ^ v.var_name.node in
      (match v.var_fields with
      | None -> Printf.bprintf buf "\ntype %s struct{}\n" vtype
      | Some (TupleFields tys) ->
          Printf.bprintf buf "\ntype %s struct {\n" vtype;
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
          Printf.bprintf buf "\ntype %s struct {\n" vtype;
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
      Printf.bprintf buf "\nfunc (%s) is%s() {}\n" vtype e_name.node)
    e_variants

and gen_impl_methods env buf type_name generics_decl _generics_use items is_enum
    impl_type_params =
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
  List.iter
    (fun (fd : fn_decl) ->
      match fd.fn_self with
      | Some self_param when is_enum ->
          (* Enum method: generate shared helper + delegator for each variant *)
          gen_enum_method env buf self_type_name generics_decl fd self_param
      | Some self_param ->
          (* Struct method *)
          gen_struct_method env buf type_name generics_decl fd self_param
      | None ->
          (* Associated function: TypeNameMethodName *)
          gen_assoc_fn env buf self_type_name generics_decl fd)
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
  let fn_generics = go_generics_decl fd.fn_generics in
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
  let fn_env = push_scope fn_env in
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
  gen_fn_body fn_env buf fd.fn_body fd.fn_ret;
  Buffer.add_string buf "}\n"

and gen_enum_method env buf enum_name _generics_decl fd _self_param =
  let method_name = go_method_name fd.fn_pub fd.fn_name.node in
  let helper_name =
    uncapitalize enum_name ^ capitalize fd.fn_name.node ^ "Impl"
  in
  let fn_env =
    {
      env with
      ret_ty = fd.fn_ret;
      self_type_name = Some enum_name;
      type_params =
        List.map (fun (tp : type_param) -> tp.tp_name.node) fd.fn_generics
        @ env.type_params;
    }
  in
  let fn_env = push_scope fn_env in
  let fn_env =
    add_value "self" (Some (TyName (dummy_loc enum_name))) ~is_mut:false fn_env
  in
  let fn_env =
    List.fold_left
      (fun e (p : param) ->
        add_value p.p_name.node (Some p.p_ty) ~is_mut:p.p_mut e)
      fn_env fd.fn_params
  in
  (* Shared helper *)
  Printf.bprintf buf "\nfunc %s(self %s" helper_name enum_name;
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
        (fun (vname, _shape) ->
          let vtype = enum_name ^ vname in
          Printf.bprintf buf "\nfunc (self %s) %s(" vtype method_name;
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
          Printf.bprintf buf "\treturn %s(self" helper_name;
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
  let fn_generics = go_generics_decl all_generics in
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
  List.iter
    (fun (item : Ast.item) ->
      match item with
      | ItemFn fd ->
          Buffer.add_char body_buf '\n';
          gen_fn_decl env body_buf fd
      | ItemStruct { s_name; s_generics; s_fields; _ } ->
          Buffer.add_char body_buf '\n';
          gen_struct_decl env body_buf s_name s_generics s_fields
      | ItemEnum { e_name; e_generics; e_variants; _ } ->
          Buffer.add_char body_buf '\n';
          let impl_methods =
            match SMap.find_opt e_name.node !impl_items_for_type with
            | Some l -> l
            | None -> []
          in
          gen_enum_decl env body_buf e_name e_generics e_variants impl_methods
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
          gen_impl_methods env body_buf i_ty gd gu i_items is_enum i_generics
      | ItemTraitImpl { ti_generics; ti_ty; ti_items; _ } ->
          let type_name_str =
            match ti_ty with
            | TyName n -> n.node
            | TyGeneric (n, _) -> n.node
            | _ -> "unknown"
          in
          let is_enum = SMap.mem type_name_str env.enums in
          let gd = go_generics_decl ti_generics in
          let gu = go_generics_use ti_generics in
          gen_impl_methods env body_buf ti_ty gd gu ti_items is_enum ti_generics
      | ItemTrait _ -> () (* Traits are emitted in Phase 8 *))
    prog.items;
  (* Assemble final output *)
  let out = Buffer.create 4096 in
  Buffer.add_string out "package main\n";
  gen_imports out env;
  gen_prelude out env;
  Buffer.add_buffer out body_buf;
  Buffer.contents out
