open Ast

(* ---------- errors ---------- *)
type resolve_error = { msg : string; line : int; col : int }

exception Resolve_error of resolve_error

let error_at (span : span) fmt =
  Printf.ksprintf
    (fun msg ->
      raise
        (Resolve_error { msg; line = span.start.line; col = span.start.col }))
    fmt

(* ---------- scopes ---------- *)

(* A scope maps names to "declared" markers.  We use a simple string set
   per scope for values, and a separate one for types. *)
module SMap = Map.Make (String)

type type_info = {
  ti_variants : string list; (* enum variant names *)
  ti_fields : string list; (* struct field names *)
}

type env = {
  values : unit SMap.t list; (* stack of scopes, head = innermost *)
  types : type_info SMap.t; (* global type namespace *)
  fns : unit SMap.t; (* global function namespace *)
}

let empty_env =
  { values = [ SMap.empty ]; types = SMap.empty; fns = SMap.empty }

let push_scope env = { env with values = SMap.empty :: env.values }

let add_value (name : ident located) env =
  match env.values with
  | scope :: rest -> { env with values = SMap.add name.node () scope :: rest }
  | [] -> env

let lookup_value (name : string) env =
  List.exists (fun scope -> SMap.mem name scope) env.values

let add_type (name : ident located) (info : type_info) env =
  if SMap.mem name.node env.types then
    error_at name.span "duplicate definition of type '%s'" name.node;
  { env with types = SMap.add name.node info env.types }

let add_fn (name : ident located) env =
  if SMap.mem name.node env.fns then
    error_at name.span "duplicate definition of function '%s'" name.node;
  { env with fns = SMap.add name.node () env.fns }

let lookup_type (name : string) env = SMap.find_opt name env.types
let lookup_fn (name : string) env = SMap.mem name env.fns

(* ---------- built-in names ---------- *)

let builtin_types =
  [
    "i8";
    "i16";
    "i32";
    "i64";
    "u8";
    "u16";
    "u32";
    "u64";
    "f32";
    "f64";
    "bool";
    "str";
    "String";
    "Option";
    "Result";
    "Vec";
    "HashMap";
  ]

let builtin_fns = [ "println"; "print"; "len"; "to_string"; "panic" ]

let init_env () =
  let env = empty_env in
  let env =
    List.fold_left
      (fun e name ->
        {
          e with
          types = SMap.add name { ti_variants = []; ti_fields = [] } e.types;
        })
      env builtin_types
  in
  let env =
    List.fold_left
      (fun e name -> { e with fns = SMap.add name () e.fns })
      env builtin_fns
  in
  env

(* ---------- type resolution ---------- *)

let rec resolve_ty env (t : ty) =
  match t with
  | TyName name ->
      if Option.is_none (lookup_type name.node env) then
        error_at name.span "undefined type '%s'" name.node
  | TyGeneric (name, args) ->
      if Option.is_none (lookup_type name.node env) then
        error_at name.span "undefined type '%s'" name.node;
      List.iter (resolve_ty env) args
  | TyRef t -> resolve_ty env t
  | TyTuple ts -> List.iter (resolve_ty env) ts
  | TySelf -> () (* validated in later phases *)

(* ---------- pattern resolution ---------- *)

let rec resolve_pat env (p : pat) =
  match p with
  | PatWild -> env
  | PatBind name -> add_value name env
  | PatLit _ -> env
  | PatTuple (enum_name, variant_name, pats) ->
      resolve_enum_variant_ref env enum_name variant_name;
      List.fold_left resolve_pat env pats
  | PatStruct (enum_name, variant_name, field_pats) ->
      resolve_enum_variant_ref env enum_name variant_name;
      List.fold_left
        (fun e fp ->
          match fp.fp_pat with
          | Some p -> resolve_pat e p
          | None -> add_value fp.fp_name e)
        env field_pats

and resolve_enum_variant_ref env enum_name variant_name =
  match lookup_type enum_name.node env with
  | None -> error_at enum_name.span "undefined type '%s'" enum_name.node
  | Some ti ->
      if not (List.mem variant_name.node ti.ti_variants) then
        error_at variant_name.span "undefined variant '%s' in '%s'"
          variant_name.node enum_name.node

and resolve_path_ref env type_name member_name =
  (* Type::Member can be either an enum variant or an associated function.
     At the resolution phase we only check that the type exists.
     For enums, we also verify the variant exists.
     For structs (and other types), member_name is an associated function
     that will be validated in the type-checking phase. *)
  match lookup_type type_name.node env with
  | None -> error_at type_name.span "undefined type '%s'" type_name.node
  | Some ti ->
      (* If it's an enum with variants, check that the variant exists *)
      if ti.ti_variants <> [] then
        begin if not (List.mem member_name.node ti.ti_variants) then
          error_at member_name.span "undefined variant '%s' in '%s'"
            member_name.node type_name.node
        end
(* For structs or types without variants, member_name is an
         associated function - defer to typechecking *)

(* ---------- expression resolution ---------- *)

let rec resolve_expr env (e : expr) =
  match e with
  | ExprLit _ -> ()
  | ExprIdent name ->
      if (not (lookup_value name.node env)) && not (lookup_fn name.node env)
      then error_at name.span "undefined name '%s'" name.node
  | ExprSelf -> () (* validated in later phases *)
  | ExprUnary (_, e) -> resolve_expr env e
  | ExprBinary (_, l, r) ->
      resolve_expr env l;
      resolve_expr env r
  | ExprCall (callee, args) ->
      resolve_expr env callee;
      List.iter (resolve_expr env) args
  | ExprMethodCall (receiver, _method_name, args) ->
      resolve_expr env receiver;
      (* method resolution deferred to type checking *)
      List.iter (resolve_expr env) args
  | ExprFieldAccess (e, _field) ->
      resolve_expr env e (* field validation deferred to type checking *)
  | ExprPath (type_name, variant_name) ->
      resolve_path_ref env type_name variant_name
  | ExprStruct (ty, fields) -> resolve_struct_constructor env ty fields
  | ExprIf (cond, then_blk, else_blk) ->
      resolve_expr env cond;
      resolve_block env then_blk;
      Option.iter (resolve_block env) else_blk
  | ExprMatch (scrutinee, arms) ->
      resolve_expr env scrutinee;
      List.iter (resolve_match_arm env) arms
  | ExprBlock blk -> resolve_block env blk
  | ExprReturn e_opt -> Option.iter (resolve_expr env) e_opt
  | ExprBreak | ExprContinue -> ()
  | ExprAssign (_, lhs, rhs) ->
      resolve_expr env lhs;
      resolve_expr env rhs
  | ExprQuestion e -> resolve_expr env e
  | ExprArray elems -> List.iter (resolve_expr env) elems
  | ExprRepeat (e, count) ->
      resolve_expr env e;
      resolve_expr env count
  | ExprIndex (e, idx) ->
      resolve_expr env e;
      resolve_expr env idx
  | ExprCast (e, ty) ->
      resolve_expr env e;
      resolve_ty env ty
  | ExprLoop (cond, blk) ->
      Option.iter (resolve_expr env) cond;
      resolve_block env blk
  | ExprWhile (cond, blk) ->
      resolve_expr env cond;
      resolve_block env blk
  | ExprFor (binding, iter_expr, blk) ->
      resolve_expr env iter_expr;
      let inner = push_scope env in
      let inner = add_value binding inner in
      resolve_block inner blk

and resolve_struct_constructor env ty fields =
  (* Resolve the type name and check fields *)
  let type_name =
    match ty with
    | TyName name -> name
    | TyGeneric (name, args) ->
        List.iter (resolve_ty env) args;
        name
    | _ -> failwith "resolver: unexpected type in struct constructor"
  in
  (match lookup_type type_name.node env with
  | None -> error_at type_name.span "undefined type '%s'" type_name.node
  | Some ti ->
      (* Check fields if this is a struct type with known fields *)
      if ti.ti_fields <> [] then
        List.iter
          (fun (sf : struct_field_init) ->
            if not (List.mem sf.sf_name.node ti.ti_fields) then
              error_at sf.sf_name.span "undefined field '%s' in '%s'"
                sf.sf_name.node type_name.node)
          fields);
  List.iter (fun (sf : struct_field_init) -> resolve_expr env sf.sf_expr) fields

and resolve_match_arm env (arm : match_arm) =
  let inner = push_scope env in
  let inner = resolve_pat inner arm.arm_pat in
  resolve_expr inner arm.arm_expr

and resolve_block env (blk : block) =
  let inner = push_scope env in
  let _final_env =
    List.fold_left (fun e s -> resolve_stmt e s) inner blk.stmts
  in
  Option.iter (resolve_expr _final_env) blk.final_expr

and resolve_stmt env (s : stmt) =
  match s with
  | StmtLet { name; ty; init; _ } ->
      Option.iter (resolve_ty env) ty;
      resolve_expr env init;
      add_value name env
  | StmtExpr e ->
      resolve_expr env e;
      env

(* ---------- item resolution ---------- *)

let resolve_fn_decl env (fd : fn_decl) =
  let inner = push_scope env in
  (* Add type parameters as types *)
  let inner =
    List.fold_left
      (fun e tp ->
        {
          e with
          types =
            SMap.add tp.tp_name.node
              { ti_variants = []; ti_fields = [] }
              e.types;
        })
      inner fd.fn_generics
  in
  (* Resolve parameter types and add params as values *)
  let inner =
    List.fold_left
      (fun e (p : param) ->
        resolve_ty e p.p_ty;
        add_value p.p_name e)
      inner fd.fn_params
  in
  (* Resolve return type *)
  Option.iter (resolve_ty inner) fd.fn_ret;
  (* Resolve body *)
  resolve_block inner fd.fn_body

let collect_globals env (items : item list) =
  List.fold_left
    (fun e item ->
      match item with
      | ItemFn fd -> add_fn fd.fn_name e
      | ItemStruct { s_name; s_fields; _ } ->
          let fields = List.map (fun (f : field) -> f.fd_name.node) s_fields in
          add_type s_name { ti_variants = []; ti_fields = fields } e
      | ItemEnum { e_name; e_variants; _ } ->
          let variants =
            List.map (fun (v : variant) -> v.var_name.node) e_variants
          in
          add_type e_name { ti_variants = variants; ti_fields = [] } e
      | ItemImpl _ | ItemTraitImpl _ | ItemTrait _ -> e)
    env items

let resolve_item env (item : item) =
  match item with
  | ItemFn fd -> resolve_fn_decl env fd
  | ItemStruct { s_fields; s_generics; _ } ->
      (* Resolve field types *)
      let inner =
        List.fold_left
          (fun e tp ->
            {
              e with
              types =
                SMap.add tp.tp_name.node
                  { ti_variants = []; ti_fields = [] }
                  e.types;
            })
          env s_generics
      in
      List.iter (fun (f : field) -> resolve_ty inner f.fd_ty) s_fields
  | ItemEnum { e_variants; e_generics; _ } ->
      let inner =
        List.fold_left
          (fun e tp ->
            {
              e with
              types =
                SMap.add tp.tp_name.node
                  { ti_variants = []; ti_fields = [] }
                  e.types;
            })
          env e_generics
      in
      List.iter
        (fun (v : variant) ->
          match v.var_fields with
          | None -> ()
          | Some (TupleFields tys) -> List.iter (resolve_ty inner) tys
          | Some (StructFields fields) ->
              List.iter (fun (f : field) -> resolve_ty inner f.fd_ty) fields)
        e_variants
  | ItemImpl { i_generics; i_ty; i_items } ->
      let inner =
        List.fold_left
          (fun e tp ->
            {
              e with
              types =
                SMap.add tp.tp_name.node
                  { ti_variants = []; ti_fields = [] }
                  e.types;
            })
          env i_generics
      in
      resolve_ty inner i_ty;
      (* Add self as a value in method scopes *)
      List.iter
        (fun fd ->
          let method_env =
            if fd.fn_self <> None then
              add_value { node = "self"; span = fd.fn_name.span } inner
            else inner
          in
          resolve_fn_decl method_env fd)
        i_items
  | ItemTraitImpl { ti_generics; ti_ty; ti_items; _ } ->
      let inner =
        List.fold_left
          (fun e tp ->
            {
              e with
              types =
                SMap.add tp.tp_name.node
                  { ti_variants = []; ti_fields = [] }
                  e.types;
            })
          env ti_generics
      in
      resolve_ty inner ti_ty;
      List.iter
        (fun fd ->
          let method_env =
            if fd.fn_self <> None then
              add_value { node = "self"; span = fd.fn_name.span } inner
            else inner
          in
          resolve_fn_decl method_env fd)
        ti_items
  | ItemTrait { t_generics; t_items; _ } ->
      let inner =
        List.fold_left
          (fun e tp ->
            {
              e with
              types =
                SMap.add tp.tp_name.node
                  { ti_variants = []; ti_fields = [] }
                  e.types;
            })
          env t_generics
      in
      List.iter
        (fun ti ->
          match ti with
          | TraitFnSig sig_ ->
              List.iter (resolve_ty inner)
                (List.map (fun (p : param) -> p.p_ty) sig_.sig_params);
              Option.iter (resolve_ty inner) sig_.sig_ret
          | TraitFnDecl fd ->
              let method_env =
                if fd.fn_self <> None then
                  add_value { node = "self"; span = fd.fn_name.span } inner
                else inner
              in
              resolve_fn_decl method_env fd)
        t_items

(* ---------- main entry point ---------- *)

let resolve_exn (prog : program) : program =
  let env = init_env () in
  let env = collect_globals env prog.items in
  List.iter (resolve_item env) prog.items;
  prog

let resolve (prog : program) : (program, string) result =
  try Ok (resolve_exn prog)
  with Resolve_error { msg; line; col } ->
    Error (Printf.sprintf "%d:%d: %s" line col msg)
