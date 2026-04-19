open Ast
open Types

(* ---------- errors ---------- *)
type typecheck_error = { msg : string; line : int; col : int }

exception Typecheck_error of typecheck_error

let error_at (span : span) fmt =
  Printf.ksprintf
    (fun msg ->
      raise
        (Typecheck_error { msg; line = span.start.line; col = span.start.col }))
    fmt

(* ---------- environment ---------- *)
module SMap = Map.Make (String)

type struct_info = { si_fields : (string * ty * bool (* pub *)) list }

type enum_info = { ei_variants : (string * variant_shape) list }
and variant_shape = VUnit | VTuple of ty list | VStruct of (string * ty) list

type fn_info = { fi_params : ty list; fi_ret : ty }
type method_info = { mi_params : ty list; mi_ret : ty; mi_is_mut : bool }

type impl_info = {
  ii_methods : method_info SMap.t;
  ii_assoc_fns : fn_info SMap.t;
}

type trait_method_sig = {
  tms_self : Ast.self_param option;
  tms_params : ty list;
  tms_ret : ty;
  tms_has_default : bool; (* has a default body *)
}

type trait_info = { tr_methods : trait_method_sig SMap.t }

type env = {
  values : (ty * bool (* is_mut *)) SMap.t list;
  structs : struct_info SMap.t;
  enums : enum_info SMap.t;
  fns : fn_info SMap.t;
  impls : impl_info SMap.t; (* type name -> impl info *)
  traits : trait_info SMap.t; (* trait name -> trait info *)
  ret_ty : ty option; (* return type of current function *)
  self_ty : ty option; (* type bound to Self in current impl/trait *)
  type_params : string list; (* in-scope generic type params *)
}

let empty_env =
  {
    values = [ SMap.empty ];
    structs = SMap.empty;
    enums = SMap.empty;
    fns = SMap.empty;
    impls = SMap.empty;
    traits = SMap.empty;
    ret_ty = None;
    self_ty = None;
    type_params = [];
  }

let push_scope env = { env with values = SMap.empty :: env.values }

let add_value name ty ~is_mut env =
  match env.values with
  | scope :: rest ->
      { env with values = SMap.add name (ty, is_mut) scope :: rest }
  | [] -> env

let lookup_value name env =
  let rec go = function
    | [] -> None
    | scope :: rest -> (
        match SMap.find_opt name scope with Some v -> Some v | None -> go rest)
  in
  go env.values

(* ---------- AST type -> internal type ---------- *)
let rec resolve_ast_ty env (t : Ast.ty) : ty =
  match t with
  | TyName { node = "i8"; _ } -> TInt 8
  | TyName { node = "i16"; _ } -> TInt 16
  | TyName { node = "i32"; _ } -> TInt 32
  | TyName { node = "i64"; _ } -> TInt 64
  | TyName { node = "u8"; _ } -> TUint 8
  | TyName { node = "u16"; _ } -> TUint 16
  | TyName { node = "u32"; _ } -> TUint 32
  | TyName { node = "u64"; _ } -> TUint 64
  | TyName { node = "f32"; _ } -> TFloat 32
  | TyName { node = "f64"; _ } -> TFloat 64
  | TyName { node = "bool"; _ } -> TBool
  | TyName { node = "str"; _ } | TyName { node = "String"; _ } -> TString
  | TyName name ->
      if List.mem name.node env.type_params then TParam name.node
      else if SMap.mem name.node env.structs then TStruct (name.node, [])
      else if SMap.mem name.node env.enums then TEnum (name.node, [])
      else error_at name.span "unknown type '%s'" name.node
  | TyGeneric ({ node = "Option"; _ }, [ arg ]) ->
      TOption (resolve_ast_ty env arg)
  | TyGeneric ({ node = "Result"; _ }, [ ok; err ]) ->
      TResult (resolve_ast_ty env ok, resolve_ast_ty env err)
  | TyGeneric ({ node = "Vec"; _ }, [ arg ]) -> TVec (resolve_ast_ty env arg)
  | TyGeneric ({ node = "HashMap"; _ }, [ k; v ]) ->
      THashMap (resolve_ast_ty env k, resolve_ast_ty env v)
  | TyGeneric (name, args) ->
      let args' = List.map (resolve_ast_ty env) args in
      if SMap.mem name.node env.structs then TStruct (name.node, args')
      else if SMap.mem name.node env.enums then TEnum (name.node, args')
      else error_at name.span "unknown type '%s'" name.node
  | TyRef t -> TRef (resolve_ast_ty env t)
  | TyTuple ts -> TTuple (List.map (resolve_ast_ty env) ts)
  | TySelf -> (
      match env.self_ty with
      | Some _ -> TSelf
      | None ->
          (* We need a span but TySelf has none; use a dummy *)
          raise
            (Typecheck_error
               {
                 msg = "`Self` used outside of trait or impl context";
                 line = 0;
                 col = 0;
               }))

(* Substitute Self with the concrete type *)
let rec subst_self self_ty t =
  match t with
  | TSelf -> self_ty
  | TOption inner -> TOption (subst_self self_ty inner)
  | TResult (ok, err) -> TResult (subst_self self_ty ok, subst_self self_ty err)
  | TVec inner -> TVec (subst_self self_ty inner)
  | THashMap (k, v) -> THashMap (subst_self self_ty k, subst_self self_ty v)
  | TRef inner -> TRef (subst_self self_ty inner)
  | TTuple ts -> TTuple (List.map (subst_self self_ty) ts)
  | TStruct (n, args) -> TStruct (n, List.map (subst_self self_ty) args)
  | TEnum (n, args) -> TEnum (n, List.map (subst_self self_ty) args)
  | TFn (ps, r) -> TFn (List.map (subst_self self_ty) ps, subst_self self_ty r)
  | _ -> t

(* ---------- type compatibility ---------- *)
(* Check if a type is a numeric literal that can coerce to the target *)
let rec types_compatible expected actual =
  if expected = actual then true
  else
    match (expected, actual) with
    (* Integer literals (i64 default) coerce to any integer type *)
    | TInt _, TInt 64 -> true
    | TUint _, TInt 64 -> true
    (* Float literals (f64 default) coerce to any float type *)
    | TFloat _, TFloat 64 -> true
    (* Result and Option with compatible inner types *)
    | TResult (ok1, err1), TResult (ok2, err2) ->
        types_compatible ok1 ok2 && types_compatible err1 err2
    | TOption inner1, TOption inner2 -> types_compatible inner1 inner2
    | TVec inner1, TVec inner2 -> types_compatible inner1 inner2
    | _ -> false

let expect_type ~(span : span) ~expected ~actual =
  if not (types_compatible expected actual) then
    error_at span "type mismatch: expected %s, got %s" (show_ty expected)
      (show_ty actual)

(* ---------- literal type inference ---------- *)
let type_of_lit (l : lit) : ty =
  match l with
  | LitInt _ -> TInt 64
  | LitFloat _ -> TFloat 64
  | LitString _ -> TString
  | LitBool _ -> TBool

(* ---------- builtin function types ---------- *)
let builtin_fn_type name =
  match name with
  | "println" -> Some { fi_params = [ TString ]; fi_ret = TVoid }
  | "print" -> Some { fi_params = [ TString ]; fi_ret = TVoid }
  | "len" -> None (* handled as method call *)
  | "to_string" -> None
  | "panic" -> Some { fi_params = [ TString ]; fi_ret = TVoid }
  | _ -> None

(* ---------- expression type checking ---------- *)

(* dummy span for generated positions *)
let dummy_span = { start = { line = 0; col = 0 }; stop = { line = 0; col = 0 } }

(* Get span from an expression - best effort *)
let expr_span (e : expr) : span =
  match e with
  | ExprIdent id -> id.span
  | ExprPath (id, _) -> id.span
  | ExprStruct (TyName id, _) | ExprStruct (TyGeneric (id, _), _) -> id.span
  | _ -> dummy_span

let rec check_expr env (e : expr) : ty =
  match e with
  | ExprLit l -> type_of_lit l
  | ExprIdent name -> (
      match name.node with
      | "None" -> (
          (* None is Option<_>, infer from context *)
          match env.ret_ty with
          | Some (TOption _ as t) -> t
          | _ -> TOption TVoid)
      | _ -> (
          match lookup_value name.node env with
          | Some (ty, _) -> ty
          | None -> (
              (* Check if it's a function name *)
              match SMap.find_opt name.node env.fns with
              | Some fi -> TFn (fi.fi_params, fi.fi_ret)
              | None -> (
                  match builtin_fn_type name.node with
                  | Some fi -> TFn (fi.fi_params, fi.fi_ret)
                  | None -> error_at name.span "undefined name '%s'" name.node))
          ))
  | ExprSelf -> (
      match lookup_value "self" env with
      | Some (ty, _) -> ty
      | None -> error_at dummy_span "`self` used outside of method context")
  | ExprUnary (Neg, e) -> (
      let t = check_expr env e in
      let span = expr_span e in
      match t with
      | TInt _ | TFloat _ -> t
      | _ -> error_at span "cannot negate type %s" (show_ty t))
  | ExprUnary (Not, e) ->
      let t = check_expr env e in
      let span = expr_span e in
      expect_type ~span ~expected:TBool ~actual:t;
      TBool
  | ExprBinary (op, l, r) -> check_binop env op l r
  | ExprCall (callee, args) -> check_call env callee args
  | ExprMethodCall (receiver, method_name, args) ->
      check_method_call env receiver method_name args
  | ExprFieldAccess (e, field) -> check_field_access env e field
  | ExprPath (type_name, member) -> check_path env type_name member
  | ExprStruct (ty, fields) -> check_struct_literal env ty fields
  | ExprIf (cond, then_blk, else_blk) -> check_if env cond then_blk else_blk
  | ExprMatch (scrutinee, arms) -> check_match env scrutinee arms
  | ExprBlock blk -> check_block env blk
  | ExprReturn e_opt -> check_return env e_opt
  | ExprBreak -> TVoid
  | ExprContinue -> TVoid
  | ExprAssign (_, lhs, rhs) -> check_assign env lhs rhs
  | ExprQuestion e -> check_question env e
  | ExprArray elems -> check_array env elems
  | ExprRepeat (elem, count) -> check_repeat env elem count
  | ExprIndex (e, idx) -> check_index env e idx
  | ExprCast (e, ty) ->
      let _ = check_expr env e in
      resolve_ast_ty env ty
  | ExprLoop (cond, blk) ->
      Option.iter
        (fun c ->
          let ct = check_expr env c in
          expect_type ~span:(expr_span c) ~expected:TBool ~actual:ct)
        cond;
      let _ = check_block env blk in
      TVoid
  | ExprWhile (cond, blk) ->
      let ct = check_expr env cond in
      expect_type ~span:(expr_span cond) ~expected:TBool ~actual:ct;
      let _ = check_block env blk in
      TVoid
  | ExprFor (binding, iter_expr, blk) -> check_for env binding iter_expr blk

and check_binop env op l r =
  let lt = check_expr env l in
  let rt = check_expr env r in
  let span = expr_span l in
  match op with
  | Add | Sub | Mul | Div | Mod -> (
      (* For string concatenation with Add *)
      match (op, lt, rt) with
      | Add, TString, TString -> TString
      | _, _, _ -> (
          expect_type ~span ~expected:lt ~actual:rt;
          match lt with
          | TInt _ | TUint _ | TFloat _ -> lt
          | _ -> error_at span "arithmetic on non-numeric type %s" (show_ty lt))
      )
  | Eq | Ne | Lt | Gt | Le | Ge ->
      expect_type ~span ~expected:lt ~actual:rt;
      TBool
  | And | Or ->
      expect_type ~span ~expected:TBool ~actual:lt;
      expect_type ~span:(expr_span r) ~expected:TBool ~actual:rt;
      TBool

and check_call env callee args =
  match callee with
  | ExprIdent name -> (
      let arg_types = List.map (check_expr env) args in
      (* Check builtins with flexible arity *)
      match name.node with
      | "println" | "print" ->
          let _ = arg_types in
          TVoid
      | "panic" ->
          let _ = arg_types in
          TVoid
      | "Some" -> (
          match arg_types with
          | [ inner ] -> TOption inner
          | _ -> error_at name.span "Some expects exactly 1 argument")
      | "Ok" -> (
          match (arg_types, env.ret_ty) with
          | [ ok_ty ], Some (TResult (_, err_ty)) -> TResult (ok_ty, err_ty)
          | [ ok_ty ], _ -> TResult (ok_ty, TString)
          | _ -> error_at name.span "Ok expects exactly 1 argument")
      | "Err" -> (
          match (arg_types, env.ret_ty) with
          | [ err_ty ], Some (TResult (ok_ty, _)) -> TResult (ok_ty, err_ty)
          | [ err_ty ], _ -> TResult (TVoid, err_ty)
          | _ -> error_at name.span "Err expects exactly 1 argument")
      | _ -> (
          match SMap.find_opt name.node env.fns with
          | Some fi ->
              check_fn_args ~span:name.span ~name:name.node fi.fi_params
                arg_types;
              maybe_subst_self env fi.fi_ret
          | None -> (
              match builtin_fn_type name.node with
              | Some fi ->
                  check_fn_args ~span:name.span ~name:name.node fi.fi_params
                    arg_types;
                  fi.fi_ret
              | None -> (
                  (* Could be a variable holding a function value *)
                  match lookup_value name.node env with
                  | Some (TFn (params, ret), _) ->
                      check_fn_args ~span:name.span ~name:name.node params
                        arg_types;
                      ret
                  | Some _ ->
                      error_at name.span "'%s' is not callable" name.node
                  | None ->
                      error_at name.span "undefined function '%s'" name.node))))
  | ExprPath (type_name, fn_name) ->
      (* Associated function call: Type::method(...) *)
      let arg_types = List.map (check_expr env) args in
      check_assoc_fn_call env type_name fn_name arg_types
  | _ -> (
      let ct = check_expr env callee in
      let arg_types = List.map (check_expr env) args in
      match ct with
      | TFn (params, ret) ->
          check_fn_args ~span:(expr_span callee) ~name:"<expr>" params arg_types;
          ret
      | _ -> error_at (expr_span callee) "expression is not callable")

and check_fn_args ~span ~name expected actual =
  let expected_len = List.length expected in
  let actual_len = List.length actual in
  if expected_len <> actual_len then
    error_at span "function '%s' expects %d argument(s), got %d" name
      expected_len actual_len;
  List.iter2
    (fun exp act -> expect_type ~span ~expected:exp ~actual:act)
    expected actual

and check_assoc_fn_call env type_name fn_name arg_types =
  (* Check for enum variant constructors first *)
  match SMap.find_opt type_name.node env.enums with
  | Some ei -> (
      match List.assoc_opt fn_name.node ei.ei_variants with
      | Some (VTuple field_tys) ->
          let expected_len = List.length field_tys in
          let actual_len = List.length arg_types in
          if expected_len <> actual_len then
            error_at fn_name.span "variant '%s::%s' expects %d field(s), got %d"
              type_name.node fn_name.node expected_len actual_len;
          List.iter2
            (fun exp act ->
              expect_type ~span:fn_name.span ~expected:exp ~actual:act)
            field_tys arg_types;
          TEnum (type_name.node, [])
      | Some VUnit ->
          if arg_types <> [] then
            error_at fn_name.span "unit variant '%s::%s' takes no arguments"
              type_name.node fn_name.node;
          TEnum (type_name.node, [])
      | Some (VStruct _) ->
          error_at fn_name.span "struct variant '%s::%s' must use named fields"
            type_name.node fn_name.node
      | None -> check_impl_assoc_fn env type_name fn_name arg_types)
  | None -> check_impl_assoc_fn env type_name fn_name arg_types

and check_impl_assoc_fn env type_name fn_name arg_types =
  match SMap.find_opt type_name.node env.impls with
  | Some ii -> (
      match SMap.find_opt fn_name.node ii.ii_assoc_fns with
      | Some fi ->
          check_fn_args ~span:fn_name.span ~name:fn_name.node fi.fi_params
            arg_types;
          let ret = maybe_subst_self env fi.fi_ret in
          ret
      | None ->
          error_at fn_name.span "no associated function '%s' on type '%s'"
            fn_name.node type_name.node)
  | None -> error_at fn_name.span "no impl block for type '%s'" type_name.node

and check_method_call env receiver method_name args =
  let recv_ty = check_expr env receiver in
  let arg_types = List.map (check_expr env) args in
  (* Check built-in methods on container types *)
  match (recv_ty, method_name.node) with
  | TVec _, "len" -> TInt 64
  | TVec inner, "push" -> (
      check_mutability env receiver method_name;
      match arg_types with
      | [ arg ] ->
          expect_type ~span:method_name.span ~expected:inner ~actual:arg;
          TVoid
      | _ -> error_at method_name.span "push expects exactly 1 argument")
  | TVec inner, "pop" ->
      check_mutability env receiver method_name;
      TOption inner
  | THashMap (_, _), "len" -> TInt 64
  | THashMap (k, v), "insert" -> (
      check_mutability env receiver method_name;
      match arg_types with
      | [ ak; av ] ->
          expect_type ~span:method_name.span ~expected:k ~actual:ak;
          expect_type ~span:method_name.span ~expected:v ~actual:av;
          TVoid
      | _ -> error_at method_name.span "insert expects exactly 2 arguments")
  | THashMap (k, v), "get" -> (
      match arg_types with
      | [ ak ] ->
          expect_type ~span:method_name.span ~expected:k ~actual:ak;
          TOption v
      | _ -> error_at method_name.span "get expects exactly 1 argument")
  | THashMap (k, _), "contains_key" -> (
      match arg_types with
      | [ ak ] ->
          expect_type ~span:method_name.span ~expected:k ~actual:ak;
          TBool
      | _ -> error_at method_name.span "contains_key expects exactly 1 argument"
      )
  | THashMap (k, _), "remove" -> (
      check_mutability env receiver method_name;
      match arg_types with
      | [ ak ] ->
          expect_type ~span:method_name.span ~expected:k ~actual:ak;
          TVoid
      | _ -> error_at method_name.span "remove expects exactly 1 argument")
  | TString, "len" -> TInt 64
  | _ -> (
      (* Look up in impls *)
      let type_name = ty_name recv_ty in
      match SMap.find_opt type_name env.impls with
      | Some ii -> (
          match SMap.find_opt method_name.node ii.ii_methods with
          | Some mi ->
              if mi.mi_is_mut then check_mutability env receiver method_name;
              check_fn_args ~span:method_name.span ~name:method_name.node
                mi.mi_params arg_types;
              maybe_subst_self env mi.mi_ret
          | None ->
              error_at method_name.span "no method '%s' on type %s"
                method_name.node (show_ty recv_ty))
      | None ->
          error_at method_name.span "no method '%s' on type %s" method_name.node
            (show_ty recv_ty))

and ty_name t =
  match t with TStruct (n, _) -> n | TEnum (n, _) -> n | _ -> show_ty t

and check_mutability env receiver method_name =
  match receiver with
  | ExprIdent name -> (
      match lookup_value name.node env with
      | Some (_, true) -> () (* is_mut = true, ok *)
      | Some (_, false) ->
          error_at method_name.span
            "cannot call mutating method '%s' on immutable binding '%s'"
            method_name.node name.node
      | None -> ())
  | ExprSelf -> (
      match lookup_value "self" env with
      | Some (_, true) -> ()
      | Some (_, false) ->
          error_at method_name.span
            "cannot call mutating method '%s' on immutable receiver '&self'"
            method_name.node
      | None -> ())
  | _ -> () (* complex expressions - defer to runtime *)

and check_field_access env e field =
  let t = check_expr env e in
  match t with
  | TStruct (name, _) -> (
      match SMap.find_opt name env.structs with
      | Some si -> (
          match
            List.find_opt (fun (n, _, _) -> n = field.node) si.si_fields
          with
          | Some (_, ty, _) -> ty
          | None ->
              error_at field.span "no field '%s' on struct '%s'" field.node name
          )
      | None -> error_at field.span "unknown struct '%s'" name)
  | _ -> error_at field.span "field access on non-struct type %s" (show_ty t)

and check_path env type_name member =
  (* Type::Variant or Type::assoc_fn (without call) *)
  match SMap.find_opt type_name.node env.enums with
  | Some ei -> (
      match List.assoc_opt member.node ei.ei_variants with
      | Some VUnit -> TEnum (type_name.node, [])
      | Some _ ->
          error_at member.span "variant '%s::%s' requires arguments"
            type_name.node member.node
      | None ->
          error_at member.span "undefined variant '%s' in '%s'" member.node
            type_name.node)
  | None -> (
      (* Could be an assoc fn reference *)
      match SMap.find_opt type_name.node env.impls with
      | Some ii -> (
          match SMap.find_opt member.node ii.ii_assoc_fns with
          | Some fi -> TFn (fi.fi_params, fi.fi_ret)
          | None ->
              error_at member.span "no associated function '%s' on type '%s'"
                member.node type_name.node)
      | None -> error_at type_name.span "unknown type '%s'" type_name.node)

and check_struct_literal env ty fields =
  let type_name =
    match ty with
    | TyName name -> name
    | TyGeneric (name, _) -> name
    | _ -> failwith "typecheck: bad struct literal type"
  in
  let resolved = resolve_ast_ty env (ty : Ast.ty) in
  (match SMap.find_opt type_name.node env.structs with
  | Some si ->
      List.iter
        (fun (sf : struct_field_init) ->
          match
            List.find_opt (fun (n, _, _) -> n = sf.sf_name.node) si.si_fields
          with
          | Some (_, expected, _) ->
              let actual = check_expr env sf.sf_expr in
              expect_type ~span:sf.sf_name.span ~expected ~actual
          | None ->
              error_at sf.sf_name.span "no field '%s' in struct '%s'"
                sf.sf_name.node type_name.node)
        fields
  | None ->
      (* Just typecheck field expressions *)
      List.iter
        (fun (sf : struct_field_init) ->
          let _ = check_expr env sf.sf_expr in
          ())
        fields);
  resolved

and check_if env cond then_blk else_blk =
  let ct = check_expr env cond in
  expect_type ~span:(expr_span cond) ~expected:TBool ~actual:ct;
  let then_ty = check_block env then_blk in
  match else_blk with
  | Some else_b ->
      let else_ty = check_block env else_b in
      (* If both branches produce a value, types must match *)
      if then_ty <> TVoid && else_ty <> TVoid then
        expect_type ~span:dummy_span ~expected:then_ty ~actual:else_ty;
      then_ty
  | None -> TVoid

and check_match env scrutinee arms =
  let scrutinee_ty = check_expr env scrutinee in
  let arm_types =
    List.map
      (fun (arm : match_arm) ->
        let inner = push_scope env in
        let inner = bind_pattern inner scrutinee_ty arm.arm_pat in
        check_expr inner arm.arm_expr)
      arms
  in
  match arm_types with
  | [] -> TVoid
  | first :: rest ->
      List.iter
        (fun t -> expect_type ~span:dummy_span ~expected:first ~actual:t)
        rest;
      first

and bind_pattern env scrutinee_ty (p : pat) : env =
  match p with
  | PatWild -> env
  | PatBind name -> add_value name.node scrutinee_ty ~is_mut:false env
  | PatLit _ -> env
  | PatTuple (enum_name, variant_name, pats) -> (
      match SMap.find_opt enum_name.node env.enums with
      | Some ei -> (
          match List.assoc_opt variant_name.node ei.ei_variants with
          | Some (VTuple field_tys) ->
              List.fold_left2
                (fun e p ty -> bind_pattern e ty p)
                env pats field_tys
          | _ -> env)
      | None -> env)
  | PatStruct (enum_name, variant_name, field_pats) -> (
      match SMap.find_opt enum_name.node env.enums with
      | Some ei -> (
          match List.assoc_opt variant_name.node ei.ei_variants with
          | Some (VStruct fields) ->
              List.fold_left
                (fun e (fp : field_pat) ->
                  match fp.fp_pat with
                  | Some p ->
                      let field_ty =
                        match List.assoc_opt fp.fp_name.node fields with
                        | Some t -> t
                        | None -> TVoid
                      in
                      bind_pattern e field_ty p
                  | None ->
                      let field_ty =
                        match List.assoc_opt fp.fp_name.node fields with
                        | Some t -> t
                        | None -> TVoid
                      in
                      add_value fp.fp_name.node field_ty ~is_mut:false e)
                env field_pats
          | _ -> env)
      | None -> env)

and check_block env blk =
  let inner = push_scope env in
  let inner = List.fold_left (fun e s -> check_stmt e s) inner blk.stmts in
  match blk.final_expr with Some e -> check_expr inner e | None -> TVoid

and check_stmt env (s : stmt) : env =
  match s with
  | StmtLet { is_mut; pat; ty; init } ->
      let declared_ty =
        match (ty, init) with
        | Some t, ExprArray [] ->
            (* Empty array with type annotation: use the annotation *)
            let dt = resolve_ast_ty env t in
            dt
        | Some t, _ ->
            let init_ty = check_expr env init in
            let dt = resolve_ast_ty env t in
            expect_type ~span:(expr_span init) ~expected:dt ~actual:init_ty;
            dt
        | None, _ -> check_expr env init
      in
      bind_let_pattern env declared_ty is_mut pat
  | StmtExpr e ->
      let _ = check_expr env e in
      env

and bind_let_pattern env ty is_mut (p : pat) : env =
  match p with
  | PatBind name -> add_value name.node ty ~is_mut env
  | PatWild -> env
  | _ -> env (* more complex patterns could be handled *)

and check_return env e_opt =
  (match (env.ret_ty, e_opt) with
  | Some expected, Some e ->
      let actual = check_expr env e in
      expect_type ~span:(expr_span e) ~expected ~actual
  | Some expected, None ->
      if expected <> TVoid then
        error_at dummy_span "return without value in function returning %s"
          (show_ty expected)
  | None, _ -> ());
  TVoid

and check_assign env lhs rhs =
  let lhs_ty = check_expr env lhs in
  let rhs_ty = check_expr env rhs in
  (* Check mutability for simple identifier assignments *)
  (match lhs with
  | ExprIdent name -> (
      match lookup_value name.node env with
      | Some (_, false) ->
          error_at name.span "cannot assign to immutable binding '%s'" name.node
      | _ -> ())
  | ExprFieldAccess (ExprIdent name, _) -> (
      match lookup_value name.node env with
      | Some (_, false) ->
          error_at name.span "cannot assign to field of immutable binding '%s'"
            name.node
      | _ -> ())
  | ExprFieldAccess (ExprSelf, _) -> (
      match lookup_value "self" env with
      | Some (_, false) ->
          error_at (expr_span lhs)
            "cannot assign to field of immutable receiver '&self'"
      | _ -> ())
  | _ -> ());
  expect_type ~span:(expr_span rhs) ~expected:lhs_ty ~actual:rhs_ty;
  TVoid

and check_question env e =
  let t = check_expr env e in
  let span = expr_span e in
  match (t, env.ret_ty) with
  | TResult (ok, err), Some (TResult (_, ret_err)) ->
      (* ? on Result<T,E> in Result-returning fn -> unwrap T, but error types must match *)
      if not (types_compatible ret_err err) then
        error_at span
          "`?` error type mismatch: function returns Result<_, %s> but \
           expression has error type %s"
          (show_ty ret_err) (show_ty err);
      ok
  | TOption inner, Some (TOption _) ->
      (* ? on Option<T> in Option-returning fn -> unwrap T *)
      inner
  | TResult _, Some ret ->
      error_at span
        "cannot use `?` on Result in function returning %s (expected Result<_, \
         _>)"
        (show_ty ret)
  | TOption _, Some ret ->
      error_at span
        "cannot use `?` on Option in function returning %s (expected Option<_>)"
        (show_ty ret)
  | TResult _, None -> error_at span "cannot use `?` outside of a function"
  | TOption _, None -> error_at span "cannot use `?` outside of a function"
  | _, _ ->
      error_at span "cannot use `?` on type %s (expected Result or Option)"
        (show_ty t)

and check_array env elems =
  match elems with
  | [] ->
      (* Empty array literal - check if we have a type hint from context.
         This will be checked by check_stmt when a type annotation is present.
         If we reach here with no context, it's an error. *)
      raise
        (Typecheck_error
           {
             msg =
               "cannot infer element type for empty array literal; add a type \
                annotation";
             line = 0;
             col = 0;
           })
  | first :: rest ->
      let first_ty = check_expr env first in
      List.iter
        (fun e ->
          let t = check_expr env e in
          expect_type ~span:(expr_span e) ~expected:first_ty ~actual:t)
        rest;
      TVec first_ty

and check_repeat env elem count =
  let elem_ty = check_expr env elem in
  let count_ty = check_expr env count in
  let span = expr_span count in
  (match count_ty with
  | TInt _ | TUint _ -> ()
  | _ -> error_at span "repeat count must be integer, got %s" (show_ty count_ty));
  TVec elem_ty

and check_index env e idx =
  let et = check_expr env e in
  let idx_ty = check_expr env idx in
  let span = expr_span idx in
  (match idx_ty with
  | TInt _ | TUint _ -> ()
  | _ -> error_at span "index must be integer, got %s" (show_ty idx_ty));
  match et with
  | TVec inner -> inner
  | TString -> TString
  | _ -> error_at (expr_span e) "cannot index into type %s" (show_ty et)

and check_for env binding iter_expr blk =
  let iter_ty = check_expr env iter_expr in
  let elem_ty =
    match iter_ty with
    | TVec inner -> inner
    | THashMap (k, v) -> TTuple [ k; v ]
    | TString -> TString
    | _ ->
        error_at (expr_span iter_expr) "cannot iterate over type %s"
          (show_ty iter_ty)
  in
  let inner = push_scope env in
  let inner = add_value binding.node elem_ty ~is_mut:false inner in
  let _ = check_block inner blk in
  TVoid

and maybe_subst_self env ty =
  match env.self_ty with Some self_ty -> subst_self self_ty ty | None -> ty

(* ---------- item checking ---------- *)

let rec collect_type_info env (items : item list) : env =
  List.fold_left
    (fun env item ->
      match item with
      | ItemStruct { s_name; s_generics; s_fields; _ } ->
          let generics =
            List.map (fun (tp : type_param) -> tp.tp_name.node) s_generics
          in
          let env_with_generics =
            { env with type_params = generics @ env.type_params }
          in
          let fields =
            List.map
              (fun (f : field) ->
                ( f.fd_name.node,
                  resolve_ast_ty env_with_generics f.fd_ty,
                  f.fd_pub ))
              s_fields
          in
          let si = { si_fields = fields } in
          { env with structs = SMap.add s_name.node si env.structs }
      | ItemEnum { e_name; e_generics; e_variants; _ } ->
          let generics =
            List.map (fun (tp : type_param) -> tp.tp_name.node) e_generics
          in
          let env_with_generics =
            { env with type_params = generics @ env.type_params }
          in
          let variants =
            List.map
              (fun (v : variant) ->
                let shape =
                  match v.var_fields with
                  | None -> VUnit
                  | Some (TupleFields tys) ->
                      VTuple (List.map (resolve_ast_ty env_with_generics) tys)
                  | Some (StructFields fields) ->
                      VStruct
                        (List.map
                           (fun (f : field) ->
                             ( f.fd_name.node,
                               resolve_ast_ty env_with_generics f.fd_ty ))
                           fields)
                in
                (v.var_name.node, shape))
              e_variants
          in
          let ei = { ei_variants = variants } in
          { env with enums = SMap.add e_name.node ei env.enums }
      | ItemFn fd ->
          let generics =
            List.map (fun (tp : type_param) -> tp.tp_name.node) fd.fn_generics
          in
          let env_with_generics =
            { env with type_params = generics @ env.type_params }
          in
          let params =
            List.map
              (fun (p : param) -> resolve_ast_ty env_with_generics p.p_ty)
              fd.fn_params
          in
          let ret =
            match fd.fn_ret with
            | Some t -> resolve_ast_ty env_with_generics t
            | None -> TVoid
          in
          let fi = { fi_params = params; fi_ret = ret } in
          { env with fns = SMap.add fd.fn_name.node fi env.fns }
      | ItemImpl { i_ty; i_items; i_generics } ->
          collect_impl env i_generics i_ty i_items
      | ItemTraitImpl { ti_ty; ti_items; ti_generics; _ } ->
          collect_impl env ti_generics ti_ty ti_items
      | ItemTrait { t_name; t_generics; t_items; _ } ->
          let generics =
            List.map (fun (tp : type_param) -> tp.tp_name.node) t_generics
          in
          let env_with_generics =
            { env with type_params = generics @ env.type_params }
          in
          let methods =
            List.fold_left
              (fun acc ti ->
                match ti with
                | TraitFnSig sig_ ->
                    let params =
                      List.map
                        (fun (p : param) ->
                          resolve_ast_ty env_with_generics p.p_ty)
                        sig_.sig_params
                    in
                    let ret =
                      match sig_.sig_ret with
                      | Some t -> resolve_ast_ty env_with_generics t
                      | None -> TVoid
                    in
                    let tms =
                      {
                        tms_self = sig_.sig_self;
                        tms_params = params;
                        tms_ret = ret;
                        tms_has_default = false;
                      }
                    in
                    SMap.add sig_.sig_name.node tms acc
                | TraitFnDecl fd ->
                    let params =
                      List.map
                        (fun (p : param) ->
                          resolve_ast_ty env_with_generics p.p_ty)
                        fd.fn_params
                    in
                    let ret =
                      match fd.fn_ret with
                      | Some t -> resolve_ast_ty env_with_generics t
                      | None -> TVoid
                    in
                    let tms =
                      {
                        tms_self = fd.fn_self;
                        tms_params = params;
                        tms_ret = ret;
                        tms_has_default = true;
                      }
                    in
                    SMap.add fd.fn_name.node tms acc)
              SMap.empty t_items
          in
          let ti = { tr_methods = methods } in
          { env with traits = SMap.add t_name.node ti env.traits })
    env items

and collect_impl env impl_generics i_ty impl_items =
  let generics =
    List.map (fun (tp : type_param) -> tp.tp_name.node) impl_generics
  in
  let env_with_generics =
    { env with type_params = generics @ env.type_params }
  in
  let self_ty = resolve_ast_ty env_with_generics i_ty in
  let type_name =
    match i_ty with
    | TyName name -> name.node
    | TyGeneric (name, _) -> name.node
    | _ -> show_ty self_ty
  in
  let existing =
    match SMap.find_opt type_name env.impls with
    | Some ii -> ii
    | None -> { ii_methods = SMap.empty; ii_assoc_fns = SMap.empty }
  in
  let env_for_methods = { env_with_generics with self_ty = Some self_ty } in
  let updated =
    List.fold_left
      (fun ii (fd : fn_decl) ->
        let fn_generics =
          List.map (fun (tp : type_param) -> tp.tp_name.node) fd.fn_generics
        in
        let env_fn =
          {
            env_for_methods with
            type_params = fn_generics @ env_for_methods.type_params;
          }
        in
        let params =
          List.map
            (fun (p : param) -> resolve_ast_ty env_fn p.p_ty)
            fd.fn_params
        in
        let ret =
          match fd.fn_ret with
          | Some t -> resolve_ast_ty env_fn t
          | None -> TVoid
        in
        match fd.fn_self with
        | Some self_param ->
            let is_mut =
              match self_param with SelfMutRef -> true | _ -> false
            in
            let mi = { mi_params = params; mi_ret = ret; mi_is_mut = is_mut } in
            { ii with ii_methods = SMap.add fd.fn_name.node mi ii.ii_methods }
        | None ->
            let fi = { fi_params = params; fi_ret = ret } in
            {
              ii with
              ii_assoc_fns = SMap.add fd.fn_name.node fi ii.ii_assoc_fns;
            })
      existing impl_items
  in
  { env with impls = SMap.add type_name updated env.impls }

let check_fn_decl env (fd : fn_decl) =
  let generics =
    List.map (fun (tp : type_param) -> tp.tp_name.node) fd.fn_generics
  in
  let fn_env = { env with type_params = generics @ env.type_params } in
  let fn_env = push_scope fn_env in
  (* Add parameters *)
  let fn_env =
    List.fold_left
      (fun e (p : param) ->
        let ty = resolve_ast_ty e p.p_ty in
        add_value p.p_name.node ty ~is_mut:p.p_mut e)
      fn_env fd.fn_params
  in
  (* Set return type *)
  let ret_ty =
    match fd.fn_ret with Some t -> resolve_ast_ty fn_env t | None -> TVoid
  in
  let fn_env = { fn_env with ret_ty = Some (maybe_subst_self fn_env ret_ty) } in
  (* Check body *)
  let body_ty = check_block fn_env fd.fn_body in
  (* Check that body type matches return type for functions with a return type *)
  let resolved_ret = maybe_subst_self fn_env ret_ty in
  if resolved_ret <> TVoid && body_ty <> TVoid then
    expect_type ~span:fd.fn_name.span ~expected:resolved_ret ~actual:body_ty

let check_item env (item : item) =
  match item with
  | ItemFn fd -> check_fn_decl env fd
  | ItemStruct _ -> () (* type info collected in first pass *)
  | ItemEnum _ -> ()
  | ItemImpl { i_ty; i_items; i_generics } ->
      let generics =
        List.map (fun (tp : type_param) -> tp.tp_name.node) i_generics
      in
      let impl_env = { env with type_params = generics @ env.type_params } in
      let self_ty = resolve_ast_ty impl_env i_ty in
      let impl_env = { impl_env with self_ty = Some self_ty } in
      List.iter
        (fun fd ->
          let method_env =
            match fd.fn_self with
            | Some self_p ->
                let is_mut =
                  match self_p with SelfMutRef -> true | _ -> false
                in
                add_value "self" self_ty ~is_mut impl_env
            | None -> impl_env
          in
          check_fn_decl method_env fd)
        i_items
  | ItemTraitImpl { ti_trait; ti_ty; ti_items; ti_generics } -> (
      let generics =
        List.map (fun (tp : type_param) -> tp.tp_name.node) ti_generics
      in
      let impl_env = { env with type_params = generics @ env.type_params } in
      let self_ty = resolve_ast_ty impl_env ti_ty in
      let impl_env = { impl_env with self_ty = Some self_ty } in
      (* Type-check method bodies *)
      List.iter
        (fun fd ->
          let method_env =
            match fd.fn_self with
            | Some self_p ->
                let is_mut =
                  match self_p with SelfMutRef -> true | _ -> false
                in
                add_value "self" self_ty ~is_mut impl_env
            | None -> impl_env
          in
          check_fn_decl method_env fd)
        ti_items;
      (* Validate impl against trait contract *)
      match SMap.find_opt ti_trait.node env.traits with
      | None -> () (* trait not found: resolver handles this *)
      | Some tr_info ->
          (* Check for missing required methods *)
          SMap.iter
            (fun mname tms ->
              if
                (not tms.tms_has_default)
                && not
                     (List.exists
                        (fun (fd : fn_decl) -> fd.fn_name.node = mname)
                        ti_items)
              then
                error_at ti_trait.span
                  "missing required method '%s' in impl %s for %s" mname
                  ti_trait.node (show_ty self_ty))
            tr_info.tr_methods;
          (* Check signature compatibility for provided methods *)
          List.iter
            (fun (fd : fn_decl) ->
              match SMap.find_opt fd.fn_name.node tr_info.tr_methods with
              | None -> () (* extra methods are allowed in impls *)
              | Some tms ->
                  (* Check receiver kind matches *)
                  (if tms.tms_self <> fd.fn_self then
                     let show_recv = function
                       | None -> "no receiver"
                       | Some SelfValue -> "self"
                       | Some SelfRef -> "&self"
                       | Some SelfMutRef -> "&mut self"
                     in
                     error_at fd.fn_name.span
                       "receiver mismatch for method '%s' in impl %s: trait \
                        declares %s, impl provides %s"
                       fd.fn_name.node ti_trait.node (show_recv tms.tms_self)
                       (show_recv fd.fn_self));
                  let impl_params =
                    List.map
                      (fun (p : param) -> resolve_ast_ty impl_env p.p_ty)
                      fd.fn_params
                  in
                  let impl_ret =
                    match fd.fn_ret with
                    | Some t -> resolve_ast_ty impl_env t
                    | None -> TVoid
                  in
                  let expected_len = List.length tms.tms_params in
                  let actual_len = List.length impl_params in
                  if expected_len <> actual_len then
                    error_at fd.fn_name.span
                      "signature mismatch for method '%s' in impl %s: expected \
                       %d parameter(s), got %d"
                      fd.fn_name.node ti_trait.node expected_len actual_len
                  else (
                    List.iter2
                      (fun exp act ->
                        if not (types_compatible exp act) then
                          error_at fd.fn_name.span
                            "signature mismatch for method '%s' in impl %s: \
                             parameter type mismatch, expected %s, got %s"
                            fd.fn_name.node ti_trait.node (show_ty exp)
                            (show_ty act))
                      tms.tms_params impl_params;
                    if not (types_compatible tms.tms_ret impl_ret) then
                      error_at fd.fn_name.span
                        "signature mismatch for method '%s' in impl %s: return \
                         type mismatch, expected %s, got %s"
                        fd.fn_name.node ti_trait.node (show_ty tms.tms_ret)
                        (show_ty impl_ret)))
            ti_items)
  | ItemTrait _ -> ()

(* ---------- main entry points ---------- *)

let typecheck_exn (prog : program) : program =
  let env = empty_env in
  let env = collect_type_info env prog.items in
  List.iter (check_item env) prog.items;
  prog

let typecheck (prog : program) : (program, string) result =
  try Ok (typecheck_exn prog)
  with Typecheck_error { msg; line; col } ->
    Error (Printf.sprintf "%d:%d: %s" line col msg)
