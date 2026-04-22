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

(* ---------- stdlib interop surface (delegated to Interop registry) ---------- *)

let lookup_stdlib_member = Interop.lookup_member
let is_wrong_case_stdlib = Interop.wrong_case_member

(* ---------- stdlib receiver/member surface (delegated to Interop registry) ---------- *)

type stdlib_method_info = { smi_params : Types.ty list; smi_ret : Types.ty }
type stdlib_field_info = { sfi_ty : Types.ty }

let stdlib_receiver_methods (pkg : string) (type_name : string)
    (method_name : string) : stdlib_method_info option =
  match Interop.receiver_method pkg type_name method_name with
  | Some rm -> Some { smi_params = rm.rmi_params; smi_ret = rm.rmi_ret }
  | None -> None

let stdlib_receiver_fields (pkg : string) (type_name : string)
    (field_name : string) : stdlib_field_info option =
  match Interop.receiver_field pkg type_name field_name with
  | Some rf -> Some { sfi_ty = rf.rfi_ty }
  | None -> None

let is_wrong_case_receiver = Interop.wrong_case_receiver

(* ---------- stdlib type resolution (delegated to Interop registry) ---------- *)

let stdlib_type_ty_simple = Interop.type_ty

(* ---------- environment ---------- *)
module SMap = Map.Make (String)

type struct_info = { si_fields : (string * ty * bool (* pub *)) list }

type enum_info = {
  ei_variants : (string * variant_shape) list;
  ei_tparams : string list;
}

and variant_shape = VUnit | VTuple of ty list | VStruct of (string * ty) list

type fn_info = {
  fi_params : ty list;
  fi_ret : ty;
  fi_bounds : (string * string list) list;
      (* type_param_name -> list of required trait names *)
}

type method_info = {
  mi_params : ty list;
  mi_ret : ty;
  mi_is_mut : bool;
  mi_consumes_self : bool;
}

type impl_info = {
  ii_methods : method_info SMap.t;
  ii_assoc_fns : fn_info SMap.t;
  ii_type_params : string list;
  ii_self_ty : ty;
}

type trait_method_sig = {
  tms_self : Ast.self_param option;
  tms_params : ty list;
  tms_ret : ty;
  tms_has_default : bool; (* has a default body *)
}

type trait_info = {
  tr_generics : string list;
  tr_methods : trait_method_sig SMap.t;
}

module SSet = Set.Make (String)

type env = {
  values : (ty * bool (* is_mut *)) SMap.t list;
  structs : struct_info SMap.t;
  enums : enum_info SMap.t;
  fns : fn_info SMap.t;
  impls : impl_info SMap.t; (* type name -> impl info *)
  traits : trait_info SMap.t; (* trait name -> trait info *)
  trait_impls : SSet.t SMap.t; (* type name -> set of trait names implemented *)
  ret_ty : ty option; (* return type of current function *)
  self_ty : ty option; (* type bound to Self in current impl/trait *)
  current_trait : string option;
      (* trait name when inside a trait declaration *)
  type_params : string list; (* in-scope generic type params *)
  param_bounds : string list SMap.t;
      (* type_param_name -> list of required trait names *)
  imported_packages : string list SMap.t;
      (* alias -> path segments, for import-aware diagnostics *)
  self_consumed : bool; (* true when current method takes self by value *)
  moved : SSet.t ref; (* mutable set of bindings that have been moved *)
}

let empty_env =
  {
    values = [ SMap.empty ];
    structs = SMap.empty;
    enums = SMap.empty;
    fns = SMap.empty;
    impls = SMap.empty;
    traits = SMap.empty;
    trait_impls = SMap.empty;
    ret_ty = None;
    self_ty = None;
    current_trait = None;
    type_params = [];
    param_bounds = SMap.empty;
    imported_packages = SMap.empty;
    self_consumed = false;
    moved = ref SSet.empty;
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

(* ---------- ownership: Copy / move tracking ---------- *)

(* A type is Copy-eligible when it is a primitive, a reference, a function
   value, or a user-defined nominal type that has `impl Copy`.  Function-type
   values (TFn) are inherently Copy because they are reference-like: passing
   a named function as a callback does not transfer ownership of the original
   callable binding. *)
let rec is_copy_type (env : env) (t : ty) : bool =
  match t with
  | TInt _ | TUint _ | TFloat _ | TBool -> true
  | TRef _ -> true
  | TFn _ -> true
  | TTuple ts -> List.for_all (fun t' -> is_copy_type env t') ts
  | TStruct (name, _) | TEnum (name, _) -> (
      match SMap.find_opt name env.trait_impls with
      | Some traits -> SSet.mem "Copy" traits
      | None -> false)
  | TParam p -> (
      match SMap.find_opt p env.param_bounds with
      | Some bounds -> List.mem "Copy" bounds
      | None -> false)
  (* Imported types, strings, containers: not Copy *)
  | TString | TVec _ | THashMap _ | TOption _ | TResult _ | TVoid | TVar _
  | TSelf | TImported _ ->
      false

(* Check whether a type has Drop implemented.
   Used by downstream features for cleanup scheduling. *)
let _is_drop_type (env : env) (t : ty) : bool =
  match t with
  | TStruct (name, _) | TEnum (name, _) -> (
      match SMap.find_opt name env.trait_impls with
      | Some traits -> SSet.mem "Drop" traits
      | None -> false)
  | _ -> false

(* Check whether a type has Clone implemented. *)
let is_clone_type (env : env) (t : ty) : bool =
  match t with
  | TStruct (name, _) | TEnum (name, _) -> (
      match SMap.find_opt name env.trait_impls with
      | Some traits -> SSet.mem "Clone" traits
      | None -> false)
  (* Primitives are trivially cloneable *)
  | TInt _ | TUint _ | TFloat _ | TBool | TString -> true
  | _ -> false

(* Mark a binding as moved. *)
let mark_moved (env : env) (name : string) : unit =
  env.moved := SSet.add name !(env.moved)

(* Check that a binding has not been moved; raise if it has. *)
let check_not_moved (env : env) (span : span) (name : string) : unit =
  if SSet.mem name !(env.moved) then
    error_at span "use of moved value '%s'" name

(* When a non-Copy value is consumed (assigned to another binding or passed
   by value), mark the source as moved. *)
let consume_if_non_copy (env : env) (span : span) (name : string) (t : ty) :
    unit =
  if not (is_copy_type env t) then begin
    check_not_moved env span name;
    mark_moved env name
  end

(* Would extracting this type from a field constitute a partial move?
   Only non-Copy nominal types, containers, and unbound generic params
   are real move targets.  Primitives, strings, and references are
   effectively value-copied at the Go level. *)
let is_partial_move_type (env : env) (t : ty) : bool =
  match t with
  | TStruct (name, _) | TEnum (name, _) -> (
      match SMap.find_opt name env.trait_impls with
      | Some traits -> not (SSet.mem "Copy" traits)
      | None -> true)
  | TVec _ | THashMap _ | TImported _ -> true
  | TParam p -> (
      match SMap.find_opt p env.param_bounds with
      | Some bounds -> not (List.mem "Copy" bounds)
      | None -> true)
  | TOption _ | TResult (_, _) -> true
  | TInt _ | TUint _ | TFloat _ | TBool | TString | TRef _ | TVoid | TTuple _
  | TFn _ | TVar _ | TSelf ->
      false

(* Whether a payload type extracted via pattern destructuring would
   constitute a disallowed partial move.  For built-in Option/Result
   pattern bindings we reject non-Copy user-defined nominal types and
   recursively reject nested built-in containers that wrap non-Copy
   payloads. *)
let rec is_destructure_move_type (env : env) (t : ty) : bool =
  match t with
  | TStruct (name, _) | TEnum (name, _) -> (
      match SMap.find_opt name env.trait_impls with
      | Some traits -> not (SSet.mem "Copy" traits)
      | None -> true)
  | TParam p -> (
      match SMap.find_opt p env.param_bounds with
      | Some bounds -> not (List.mem "Copy" bounds)
      | None -> true)
  | TVec _ | THashMap _ | TImported _ -> true
  | TOption t -> is_destructure_move_type env t
  | TResult (t1, t2) ->
      is_destructure_move_type env t1 || is_destructure_move_type env t2
  | _ -> false

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
  | TyPath (pkg, member) -> (
      (* Package-qualified type e.g. http::Request *)
      match lookup_stdlib_member pkg.node member.node with
      | Some mi -> (
          match mi.mi_kind with
          | Interop.MemberType -> (
              match stdlib_type_ty_simple pkg.node member.node with
              | Some ty -> ty
              | None ->
                  error_at member.span "undefined type '%s::%s'" pkg.node
                    member.node)
          | Interop.MemberFn ->
              error_at member.span "'%s::%s' is a function, not a type" pkg.node
                member.node)
      | None -> (
          match is_wrong_case_stdlib pkg.node member.node with
          | Some correct ->
              error_at member.span
                "wrong case: '%s::%s' should be '%s::%s' (use snake_case for \
                 functions, PascalCase for types)"
                pkg.node member.node pkg.node correct
          | None ->
              error_at member.span
                "undefined member '%s' in imported package '%s'" member.node
                pkg.node))

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
  | TImported _ -> t
  | _ -> t

(* ---------- type compatibility ---------- *)

(* Collect all TParam names occurring in a type. *)
let rec collect_tparams acc = function
  | TParam p -> if List.mem p acc then acc else p :: acc
  | TOption t | TVec t | TRef t -> collect_tparams acc t
  | TResult (a, b) | THashMap (a, b) ->
      collect_tparams (collect_tparams acc a) b
  | TTuple ts -> List.fold_left collect_tparams acc ts
  | TStruct (_, args) | TEnum (_, args) ->
      List.fold_left collect_tparams acc args
  | TFn (ps, r) -> collect_tparams (List.fold_left collect_tparams acc ps) r
  | _ -> acc

(* Check if a type is a numeric literal that can coerce to the target.
   ~type_params lists the generic type parameter names currently in scope;
   TParam is only a wildcard when its name is in this list.
   ~strict disables the TVoid wildcard so function signature comparisons
   don't treat TVoid as matching any type — function types are structural
   contracts and must not accept mismatched return types. *)
let rec types_compatible ?(type_params = []) ?(strict = false) expected actual =
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
        types_compatible ~type_params ok1 ok2
        && types_compatible ~type_params err1 err2
    | TOption inner1, TOption inner2 ->
        types_compatible ~type_params inner1 inner2
    | TVec inner1, TVec inner2 -> types_compatible ~type_params inner1 inner2
    | THashMap (k1, v1), THashMap (k2, v2) ->
        types_compatible ~type_params k1 k2
        && types_compatible ~type_params v1 v2
    (* TVoid acts as a wildcard for builtin constructors like HashMap::new(),
       but NOT when comparing function signatures (strict mode). *)
    | _, TVoid -> not strict
    | TVoid, _ -> not strict
    (* TParam is a wildcard only when its name is in the ~type_params list
       (which should contain only the *callee's* generic parameters, not
       the caller's in-scope parameters).  Same-name TParams are already
       handled by the structural equality check at the top. *)
    | TParam p, _ -> List.mem p type_params
    | _, TParam p -> List.mem p type_params
    (* Struct/Enum with matching name: compatible when args are pairwise
       compatible. Bare names (no args) are NOT automatically compatible
       with instantiated forms. *)
    | TStruct (n1, args1), TStruct (n2, args2) when n1 = n2 ->
        List.length args1 = List.length args2
        && List.for_all2 (types_compatible ~type_params) args1 args2
    | TEnum (n1, args1), TEnum (n2, args2) when n1 = n2 ->
        List.length args1 = List.length args2
        && List.for_all2 (types_compatible ~type_params) args1 args2
    (* TFn: compatible when params and return type are structurally
       compatible.  Uses strict comparison for the return type and
       parameters so that TVoid wildcards don't make mismatched
       function signatures pass — function types are structural contracts. *)
    | TFn (ps1, r1), TFn (ps2, r2) ->
        List.length ps1 = List.length ps2
        && types_compatible ~type_params ~strict:true r1 r2
        && List.for_all2 (types_compatible ~type_params ~strict:true) ps1 ps2
    (* TImported: same package+name is compatible *)
    | TImported (p1, n1), TImported (p2, n2) -> p1 = p2 && n1 = n2
    | _ -> false

let expect_type ~env:_ ~(span : span) ~expected ~actual =
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
  | "println" ->
      Some { fi_params = [ TString ]; fi_ret = TVoid; fi_bounds = [] }
  | "print" -> Some { fi_params = [ TString ]; fi_ret = TVoid; fi_bounds = [] }
  | "len" -> None (* handled as method call *)
  | "to_string" -> None
  | "panic" -> Some { fi_params = [ TString ]; fi_ret = TVoid; fi_bounds = [] }
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
  | ExprStructVariant (id, _, _) -> id.span
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
          | Some (ty, _) ->
              check_not_moved env name.span name.node;
              ty
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
      expect_type ~env ~span ~expected:TBool ~actual:t;
      TBool
  | ExprBinary (op, l, r) -> check_binop env op l r
  | ExprCall (callee, args) -> check_call env callee args
  | ExprMethodCall (receiver, method_name, args) ->
      check_method_call env receiver method_name args
  | ExprFieldAccess (e, field) -> check_field_access env e field
  | ExprPath (type_name, member) -> check_path env type_name member
  | ExprStruct (ty, fields) -> check_struct_literal env ty fields
  | ExprStructVariant (type_name, variant_name, fields) ->
      check_struct_variant_literal env type_name variant_name fields
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
          expect_type ~env ~span:(expr_span c) ~expected:TBool ~actual:ct)
        cond;
      let _ = check_block env blk in
      TVoid
  | ExprWhile (cond, blk) ->
      let ct = check_expr env cond in
      expect_type ~env ~span:(expr_span cond) ~expected:TBool ~actual:ct;
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
          expect_type ~env ~span ~expected:lt ~actual:rt;
          match lt with
          | TInt _ | TUint _ | TFloat _ -> lt
          | _ -> error_at span "arithmetic on non-numeric type %s" (show_ty lt))
      )
  | Eq | Ne | Lt | Gt | Le | Ge ->
      expect_type ~env ~span ~expected:lt ~actual:rt;
      TBool
  | And | Or ->
      expect_type ~env ~span ~expected:TBool ~actual:lt;
      expect_type ~env ~span:(expr_span r) ~expected:TBool ~actual:rt;
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
              check_fn_args ~env ~span:name.span ~name:name.node fi.fi_params
                arg_types;
              check_bounds env name.span fi.fi_bounds fi.fi_params arg_types;
              consume_call_args env args arg_types;
              maybe_subst_self env fi.fi_ret
          | None -> (
              match builtin_fn_type name.node with
              | Some fi ->
                  check_fn_args ~env ~span:name.span ~name:name.node
                    fi.fi_params arg_types;
                  consume_call_args env args arg_types;
                  fi.fi_ret
              | None -> (
                  (* Could be a variable holding a function value *)
                  match lookup_value name.node env with
                  | Some (TFn (params, ret), _) ->
                      check_fn_args ~env ~span:name.span ~name:name.node params
                        arg_types;
                      consume_call_args env args arg_types;
                      ret
                  | Some _ ->
                      error_at name.span "'%s' is not callable" name.node
                  | None ->
                      error_at name.span "undefined function '%s'" name.node))))
  | ExprPath (type_name, fn_name) ->
      (* Associated function call: Type::method(...) *)
      let arg_types = List.map (check_expr env) args in
      let result = check_assoc_fn_call env type_name fn_name arg_types in
      consume_call_args env args arg_types;
      result
  | _ -> (
      let ct = check_expr env callee in
      let arg_types = List.map (check_expr env) args in
      match ct with
      | TFn (params, ret) ->
          check_fn_args ~env ~span:(expr_span callee) ~name:"<expr>" params
            arg_types;
          consume_call_args env args arg_types;
          ret
      | _ -> error_at (expr_span callee) "expression is not callable")

(* Ownership: after a by-value call, mark each identifier argument as
   moved when its type is non-Copy.  Also reject partial moves from
   field access.

   Function-type values (TFn) are inherently Copy (see is_copy_type), so
   passing a named function as a callback never consumes the original
   callable binding — essential for handler registration where the same
   function can be registered on multiple routes. *)
and consume_call_args env (args : expr list) (arg_types : ty list) : unit =
  List.iter2
    (fun arg ty ->
      match arg with
      | ExprIdent name -> consume_if_non_copy env name.span name.node ty
      | ExprFieldAccess (_, field) when is_partial_move_type env ty ->
          error_at field.span
            "cannot move out of field '%s' -- partial moves are not supported"
            field.node
      | _ -> ())
    args arg_types

(* Ownership: when a method has a consuming `self` receiver, mark
   the receiver binding as moved if its type is non-Copy. *)
and consume_receiver env (span : span) (receiver : expr) (recv_ty : ty) : unit =
  match receiver with
  | ExprIdent name -> consume_if_non_copy env span name.node recv_ty
  | ExprSelf -> consume_if_non_copy env span "self" recv_ty
  | _ -> ()

and check_fn_args ~env:_ ~span ~name expected actual =
  let expected_len = List.length expected in
  let actual_len = List.length actual in
  if expected_len <> actual_len then
    error_at span "function '%s' expects %d argument(s), got %d" name
      expected_len actual_len;
  (* Collect TParam names from the callee's signature so they act as
     wildcards — the callee's own generic parameters can be instantiated
     with any concrete type.  The caller's in-scope type parameters are
     NOT included: they are rigid/opaque in the caller. *)
  let sig_tparams = List.fold_left collect_tparams [] expected in
  List.iter2
    (fun exp act ->
      if not (types_compatible ~type_params:sig_tparams exp act) then
        (* When a callback parameter expects a function type but the
           actual argument is not callable, give a direct "not callable"
           diagnostic instead of a generic type-mismatch message. *)
        match (exp, act) with
        | ( TFn _,
            ( TInt _ | TUint _ | TFloat _ | TBool | TString | TStruct _
            | TEnum _ | TVec _ | THashMap _ | TOption _ | TResult _ | TRef _
            | TTuple _ | TVoid | TSelf ) ) ->
            error_at span
              "not callable: expected %s, but got non-callable type %s"
              (show_ty exp) (show_ty act)
        | _ ->
            error_at span "type mismatch: expected %s, got %s" (show_ty exp)
              (show_ty act))
    expected actual

(* Check that trait bounds are satisfied when calling a bounded generic function.
   Infers the concrete type for each type parameter from actual args and checks
   that the concrete type has an impl for each required trait. *)
and check_bounds env span bounds formals actuals =
  if bounds = [] then ()
  else
    let bindings = infer_tparam_bindings formals actuals in
    List.iter
      (fun (tparam_name, required_traits) ->
        match List.assoc_opt tparam_name bindings with
        | None -> () (* unresolved; generic may be used without args *)
        | Some concrete_ty -> (
            (* If the concrete type is itself a type parameter, skip --
               it will be checked when the outer function is called. *)
            match concrete_ty with
            | TParam _ -> ()
            | _ ->
                let concrete_name = ty_name concrete_ty in
                let implemented =
                  match SMap.find_opt concrete_name env.trait_impls with
                  | Some s -> s
                  | None -> SSet.empty
                in
                List.iter
                  (fun trait_name ->
                    if not (SSet.mem trait_name implemented) then
                      error_at span
                        "type %s does not implement trait '%s' (required by \
                         bound on %s)"
                        (show_ty concrete_ty) trait_name tparam_name)
                  required_traits))
      bounds

(* ---------- stdlib interop resolution ---------- *)

and stdlib_fn_type (pkg_alias : string) (member_name : string) : fn_info option
    =
  match Interop.fn_type pkg_alias member_name with
  | Some fti ->
      Some { fi_params = fti.fti_params; fi_ret = fti.fti_ret; fi_bounds = [] }
  | None -> None

and check_stdlib_path _env type_name member =
  let pkg = type_name.node in
  let name = member.node in
  (* First check if it's a valid member *)
  match lookup_stdlib_member pkg name with
  | Some mi -> (
      match mi.mi_kind with
      | Interop.MemberType ->
          (* Imported types are not valid in expression/value position;
             use them in type annotations instead. *)
          error_at member.span "'%s::%s' is a type, not a value expression" pkg
            name
      | Interop.MemberFn -> (
          match stdlib_fn_type pkg name with
          | Some fi -> TFn (fi.fi_params, fi.fi_ret)
          | None ->
              error_at member.span
                "undefined member '%s' in imported package '%s'" name pkg))
  | None -> (
      (* Check for wrong-case usage *)
      match is_wrong_case_stdlib pkg name with
      | Some correct ->
          error_at member.span
            "wrong case: '%s::%s' should be '%s::%s' (use snake_case for \
             functions, PascalCase for types)"
            pkg name pkg correct
      | None ->
          error_at member.span "undefined member '%s' in imported package '%s'"
            name pkg)

and check_stdlib_call env type_name fn_name arg_types =
  let pkg = type_name.node in
  let name = fn_name.node in
  (* First check if it's a valid member *)
  match lookup_stdlib_member pkg name with
  | Some mi -> (
      match mi.mi_kind with
      | Interop.MemberFn -> (
          match stdlib_fn_type pkg name with
          | Some fi ->
              check_fn_args ~env ~span:fn_name.span ~name fi.fi_params arg_types;
              fi.fi_ret
          | None ->
              error_at fn_name.span
                "undefined member '%s' in imported package '%s'" name pkg)
      | Interop.MemberType ->
          error_at fn_name.span "'%s::%s' is a type, not a callable" pkg name)
  | None -> (
      (* Check for wrong-case usage *)
      match is_wrong_case_stdlib pkg name with
      | Some correct ->
          error_at fn_name.span
            "wrong case: '%s::%s' should be '%s::%s' (use snake_case for \
             functions, PascalCase for types)"
            pkg name pkg correct
      | None ->
          error_at fn_name.span "undefined member '%s' in imported package '%s'"
            name pkg)

and check_assoc_fn_call env type_name fn_name arg_types =
  (* If this is an imported package, resolve stdlib member *)
  if SMap.mem type_name.node env.imported_packages then
    check_stdlib_call env type_name fn_name arg_types
  else
    (* Check for enum variant constructors first *)
    match SMap.find_opt type_name.node env.enums with
    | Some ei -> (
        match List.assoc_opt fn_name.node ei.ei_variants with
        | Some (VTuple field_tys) ->
            let expected_len = List.length field_tys in
            let actual_len = List.length arg_types in
            if expected_len <> actual_len then
              error_at fn_name.span
                "variant '%s::%s' expects %d field(s), got %d" type_name.node
                fn_name.node expected_len actual_len;
            (* Type-check args: enum tparams act as wildcards *)
            List.iter2
              (fun exp act ->
                if not (types_compatible ~type_params:ei.ei_tparams exp act)
                then
                  error_at fn_name.span "type mismatch: expected %s, got %s"
                    (show_ty exp) (show_ty act))
              field_tys arg_types;
            (* Infer concrete type args for generic enums from payload.
               Inside an impl block for the same enum, inherit type args
               from Self instead of re-inferring (they may be generic). *)
            let type_args =
              match (ei.ei_tparams, env.self_ty) with
              | [], _ -> []
              | _, Some (TEnum (sn, (_ :: _ as args))) when sn = type_name.node
                ->
                  args
              | _tparams, _ ->
                  let bindings =
                    infer_tparam_bindings ~span:fn_name.span field_tys arg_types
                  in
                  List.map
                    (fun tp ->
                      match List.assoc_opt tp bindings with
                      | Some t -> t
                      | None -> TParam tp)
                    ei.ei_tparams
            in
            TEnum (type_name.node, type_args)
        | Some VUnit ->
            if arg_types <> [] then
              error_at fn_name.span "unit variant '%s::%s' takes no arguments"
                type_name.node fn_name.node;
            (* Inherit type args from Self inside an impl block for the same enum *)
            let type_args =
              match (ei.ei_tparams, env.self_ty) with
              | [], _ -> []
              | _, Some (TEnum (sn, (_ :: _ as args))) when sn = type_name.node
                ->
                  args
              | tparams, _ -> List.map (fun tp -> TParam tp) tparams
            in
            TEnum (type_name.node, type_args)
        | Some (VStruct _) ->
            error_at fn_name.span
              "struct variant '%s::%s' must use named fields" type_name.node
              fn_name.node
        | None -> check_impl_assoc_fn env type_name fn_name arg_types)
    | None -> check_impl_assoc_fn env type_name fn_name arg_types

and concrete_type_for_name env name =
  (* If we're inside an impl block whose Self type matches this name,
     use the full Self type (which preserves generic arguments like Box<T>). *)
  match env.self_ty with
  | Some ((TStruct (n, _) | TEnum (n, _)) as st) when n = name -> st
  | _ ->
      if SMap.mem name env.structs then TStruct (name, [])
      else if SMap.mem name env.enums then TEnum (name, [])
      else TVoid

(* Infer bindings for impl type parameters by matching formal param types
   against actual argument types. Returns a (string * ty) list mapping
   TParam names to concrete types.
   When ~span is provided, conflicting repeated bindings raise a type error. *)
and infer_tparam_bindings ?span formals actuals =
  let bindings = Hashtbl.create 4 in
  let rec unify formal actual =
    match (formal, actual) with
    | TParam p, _ -> (
        match Hashtbl.find_opt bindings p with
        | None -> Hashtbl.replace bindings p actual
        | Some prev -> (
            if not (types_compatible prev actual) then
              match span with
              | Some s ->
                  error_at s "conflicting types for type parameter %s: %s vs %s"
                    p (show_ty prev) (show_ty actual)
              | None -> ()))
    | TOption f, TOption a -> unify f a
    | TResult (f1, f2), TResult (a1, a2) ->
        unify f1 a1;
        unify f2 a2
    | TVec f, TVec a -> unify f a
    | THashMap (fk, fv), THashMap (ak, av) ->
        unify fk ak;
        unify fv av
    | TRef f, TRef a -> unify f a
    | TTuple fs, TTuple ats when List.length fs = List.length ats ->
        List.iter2 unify fs ats
    | TStruct (n1, args1), TStruct (n2, args2)
      when n1 = n2 && List.length args1 = List.length args2 ->
        List.iter2 unify args1 args2
    | TEnum (n1, args1), TEnum (n2, args2)
      when n1 = n2 && List.length args1 = List.length args2 ->
        List.iter2 unify args1 args2
    | _ -> ()
  in
  List.iter2 unify formals actuals;
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) bindings []

(* Substitute TParam names in a type using the given bindings. *)
and subst_tparams bindings t =
  match t with
  | TParam p -> (
      match List.assoc_opt p bindings with Some t' -> t' | None -> t)
  | TOption inner -> TOption (subst_tparams bindings inner)
  | TResult (ok, err) ->
      TResult (subst_tparams bindings ok, subst_tparams bindings err)
  | TVec inner -> TVec (subst_tparams bindings inner)
  | THashMap (k, v) ->
      THashMap (subst_tparams bindings k, subst_tparams bindings v)
  | TRef inner -> TRef (subst_tparams bindings inner)
  | TTuple ts -> TTuple (List.map (subst_tparams bindings) ts)
  | TStruct (n, args) -> TStruct (n, List.map (subst_tparams bindings) args)
  | TEnum (n, args) -> TEnum (n, List.map (subst_tparams bindings) args)
  | TFn (ps, r) ->
      TFn (List.map (subst_tparams bindings) ps, subst_tparams bindings r)
  | _ -> t

and check_impl_assoc_fn env type_name fn_name arg_types =
  match SMap.find_opt type_name.node env.impls with
  | Some ii -> (
      match SMap.find_opt fn_name.node ii.ii_assoc_fns with
      | Some fi ->
          check_fn_args ~env ~span:fn_name.span ~name:fn_name.node fi.fi_params
            arg_types;
          (* Substitute Self with the concrete type from the call site.
             We derive the concrete type from type_name so this works
             even outside impl blocks (e.g. Type::new().method()). *)
          let concrete_self = concrete_type_for_name env type_name.node in
          let result = subst_self concrete_self fi.fi_ret in
          (* If the result has empty type args but the impl is generic,
             infer concrete type arguments from the call arguments. *)
          let result =
            match (result, ii.ii_type_params) with
            | (TStruct (n, []) | TEnum (n, [])), _ :: _ ->
                (* Infer type param bindings from function args *)
                let bindings =
                  if
                    List.length fi.fi_params = List.length arg_types
                    && List.length arg_types > 0
                  then infer_tparam_bindings fi.fi_params arg_types
                  else []
                in
                if bindings <> [] then
                  (* Instantiate the impl's self_ty with inferred bindings *)
                  let instantiated = subst_tparams bindings ii.ii_self_ty in
                  (* Ensure the type name matches *)
                  match instantiated with
                  | TStruct (n', _) when n' = n -> instantiated
                  | TEnum (n', _) when n' = n -> instantiated
                  | _ -> result
                else result
            | _ -> result
          in
          result
      | None ->
          error_at fn_name.span "no associated function '%s' on type '%s'"
            fn_name.node type_name.node)
  | None ->
      (* Handle builtin constructors for collection types *)
      check_builtin_assoc_fn type_name fn_name arg_types

and check_builtin_assoc_fn type_name fn_name arg_types =
  match (type_name.node, fn_name.node) with
  | "HashMap", "new" ->
      if arg_types <> [] then
        error_at fn_name.span "HashMap::new() takes no arguments";
      THashMap (TVoid, TVoid)
  | "Vec", "new" ->
      if arg_types <> [] then
        error_at fn_name.span "Vec::new() takes no arguments";
      TVec TVoid
  | _ -> error_at fn_name.span "no impl block for type '%s'" type_name.node

and check_method_call env receiver method_name args =
  let recv_ty = check_expr env receiver in
  let arg_types = List.map (check_expr env) args in
  (* Ownership: handle clone() as a special method that does not consume
     the receiver. It returns a deep copy of the value. *)
  if method_name.node = "clone" && arg_types = [] then begin
    if not (is_clone_type env recv_ty) then
      error_at method_name.span "type %s does not implement Clone"
        (show_ty recv_ty);
    recv_ty
  end
  else
    (* Check built-in methods on container types *)
    match (recv_ty, method_name.node) with
    | TVec _, "len" -> TInt 64
    | TVec inner, "push" -> (
        check_mutability env receiver method_name;
        match arg_types with
        | [ arg ] ->
            expect_type ~env ~span:method_name.span ~expected:inner ~actual:arg;
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
            expect_type ~env ~span:method_name.span ~expected:k ~actual:ak;
            expect_type ~env ~span:method_name.span ~expected:v ~actual:av;
            TVoid
        | _ -> error_at method_name.span "insert expects exactly 2 arguments")
    | THashMap (k, v), "get" -> (
        match arg_types with
        | [ ak ] ->
            expect_type ~env ~span:method_name.span ~expected:k ~actual:ak;
            TOption v
        | _ -> error_at method_name.span "get expects exactly 1 argument")
    | THashMap (k, _), "contains_key" -> (
        match arg_types with
        | [ ak ] ->
            expect_type ~env ~span:method_name.span ~expected:k ~actual:ak;
            TBool
        | _ ->
            error_at method_name.span "contains_key expects exactly 1 argument")
    | THashMap (k, _), "remove" -> (
        check_mutability env receiver method_name;
        match arg_types with
        | [ ak ] ->
            expect_type ~env ~span:method_name.span ~expected:k ~actual:ak;
            TVoid
        | _ -> error_at method_name.span "remove expects exactly 1 argument")
    | TString, "len" -> TInt 64
    | _ -> check_user_method_call env receiver recv_ty method_name arg_types

and check_user_method_call env receiver recv_ty method_name arg_types =
  (* Resolve TSelf to concrete type before looking up impls *)
  let resolved_recv =
    match recv_ty with
    | TSelf -> ( match env.self_ty with Some st -> st | None -> recv_ty)
    | other -> other
  in
  (* Inside a trait declaration, Self resolves to TSelf; look up sibling methods *)
  match resolved_recv with
  | TSelf -> (
      match env.current_trait with
      | Some trait_name -> (
          match SMap.find_opt trait_name env.traits with
          | Some tr_info -> (
              match SMap.find_opt method_name.node tr_info.tr_methods with
              | Some tms ->
                  check_fn_args ~env ~span:method_name.span
                    ~name:method_name.node tms.tms_params arg_types;
                  tms.tms_ret
              | None ->
                  error_at method_name.span "no method '%s' on type %s"
                    method_name.node (show_ty resolved_recv))
          | None ->
              error_at method_name.span "no method '%s' on type %s"
                method_name.node (show_ty resolved_recv))
      | None ->
          error_at method_name.span "no method '%s' on type %s" method_name.node
            (show_ty resolved_recv))
  (* For type parameters with trait bounds, resolve method from trait *)
  | TParam p when List.mem p env.type_params -> (
      let bound_traits =
        match SMap.find_opt p env.param_bounds with Some ts -> ts | None -> []
      in
      (* Only search traits declared as bounds on this type parameter *)
      let providing_traits =
        List.filter_map
          (fun trait_name ->
            match SMap.find_opt trait_name env.traits with
            | Some tr_info -> (
                match SMap.find_opt method_name.node tr_info.tr_methods with
                | Some tms -> Some (trait_name, tms)
                | None -> None)
            | None -> None)
          bound_traits
      in
      match providing_traits with
      | [ (_, tms) ] ->
          let params = List.map (subst_self resolved_recv) tms.tms_params in
          let ret = subst_self resolved_recv tms.tms_ret in
          check_fn_args ~env ~span:method_name.span ~name:method_name.node
            params arg_types;
          (* Ownership: consuming self receiver moves the receiver binding *)
          (match tms.tms_self with
          | Some Ast.SelfValue ->
              consume_receiver env method_name.span receiver resolved_recv
          | _ -> ());
          ret
      | _ :: _ ->
          let names =
            List.map fst providing_traits |> List.sort String.compare
          in
          error_at method_name.span
            "ambiguous method '%s' on type %s: provided by traits %s"
            method_name.node (show_ty resolved_recv) (String.concat ", " names)
      | [] ->
          error_at method_name.span "no method '%s' on type %s" method_name.node
            (show_ty resolved_recv))
  (* Imported stdlib receiver methods *)
  | TImported (pkg, tname) -> (
      match stdlib_receiver_methods pkg tname method_name.node with
      | Some smi ->
          check_fn_args ~env ~span:method_name.span ~name:method_name.node
            smi.smi_params arg_types;
          smi.smi_ret
      | None -> (
          (* Check for wrong-case receiver member *)
          match is_wrong_case_receiver pkg tname method_name.node with
          | Some correct ->
              error_at method_name.span
                "wrong case: '%s' should be '%s' (rgo uses snake_case for \
                 methods)"
                method_name.node correct
          | None ->
              error_at method_name.span "no method '%s' on type %s::%s"
                method_name.node pkg tname))
  | _ -> (
      let type_name = ty_name resolved_recv in
      (* Check for ambiguous trait method resolution *)
      let providing_traits =
        SMap.fold
          (fun trait_name tr_info acc ->
            if SMap.mem method_name.node tr_info.tr_methods then
              let impl_set =
                match SMap.find_opt type_name env.trait_impls with
                | Some s -> s
                | None -> SSet.empty
              in
              if SSet.mem trait_name impl_set then trait_name :: acc else acc
            else acc)
          env.traits []
      in
      if List.length providing_traits > 1 then
        error_at method_name.span
          "ambiguous method '%s' on type %s: provided by traits %s"
          method_name.node (show_ty resolved_recv)
          (String.concat ", " (List.sort String.compare providing_traits));
      match SMap.find_opt type_name env.impls with
      | Some ii -> (
          match SMap.find_opt method_name.node ii.ii_methods with
          | Some mi ->
              if mi.mi_is_mut then check_mutability env receiver method_name;
              check_fn_args ~env ~span:method_name.span ~name:method_name.node
                mi.mi_params arg_types;
              (* Ownership: consuming self receiver moves the receiver binding *)
              if mi.mi_consumes_self then
                consume_receiver env method_name.span receiver resolved_recv;
              maybe_subst_self env mi.mi_ret
          | None ->
              error_at method_name.span "no method '%s' on type %s"
                method_name.node (show_ty resolved_recv))
      | None ->
          error_at method_name.span "no method '%s' on type %s" method_name.node
            (show_ty resolved_recv))

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
  (* Resolve TSelf to concrete type *)
  let t =
    match t with
    | TSelf -> ( match env.self_ty with Some st -> st | None -> t)
    | _ -> t
  in
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
  | TImported (pkg, tname) -> (
      match stdlib_receiver_fields pkg tname field.node with
      | Some sfi -> sfi.sfi_ty
      | None -> (
          match is_wrong_case_receiver pkg tname field.node with
          | Some correct ->
              error_at field.span
                "wrong case: '%s' should be '%s' (rgo uses snake_case for \
                 fields)"
                field.node correct
          | None ->
              error_at field.span "no field '%s' on type %s::%s" field.node pkg
                tname))
  | _ -> error_at field.span "field access on non-struct type %s" (show_ty t)

and check_path env type_name member =
  (* If this is an imported package, resolve stdlib member *)
  if SMap.mem type_name.node env.imported_packages then
    check_stdlib_path env type_name member
  else
    (* Type::Variant or Type::assoc_fn (without call) *)
    let try_assoc_fn () =
      match SMap.find_opt type_name.node env.impls with
      | Some ii -> (
          match SMap.find_opt member.node ii.ii_assoc_fns with
          | Some fi ->
              (* Substitute Self with the concrete type so stored
                 associated-function values carry the resolved type
                 (e.g. let make = Status::default; make().is_active()). *)
              let concrete_self = concrete_type_for_name env type_name.node in
              Some
                (TFn
                   ( List.map (subst_self concrete_self) fi.fi_params,
                     subst_self concrete_self fi.fi_ret ))
          | None -> None)
      | None -> None
    in
    match SMap.find_opt type_name.node env.enums with
    | Some ei -> (
        match List.assoc_opt member.node ei.ei_variants with
        | Some VUnit ->
            (* Inherit type args from Self inside an impl block for the same enum *)
            let type_args =
              match (ei.ei_tparams, env.self_ty) with
              | [], _ -> []
              | _, Some (TEnum (sn, (_ :: _ as args))) when sn = type_name.node
                ->
                  args
              | tparams, _ -> List.map (fun tp -> TParam tp) tparams
            in
            TEnum (type_name.node, type_args)
        | Some _ ->
            error_at member.span "variant '%s::%s' requires arguments"
              type_name.node member.node
        | None -> (
            (* Not a variant — try associated function *)
            match try_assoc_fn () with
            | Some t -> t
            | None ->
                error_at member.span "undefined variant '%s' in '%s'"
                  member.node type_name.node))
    | None -> (
        match try_assoc_fn () with
        | Some t -> t
        | None -> error_at type_name.span "unknown type '%s'" type_name.node)

and check_struct_literal env ty fields =
  let type_name =
    match ty with
    | TyName name -> name
    | TyGeneric (name, _) -> name
    | _ -> failwith "typecheck: bad struct literal type"
  in
  let resolved =
    let r = resolve_ast_ty env (ty : Ast.ty) in
    (* If the struct literal uses a bare name like `Box { ... }` inside
       an impl block for `Box<T>`, inherit the generic args from Self. *)
    match (r, env.self_ty) with
    | TStruct (n, []), Some (TStruct (sn, (_ :: _ as args))) when n = sn ->
        TStruct (n, args)
    | _ -> r
  in
  (match SMap.find_opt type_name.node env.structs with
  | Some si ->
      List.iter
        (fun (sf : struct_field_init) ->
          match
            List.find_opt (fun (n, _, _) -> n = sf.sf_name.node) si.si_fields
          with
          | Some (_, expected, _) ->
              let actual = check_expr env sf.sf_expr in
              expect_type ~env ~span:sf.sf_name.span ~expected ~actual
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

and check_struct_variant_literal env type_name variant_name fields =
  match SMap.find_opt type_name.node env.enums with
  | Some ei -> (
      match List.assoc_opt variant_name.node ei.ei_variants with
      | Some (VStruct expected_fields) -> (
          let field_actuals =
            List.map
              (fun (sf : struct_field_init) ->
                let actual = check_expr env sf.sf_expr in
                (match
                   List.find_opt
                     (fun (n, _) -> n = sf.sf_name.node)
                     expected_fields
                 with
                | Some (_, expected) ->
                    if
                      not
                        (types_compatible ~type_params:ei.ei_tparams expected
                           actual)
                    then
                      error_at sf.sf_name.span
                        "type mismatch: expected %s, got %s" (show_ty expected)
                        (show_ty actual)
                | None ->
                    error_at sf.sf_name.span "no field '%s' in variant '%s::%s'"
                      sf.sf_name.node type_name.node variant_name.node);
                actual)
              fields
          in
          (* Infer concrete type args for generic enums from struct fields *)
          let base = TEnum (type_name.node, []) in
          match (base, env.self_ty, ei.ei_tparams) with
          | TEnum (n, []), Some (TEnum (sn, (_ :: _ as args))), _ when n = sn ->
              TEnum (n, args)
          | TEnum (n, []), _, _ :: _ ->
              let field_formals =
                List.filter_map
                  (fun (sf : struct_field_init) ->
                    match
                      List.find_opt
                        (fun (n, _) -> n = sf.sf_name.node)
                        expected_fields
                    with
                    | Some (_, ty) -> Some ty
                    | None -> None)
                  fields
              in
              let bindings =
                infer_tparam_bindings ~span:variant_name.span field_formals
                  field_actuals
              in
              let type_args =
                List.map
                  (fun tp ->
                    match List.assoc_opt tp bindings with
                    | Some t -> t
                    | None -> TParam tp)
                  ei.ei_tparams
              in
              TEnum (n, type_args)
          | _ -> base)
      | Some VUnit ->
          error_at variant_name.span
            "unit variant '%s::%s' does not have named fields" type_name.node
            variant_name.node
      | Some (VTuple _) ->
          error_at variant_name.span
            "tuple variant '%s::%s' does not have named fields" type_name.node
            variant_name.node
      | None ->
          error_at variant_name.span "undefined variant '%s' in '%s'"
            variant_name.node type_name.node)
  | None -> error_at type_name.span "unknown enum '%s'" type_name.node

and check_if env cond then_blk else_blk =
  let ct = check_expr env cond in
  expect_type ~env ~span:(expr_span cond) ~expected:TBool ~actual:ct;
  let then_ty = check_block env then_blk in
  match else_blk with
  | Some else_b ->
      let else_ty = check_block env else_b in
      (* If both branches produce a value, types must match *)
      if then_ty <> TVoid && else_ty <> TVoid then
        expect_type ~env ~span:dummy_span ~expected:then_ty ~actual:else_ty;
      then_ty
  | None -> TVoid

(* Check whether a pattern would partially move a non-Copy payload out of
   a user-defined enum.  Called only when the scrutinee is a non-Copy enum. *)
and check_pattern_partial_move env scrutinee_ty (p : pat) : unit =
  match p with
  | PatTuple (enum_name, variant_name, pats) -> (
      match SMap.find_opt enum_name.node env.enums with
      | Some ei -> (
          match List.assoc_opt variant_name.node ei.ei_variants with
          | Some (VTuple field_tys) ->
              List.iter2
                (fun p ty ->
                  if pat_has_bindings p && is_partial_move_type env ty then
                    error_at variant_name.span
                      "cannot destructure non-Copy enum payload -- partial \
                       moves are not supported")
                pats field_tys
          | _ -> ())
      | None -> (
          (* For built-in Result/Option: check inner payload types *)
          let inner_ty =
            match (enum_name.node, variant_name.node, scrutinee_ty) with
            | "Result", "Ok", TResult (ok_ty, _) -> Some ok_ty
            | "Result", "Err", TResult (_, err_ty) -> Some err_ty
            | "Option", "Some", TOption ty -> Some ty
            | _ -> None
          in
          match inner_ty with
          | Some ty ->
              List.iter
                (fun p ->
                  if pat_has_bindings p && is_destructure_move_type env ty then
                    error_at variant_name.span
                      "cannot destructure non-Copy enum payload -- partial \
                       moves are not supported")
                pats
          | None -> ()))
  | PatStruct (_enum_name, variant_name, field_pats) -> (
      match SMap.find_opt _enum_name.node env.enums with
      | Some ei -> (
          match List.assoc_opt variant_name.node ei.ei_variants with
          | Some (VStruct fields) ->
              List.iter
                (fun (fp : field_pat) ->
                  let has_bind =
                    match fp.fp_pat with
                    | Some p -> pat_has_bindings p
                    | None -> true
                  in
                  if has_bind then
                    match List.assoc_opt fp.fp_name.node fields with
                    | Some ty when is_partial_move_type env ty ->
                        error_at variant_name.span
                          "cannot destructure non-Copy enum payload -- partial \
                           moves are not supported"
                    | _ -> ())
                field_pats
          | _ -> ())
      | None -> ())
  | _ -> ()

and check_match env scrutinee arms =
  let scrutinee_ty = check_expr env scrutinee in
  (* Ownership: reject enum-payload pattern destructuring that would partially
     move a non-Copy payload out of an owned enum binding.
     Applies to user-defined enums, built-in Option, and built-in Result.
     For ExprSelf, only check when self is consumed (by-value receiver);
     borrowed self (&self/&mut self) does not move ownership. *)
  let check_enum_partial_move =
    match (scrutinee, scrutinee_ty) with
    | ExprSelf, _ -> env.self_consumed
    | _, (TEnum (_, _) | TOption _ | TResult (_, _))
      when not (is_copy_type env scrutinee_ty) ->
        true
    | _ -> false
  in
  if check_enum_partial_move then
    List.iter
      (fun (arm : match_arm) ->
        check_pattern_partial_move env scrutinee_ty arm.arm_pat)
      arms;
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
        (fun t -> expect_type ~env ~span:dummy_span ~expected:first ~actual:t)
        rest;
      first

(* Check if a pattern extracts data (has any binding, not just wildcards). *)
and pat_has_bindings (p : pat) : bool =
  match p with
  | PatWild -> false
  | PatLit _ -> false
  | PatBind _ -> true
  | PatTuple (_, _, pats) -> List.exists pat_has_bindings pats
  | PatStruct (_, _, fps) ->
      List.exists
        (fun (fp : field_pat) ->
          match fp.fp_pat with Some p -> pat_has_bindings p | None -> true)
        fps

and bind_pattern env scrutinee_ty (p : pat) : env =
  match p with
  | PatWild -> env
  | PatBind name -> add_value name.node scrutinee_ty ~is_mut:false env
  | PatLit _ -> env
  | PatTuple (enum_name, variant_name, pats)
    when enum_name.node = "Result" || enum_name.node = "Option" -> (
      match (enum_name.node, variant_name.node, scrutinee_ty, pats) with
      | "Result", "Ok", TResult (ok_ty, _), [ inner_pat ] ->
          bind_pattern env ok_ty inner_pat
      | "Result", "Ok", TResult _, [] -> env
      | "Result", "Err", TResult (_, err_ty), [ inner_pat ] ->
          bind_pattern env err_ty inner_pat
      | "Result", "Err", TResult _, [] -> env
      | "Option", "Some", TOption inner_ty, [ inner_pat ] ->
          bind_pattern env inner_ty inner_pat
      | "Option", "Some", TOption _, [] -> env
      | "Option", "None", TOption _, [] -> env
      | "Result", ("Ok" | "Err"), TResult _, _ ->
          error_at variant_name.span "variant '%s::%s' expects at most 1 field"
            enum_name.node variant_name.node
      | "Option", "Some", TOption _, _ ->
          error_at variant_name.span
            "variant 'Option::Some' expects at most 1 field"
      | "Option", "None", TOption _, _ ->
          error_at variant_name.span "variant 'Option::None' expects 0 fields"
      | "Result", _, _, _ | "Option", _, _, _ ->
          error_at enum_name.span
            "pattern '%s::%s' does not match scrutinee type %s" enum_name.node
            variant_name.node (show_ty scrutinee_ty)
      | _ -> env)
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
            expect_type ~env ~span:(expr_span init) ~expected:dt ~actual:init_ty;
            dt
        | None, _ -> check_expr env init
      in
      (* Reject binding void expressions to variables *)
      (match (declared_ty, pat) with
      | TVoid, PatBind name ->
          error_at name.span "cannot bind result of void expression to '%s'"
            name.node
      | TVoid, _ ->
          error_at (expr_span init) "cannot bind result of void expression"
      | _ -> ());
      (* Ownership: if RHS is a simple identifier with a non-Copy type,
         mark the source as moved.  Reject partial moves from fields. *)
      (match init with
      | ExprIdent src_name ->
          consume_if_non_copy env src_name.span src_name.node declared_ty
      | ExprFieldAccess (_, field) when is_partial_move_type env declared_ty ->
          error_at field.span
            "cannot move out of field '%s' -- partial moves are not supported"
            field.node
      | _ -> ());
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
  | Some expected, Some e -> (
      let actual = check_expr env e in
      (* Reject returning void expressions — they have no value to return *)
      if actual = TVoid then
        error_at (expr_span e) "cannot return void expression"
      else expect_type ~env ~span:(expr_span e) ~expected ~actual;
      (* Ownership: returning a non-Copy binding by value marks it moved so
         any later use in the same scope is diagnosed as use-after-move. *)
      match e with
      | ExprIdent name -> consume_if_non_copy env name.span name.node actual
      | _ -> ())
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
  expect_type ~env ~span:(expr_span rhs) ~expected:lhs_ty ~actual:rhs_ty;
  (* Ownership: if RHS is a simple identifier with a non-Copy type,
     mark the source as moved.  Reject partial moves from fields. *)
  (match rhs with
  | ExprIdent src_name ->
      consume_if_non_copy env src_name.span src_name.node rhs_ty
  | ExprFieldAccess (_, field) when is_partial_move_type env rhs_ty ->
      error_at field.span
        "cannot move out of field '%s' -- partial moves are not supported"
        field.node
  | _ -> ());
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
          expect_type ~env ~span:(expr_span e) ~expected:first_ty ~actual:t)
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
          let ei = { ei_variants = variants; ei_tparams = generics } in
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
          let bounds =
            List.filter_map
              (fun (tp : type_param) ->
                match tp.tp_bound with
                | None | Some [] -> None
                | Some bs ->
                    Some
                      ( tp.tp_name.node,
                        List.map (fun (b : ident located) -> b.node) bs ))
              fd.fn_generics
          in
          let fi = { fi_params = params; fi_ret = ret; fi_bounds = bounds } in
          { env with fns = SMap.add fd.fn_name.node fi env.fns }
      | ItemImpl { i_ty; i_items; i_generics } ->
          collect_impl env i_generics i_ty i_items
      | ItemTraitImpl { ti_ty; ti_items; ti_generics; ti_trait } -> (
          (* Check for duplicate trait impl *)
          let type_name =
            match ti_ty with
            | TyName name -> name.node
            | TyGeneric (name, _) -> name.node
            | _ -> "_"
          in
          let existing_traits =
            match SMap.find_opt type_name env.trait_impls with
            | Some s -> s
            | None -> SSet.empty
          in
          if SSet.mem ti_trait.node existing_traits then
            error_at ti_trait.span "duplicate impl of trait '%s' for type '%s'"
              ti_trait.node type_name;
          let updated_traits = SSet.add ti_trait.node existing_traits in
          (* Ownership: reject Drop + Copy overlap *)
          if SSet.mem "Drop" updated_traits && SSet.mem "Copy" updated_traits
          then
            error_at ti_trait.span
              "type '%s' cannot implement both Drop and Copy" type_name;
          let env =
            {
              env with
              trait_impls = SMap.add type_name updated_traits env.trait_impls;
            }
          in
          (* Include default methods from the trait that aren't overridden *)
          let provided_names =
            List.map (fun (fd : fn_decl) -> fd.fn_name.node) ti_items
          in
          let default_methods =
            match SMap.find_opt ti_trait.node env.traits with
            | Some tr_info ->
                SMap.fold
                  (fun mname tms acc ->
                    if
                      tms.tms_has_default && not (List.mem mname provided_names)
                    then
                      (* Register as a method_info in the impl *)
                      let mi =
                        {
                          mi_params = tms.tms_params;
                          mi_ret = tms.tms_ret;
                          mi_is_mut =
                            (match tms.tms_self with
                            | Some SelfMutRef -> true
                            | _ -> false);
                          mi_consumes_self =
                            (match tms.tms_self with
                            | Some SelfValue -> true
                            | _ -> false);
                        }
                      in
                      (mname, mi) :: acc
                    else acc)
                  tr_info.tr_methods []
            | None -> []
          in
          let env = collect_impl env ti_generics ti_ty ti_items in
          (* Add default methods to the impl *)
          match default_methods with
          | [] -> env
          | _ ->
              let impl_type_name =
                match ti_ty with
                | TyName n -> n.node
                | TyGeneric (n, _) -> n.node
                | _ -> "_"
              in
              let existing =
                match SMap.find_opt impl_type_name env.impls with
                | Some ii -> ii
                | None ->
                    {
                      ii_methods = SMap.empty;
                      ii_assoc_fns = SMap.empty;
                      ii_type_params = [];
                      ii_self_ty = TVoid;
                    }
              in
              let updated =
                List.fold_left
                  (fun ii (mname, mi) ->
                    { ii with ii_methods = SMap.add mname mi ii.ii_methods })
                  existing default_methods
              in
              { env with impls = SMap.add impl_type_name updated env.impls })
      | ItemTrait { t_name; t_generics; t_items; _ } ->
          let generics =
            List.map (fun (tp : type_param) -> tp.tp_name.node) t_generics
          in
          let env_with_generics =
            {
              env with
              type_params = generics @ env.type_params;
              self_ty = Some TSelf;
            }
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
          let ti = { tr_generics = generics; tr_methods = methods } in
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
    | None ->
        {
          ii_methods = SMap.empty;
          ii_assoc_fns = SMap.empty;
          ii_type_params = generics;
          ii_self_ty = self_ty;
        }
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
            let consumes =
              match self_param with SelfValue -> true | _ -> false
            in
            let mi =
              {
                mi_params = params;
                mi_ret = ret;
                mi_is_mut = is_mut;
                mi_consumes_self = consumes;
              }
            in
            { ii with ii_methods = SMap.add fd.fn_name.node mi ii.ii_methods }
        | None ->
            let fi = { fi_params = params; fi_ret = ret; fi_bounds = [] } in
            {
              ii with
              ii_assoc_fns = SMap.add fd.fn_name.node fi ii.ii_assoc_fns;
            })
      existing impl_items
  in
  { env with impls = SMap.add type_name updated env.impls }

let bounds_of_generics base (generics : type_param list) =
  List.fold_left
    (fun acc (tp : type_param) ->
      match tp.tp_bound with
      | None | Some [] -> acc
      | Some bs ->
          SMap.add tp.tp_name.node
            (List.map (fun (b : ident located) -> b.node) bs)
            acc)
    base generics

let check_fn_decl env (fd : fn_decl) =
  let generics =
    List.map (fun (tp : type_param) -> tp.tp_name.node) fd.fn_generics
  in
  let bounds_map =
    List.fold_left
      (fun acc (tp : type_param) ->
        match tp.tp_bound with
        | None | Some [] -> acc
        | Some bs ->
            SMap.add tp.tp_name.node
              (List.map (fun (b : ident located) -> b.node) bs)
              acc)
      env.param_bounds fd.fn_generics
  in
  let fn_env =
    {
      env with
      type_params = generics @ env.type_params;
      param_bounds = bounds_map;
      moved = ref SSet.empty;
      (* fresh move set per function body *)
    }
  in
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
    expect_type ~env:fn_env ~span:fd.fn_name.span ~expected:resolved_ret
      ~actual:body_ty

let check_item env (item : item) =
  match item with
  | ItemFn fd -> check_fn_decl env fd
  | ItemStruct _ -> () (* type info collected in first pass *)
  | ItemEnum _ -> ()
  | ItemImpl { i_ty; i_items; i_generics } ->
      let generics =
        List.map (fun (tp : type_param) -> tp.tp_name.node) i_generics
      in
      let outer_bounds = bounds_of_generics env.param_bounds i_generics in
      let impl_env =
        {
          env with
          type_params = generics @ env.type_params;
          param_bounds = outer_bounds;
        }
      in
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
                let consumed =
                  match self_p with SelfValue -> true | _ -> false
                in
                let env_with_self = add_value "self" self_ty ~is_mut impl_env in
                { env_with_self with self_consumed = consumed }
            | None -> impl_env
          in
          check_fn_decl method_env fd)
        i_items
  | ItemTraitImpl { ti_trait; ti_ty; ti_items; ti_generics } -> (
      let generics =
        List.map (fun (tp : type_param) -> tp.tp_name.node) ti_generics
      in
      let outer_bounds = bounds_of_generics env.param_bounds ti_generics in
      let impl_env =
        {
          env with
          type_params = generics @ env.type_params;
          param_bounds = outer_bounds;
        }
      in
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
                let consumed =
                  match self_p with SelfValue -> true | _ -> false
                in
                let env_with_self = add_value "self" self_ty ~is_mut impl_env in
                { env_with_self with self_consumed = consumed }
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
          (* Check signature compatibility for provided methods.
             Include both impl and trait generic names so TParam is
             treated as a wildcard for both sides. *)
          let sig_type_params = tr_info.tr_generics @ impl_env.type_params in
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
                        if
                          not
                            (types_compatible ~type_params:sig_type_params exp
                               act)
                        then
                          error_at fd.fn_name.span
                            "signature mismatch for method '%s' in impl %s: \
                             parameter type mismatch, expected %s, got %s"
                            fd.fn_name.node ti_trait.node (show_ty exp)
                            (show_ty act))
                      tms.tms_params impl_params;
                    if
                      not
                        (types_compatible ~type_params:sig_type_params
                           tms.tms_ret impl_ret)
                    then
                      error_at fd.fn_name.span
                        "signature mismatch for method '%s' in impl %s: return \
                         type mismatch, expected %s, got %s"
                        fd.fn_name.node ti_trait.node (show_ty tms.tms_ret)
                        (show_ty impl_ret)))
            ti_items)
  | ItemTrait { t_name; t_generics; t_items; _ } ->
      let generics =
        List.map (fun (tp : type_param) -> tp.tp_name.node) t_generics
      in
      let outer_bounds = bounds_of_generics env.param_bounds t_generics in
      let trait_env =
        {
          env with
          type_params = generics @ env.type_params;
          param_bounds = outer_bounds;
          self_ty = Some TSelf;
          current_trait = Some t_name.node;
        }
      in
      List.iter
        (fun ti ->
          match ti with
          | TraitFnDecl fd ->
              let method_env =
                match fd.fn_self with
                | Some self_p ->
                    let is_mut =
                      match self_p with SelfMutRef -> true | _ -> false
                    in
                    let consumed =
                      match self_p with SelfValue -> true | _ -> false
                    in
                    let env_with_self =
                      add_value "self" TSelf ~is_mut trait_env
                    in
                    { env_with_self with self_consumed = consumed }
                | None -> trait_env
              in
              check_fn_decl method_env fd
          | TraitFnSig _ -> ())
        t_items

(* ---------- main entry points ---------- *)

let typecheck_exn (prog : program) : program =
  let env = empty_env in
  (* Register imported packages for member-aware diagnostics *)
  let env =
    List.fold_left
      (fun e (imp : import_path) ->
        let segments =
          List.map (fun (s : ident located) -> s.node) imp.imp_segments
        in
        let alias =
          match Resolver.alias_for_path segments with
          | Some a -> a
          | None ->
              (List.nth imp.imp_segments (List.length imp.imp_segments - 1))
                .node
        in
        {
          e with
          imported_packages = SMap.add alias segments e.imported_packages;
        })
      env prog.imports
  in
  let env = collect_type_info env prog.items in
  List.iter (check_item env) prog.items;
  prog

let typecheck (prog : program) : (program, string) result =
  try Ok (typecheck_exn prog)
  with Typecheck_error { msg; line; col } ->
    Error (Printf.sprintf "%d:%d: %s" line col msg)
