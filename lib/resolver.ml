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

(* ---------- import errors ---------- *)

type import_error_kind =
  | Malformed_import of string
  | Unsupported_external_package of string
  | Import_alias_collision of string * span
  | Missing_import of string

exception Import_error of { kind : import_error_kind; span : span }

let import_error_at span kind = raise (Import_error { kind; span })

(* ---------- supported stdlib packages ---------- *)

let supported_stdlib_packages = [ ([ "net"; "http" ], "http") ]

let is_stdlib_path segments =
  List.exists (fun (path, _alias) -> path = segments) supported_stdlib_packages

let alias_for_path segments =
  List.find_map
    (fun (path, alias) -> if path = segments then Some alias else None)
    supported_stdlib_packages

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
  traits : unit SMap.t; (* global trait namespace *)
  imported_packages : string list SMap.t;
      (* alias -> path segments, e.g. "http" -> ["net"; "http"] *)
}

let empty_env =
  {
    values = [ SMap.empty ];
    types = SMap.empty;
    fns = SMap.empty;
    traits = SMap.empty;
    imported_packages = SMap.empty;
  }

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

let add_trait (name : ident located) env =
  { env with traits = SMap.add name.node () env.traits }

let lookup_trait (name : string) env = SMap.mem name env.traits

let lookup_imported_package (name : string) env =
  SMap.mem name env.imported_packages

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

let builtin_fns =
  [
    "println"; "print"; "len"; "to_string"; "panic"; "Some"; "Ok"; "Err"; "None";
  ]

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

(* ---------- trait bound resolution ---------- *)

let resolve_bounds env (type_params : type_param list) =
  List.iter
    (fun tp ->
      match tp.tp_bound with
      | None -> ()
      | Some bounds ->
          List.iter
            (fun (bound : ident located) ->
              if not (lookup_trait bound.node env) then
                error_at bound.span "undefined trait '%s'" bound.node)
            bounds)
    type_params

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
  | TyPath (pkg, _member) ->
      (* Package-qualified type: check the package is imported *)
      if not (lookup_imported_package pkg.node env) then
        let is_known_alias =
          List.exists
            (fun (_path, alias) -> alias = pkg.node)
            supported_stdlib_packages
        in
        if is_known_alias then
          import_error_at pkg.span (Missing_import pkg.node)
        else error_at pkg.span "undefined type '%s'" pkg.node

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
  (* If left side is an imported package, defer to typechecking *)
  if lookup_imported_package enum_name.node env then ()
  else
    match enum_name.node with
    | "Option" ->
        if not (List.mem variant_name.node [ "Some"; "None" ]) then
          error_at variant_name.span "undefined variant '%s' in '%s'"
            variant_name.node enum_name.node
    | "Result" ->
        if not (List.mem variant_name.node [ "Ok"; "Err" ]) then
          error_at variant_name.span "undefined variant '%s' in '%s'"
            variant_name.node enum_name.node
    | _ -> (
        match lookup_type enum_name.node env with
        | None -> error_at enum_name.span "undefined type '%s'" enum_name.node
        | Some ti ->
            if not (List.mem variant_name.node ti.ti_variants) then
              error_at variant_name.span "undefined variant '%s' in '%s'"
                variant_name.node enum_name.node)

and resolve_path_ref env type_name _member_name =
  (* If the left-hand side is an imported package, the path is valid
     and member resolution is deferred to typechecking. *)
  if lookup_imported_package type_name.node env then ()
  else
    (* Type::Member can be either an enum variant or an associated function.
       At the resolution phase we only check that the type exists.
       Member validation (variant or associated function) is deferred to
       the type-checking phase where impl blocks are available. *)
    match lookup_type type_name.node env with
    | None ->
        (* Check if this could be a known stdlib package that's not imported *)
        let is_known_alias =
          List.exists
            (fun (_path, alias) -> alias = type_name.node)
            supported_stdlib_packages
        in
        if is_known_alias then
          import_error_at type_name.span (Missing_import type_name.node)
        else error_at type_name.span "undefined type '%s'" type_name.node
    | Some _ti -> ()

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
  | ExprStructVariant (type_name, variant_name, fields) ->
      resolve_path_ref env type_name variant_name;
      List.iter
        (fun (sf : struct_field_init) -> resolve_expr env sf.sf_expr)
        fields
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
  | StmtLet { pat; ty; init; _ } ->
      Option.iter (resolve_ty env) ty;
      resolve_expr env init;
      resolve_pat env pat
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
  (* Resolve trait bounds on type parameters *)
  resolve_bounds inner fd.fn_generics;
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
      | ItemFn fd ->
          if SMap.mem fd.fn_name.node e.imported_packages then
            import_error_at fd.fn_name.span
              (Import_alias_collision (fd.fn_name.node, fd.fn_name.span));
          add_fn fd.fn_name e
      | ItemStruct { s_name; s_fields; _ } ->
          if SMap.mem s_name.node e.imported_packages then
            import_error_at s_name.span
              (Import_alias_collision (s_name.node, s_name.span));
          let fields = List.map (fun (f : field) -> f.fd_name.node) s_fields in
          add_type s_name { ti_variants = []; ti_fields = fields } e
      | ItemEnum { e_name; e_variants; _ } ->
          if SMap.mem e_name.node e.imported_packages then
            import_error_at e_name.span
              (Import_alias_collision (e_name.node, e_name.span));
          let variants =
            List.map (fun (v : variant) -> v.var_name.node) e_variants
          in
          add_type e_name { ti_variants = variants; ti_fields = [] } e
      | ItemImpl _ | ItemTraitImpl _ -> e
      | ItemTrait { t_name; _ } ->
          if SMap.mem t_name.node e.imported_packages then
            import_error_at t_name.span
              (Import_alias_collision (t_name.node, t_name.span));
          add_trait t_name e)
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
      resolve_bounds inner s_generics;
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
      resolve_bounds inner e_generics;
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
      resolve_bounds inner i_generics;
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
  | ItemTraitImpl { ti_generics; ti_trait; ti_ty; ti_items } ->
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
      resolve_bounds inner ti_generics;
      if not (lookup_trait ti_trait.node env) then
        error_at ti_trait.span "undefined trait '%s'" ti_trait.node;
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
      resolve_bounds inner t_generics;
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

(* ---------- import resolution ---------- *)

let resolve_imports env (imports : import_path list) =
  List.fold_left
    (fun e (imp : import_path) ->
      let segments =
        List.map (fun (seg : ident located) -> seg.node) imp.imp_segments
      in
      (* Validate: must have at least 2 segments *)
      if List.length imp.imp_segments < 2 then
        import_error_at imp.imp_span
          (Malformed_import
             (Printf.sprintf "malformed import: expected 'use pkg::sub'"));
      (* Check if this is a supported stdlib path *)
      if not (is_stdlib_path segments) then
        import_error_at imp.imp_span
          (Unsupported_external_package
             (Printf.sprintf "unsupported external package '%s'"
                (String.concat "::" segments)));
      (* Get the alias (last segment) *)
      let alias =
        match alias_for_path segments with
        | Some a -> a
        | None -> List.nth segments (List.length segments - 1)
      in
      let alias_loc =
        List.nth imp.imp_segments (List.length imp.imp_segments - 1)
      in
      (* Check for collision with existing types, functions, or other imports *)
      if SMap.mem alias e.types then
        import_error_at alias_loc.span
          (Import_alias_collision (alias, alias_loc.span));
      if SMap.mem alias e.fns then
        import_error_at alias_loc.span
          (Import_alias_collision (alias, alias_loc.span));
      if SMap.mem alias e.imported_packages then
        import_error_at alias_loc.span
          (Import_alias_collision (alias, alias_loc.span));
      { e with imported_packages = SMap.add alias segments e.imported_packages })
    env imports

(* ---------- main entry point ---------- *)

let resolve_exn (prog : program) : program =
  let env = init_env () in
  let env = resolve_imports env prog.imports in
  let env = collect_globals env prog.items in
  List.iter (resolve_item env) prog.items;
  prog

let format_import_error (e : import_error_kind) =
  match e with
  | Malformed_import msg -> msg
  | Unsupported_external_package msg -> msg
  | Import_alias_collision (alias, _) ->
      Printf.sprintf "import alias '%s' collides with an existing declaration"
        alias
  | Missing_import pkg ->
      Printf.sprintf
        "package '%s' is not imported; add 'use ...::%s;' at the top of the \
         file"
        pkg pkg

let resolve (prog : program) : (program, string) result =
  try Ok (resolve_exn prog) with
  | Resolve_error { msg; line; col } ->
      Error (Printf.sprintf "%d:%d: %s" line col msg)
  | Import_error { kind; span } ->
      Error
        (Printf.sprintf "%d:%d: %s" span.start.line span.start.col
           (format_import_error kind))
