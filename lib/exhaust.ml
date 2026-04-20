(* Exhaustiveness checking for enum match expressions.
   Runs after typechecking, before codegen. Walks the AST
   and verifies that every match on an enum covers all variants
   (or includes a wildcard/catch-all pattern).

   The checker determines enum coverage from the scrutinee's type
   (resolved via function parameters, let-binding annotations,
   and expression analysis) rather than reconstructing the enum
   name solely from arm patterns. *)

open Ast

type exhaust_error = { msg : string; line : int; col : int }

exception Exhaust_error of exhaust_error

let error_at line col fmt =
  Printf.ksprintf (fun msg -> raise (Exhaust_error { msg; line; col })) fmt

(* Collect all enum declarations: name -> list of variant names *)
let collect_enums (prog : program) : (string * string list) list =
  List.filter_map
    (fun item ->
      match item with
      | ItemEnum { e_name; e_variants; _ } ->
          let names =
            List.map (fun (v : variant) -> v.var_name.node) e_variants
          in
          Some (e_name.node, names)
      | _ -> None)
    prog.items

(* Collect function return types: fn_name -> type name (if enum) *)
let collect_fn_ret_types (prog : program) : (string * string) list =
  List.filter_map
    (fun item ->
      match item with
      | ItemFn { fn_name; fn_ret = Some (TyName ret_name); _ } ->
          Some (fn_name.node, ret_name.node)
      | _ -> None)
    prog.items

module SMap = Map.Make (String)
module SSet = Set.Make (String)

(* Type environment: variable name -> type name (for enum resolution) *)
type tenv = string SMap.t

(* Extract the base type name from an AST type *)
let ty_base_name (t : ty) : string option =
  match t with
  | TyName n -> Some n.node
  | TyGeneric (n, _) -> Some n.node
  | _ -> None

(* Determine whether a pattern is a catch-all (wildcard or binding) *)
let is_catchall (p : pat) : bool =
  match p with PatWild | PatBind _ -> true | _ -> false

(* Extract the enum name from a pattern, if it's an enum variant pattern *)
let pat_enum_name (p : pat) : string option =
  match p with
  | PatTuple (enum_name, _, _) -> Some enum_name.node
  | PatStruct (enum_name, _, _) -> Some enum_name.node
  | _ -> None

(* Extract the variant name from a pattern *)
let pat_variant_name (p : pat) : string option =
  match p with
  | PatTuple (_, variant_name, _) -> Some variant_name.node
  | PatStruct (_, variant_name, _) -> Some variant_name.node
  | _ -> None

(* Get position info from a match expression's scrutinee or first arm *)
let match_position (scrutinee : expr) (arms : match_arm list) : int * int =
  match scrutinee with
  | ExprIdent id -> (id.span.start.line, id.span.start.col)
  | ExprPath (id, _) -> (id.span.start.line, id.span.start.col)
  | ExprFieldAccess (_, id) -> (id.span.start.line, id.span.start.col)
  | ExprMethodCall (_, id, _) -> (id.span.start.line, id.span.start.col)
  | _ -> (
      match arms with
      | { arm_pat = PatTuple (id, _, _); _ } :: _ ->
          (id.span.start.line, id.span.start.col)
      | { arm_pat = PatStruct (id, _, _); _ } :: _ ->
          (id.span.start.line, id.span.start.col)
      | { arm_pat = PatBind id; _ } :: _ ->
          (id.span.start.line, id.span.start.col)
      | _ -> (0, 0))

(* Infer the type name of an expression from authoritative sources only.
   Follows block expressions (whose final expression determines the type)
   but does NOT sample individual branches of if-expressions or arms of
   match-expressions, because those are multi-branch forms whose type
   cannot be reliably determined by examining a single branch. *)
let rec expr_type_name tenv fn_ret_types (e : expr) : string option =
  match e with
  | ExprIdent id -> SMap.find_opt id.node tenv
  | ExprPath (type_name, _) -> Some type_name.node
  | ExprCall (ExprIdent fn_name, _) -> List.assoc_opt fn_name.node fn_ret_types
  | ExprBlock { final_expr = Some fe; _ } -> expr_type_name tenv fn_ret_types fe
  | _ -> None

(* Resolve the scrutinee expression to an enum name using the type env *)
let scrutinee_enum_name tenv fn_ret_types (scrutinee : expr) : string option =
  match scrutinee with
  | ExprIdent id -> SMap.find_opt id.node tenv
  | ExprCall (ExprIdent fn_name, _) -> List.assoc_opt fn_name.node fn_ret_types
  | ExprPath (type_name, _) -> Some type_name.node
  | _ -> expr_type_name tenv fn_ret_types scrutinee

(* Check a single match expression for exhaustiveness.
   Uses the scrutinee's type (from tenv) to determine which enum is matched,
   falling back to pattern-based extraction. *)
let check_match_expr enums tenv fn_ret_types scrutinee (arms : match_arm list) :
    unit =
  (* If any arm is a catch-all, the match is exhaustive *)
  if List.exists (fun (a : match_arm) -> is_catchall a.arm_pat) arms then ()
  else
    (* First try to determine the enum from the scrutinee's type *)
    let enum_name =
      match scrutinee_enum_name tenv fn_ret_types scrutinee with
      | Some name when List.assoc_opt name enums <> None -> Some name
      | _ ->
          (* Fall back to extracting from arm patterns *)
          List.find_map (fun (a : match_arm) -> pat_enum_name a.arm_pat) arms
    in
    match enum_name with
    | None -> () (* Not an enum match *)
    | Some ename -> (
        match List.assoc_opt ename enums with
        | None -> ()
        | Some all_variants ->
            let covered =
              List.fold_left
                (fun acc (a : match_arm) ->
                  match pat_variant_name a.arm_pat with
                  | Some vname -> SSet.add vname acc
                  | None -> acc)
                SSet.empty arms
            in
            let all_set = SSet.of_list all_variants in
            let missing = SSet.diff all_set covered in
            if not (SSet.is_empty missing) then
              let line, col = match_position scrutinee arms in
              let missing_list =
                SSet.elements missing
                |> List.map (fun v -> ename ^ "::" ^ v)
                |> String.concat ", "
              in
              error_at line col "non-exhaustive match: missing pattern(s) %s"
                missing_list)

(* Walk all expressions in the AST looking for match expressions *)
let rec walk_expr enums tenv fn_ret_types (e : expr) : unit =
  match e with
  | ExprMatch (scrutinee, arms) ->
      check_match_expr enums tenv fn_ret_types scrutinee arms;
      walk_expr enums tenv fn_ret_types scrutinee;
      List.iter
        (fun (a : match_arm) -> walk_expr enums tenv fn_ret_types a.arm_expr)
        arms
  | ExprLit _ | ExprIdent _ | ExprSelf | ExprBreak | ExprContinue -> ()
  | ExprUnary (_, e) -> walk_expr enums tenv fn_ret_types e
  | ExprBinary (_, e1, e2) ->
      walk_expr enums tenv fn_ret_types e1;
      walk_expr enums tenv fn_ret_types e2
  | ExprCall (f, args) ->
      walk_expr enums tenv fn_ret_types f;
      List.iter (walk_expr enums tenv fn_ret_types) args
  | ExprMethodCall (recv, _, args) ->
      walk_expr enums tenv fn_ret_types recv;
      List.iter (walk_expr enums tenv fn_ret_types) args
  | ExprFieldAccess (e, _) -> walk_expr enums tenv fn_ret_types e
  | ExprPath (_, _) -> ()
  | ExprStruct (_, fields) ->
      List.iter
        (fun (sf : struct_field_init) ->
          walk_expr enums tenv fn_ret_types sf.sf_expr)
        fields
  | ExprStructVariant (_, _, fields) ->
      List.iter
        (fun (sf : struct_field_init) ->
          walk_expr enums tenv fn_ret_types sf.sf_expr)
        fields
  | ExprIf (cond, then_blk, else_blk) ->
      walk_expr enums tenv fn_ret_types cond;
      walk_block enums tenv fn_ret_types then_blk;
      Option.iter (walk_block enums tenv fn_ret_types) else_blk
  | ExprBlock blk -> walk_block enums tenv fn_ret_types blk
  | ExprReturn e_opt -> Option.iter (walk_expr enums tenv fn_ret_types) e_opt
  | ExprAssign (_, lhs, rhs) ->
      walk_expr enums tenv fn_ret_types lhs;
      walk_expr enums tenv fn_ret_types rhs
  | ExprQuestion e -> walk_expr enums tenv fn_ret_types e
  | ExprArray elems -> List.iter (walk_expr enums tenv fn_ret_types) elems
  | ExprRepeat (e, count) ->
      walk_expr enums tenv fn_ret_types e;
      walk_expr enums tenv fn_ret_types count
  | ExprIndex (e, idx) ->
      walk_expr enums tenv fn_ret_types e;
      walk_expr enums tenv fn_ret_types idx
  | ExprCast (e, _) -> walk_expr enums tenv fn_ret_types e
  | ExprLoop (cond, blk) ->
      Option.iter (walk_expr enums tenv fn_ret_types) cond;
      walk_block enums tenv fn_ret_types blk
  | ExprWhile (cond, blk) ->
      walk_expr enums tenv fn_ret_types cond;
      walk_block enums tenv fn_ret_types blk
  | ExprFor (_, iter, blk) ->
      walk_expr enums tenv fn_ret_types iter;
      walk_block enums tenv fn_ret_types blk

and walk_block enums tenv fn_ret_types (blk : block) : unit =
  let tenv =
    List.fold_left
      (fun acc s -> walk_stmt enums acc fn_ret_types s)
      tenv blk.stmts
  in
  Option.iter (walk_expr enums tenv fn_ret_types) blk.final_expr

and walk_stmt enums tenv fn_ret_types (s : stmt) : tenv =
  match s with
  | StmtLet { pat = PatBind name; ty; init; _ } -> (
      walk_expr enums tenv fn_ret_types init;
      (* Track the variable's type in the environment *)
      let type_name =
        match ty with
        | Some t -> ty_base_name t
        | None -> expr_type_name tenv fn_ret_types init
      in
      match type_name with
      | Some tn -> SMap.add name.node tn tenv
      | None -> tenv)
  | StmtLet { init; _ } ->
      walk_expr enums tenv fn_ret_types init;
      tenv
  | StmtExpr e ->
      walk_expr enums tenv fn_ret_types e;
      tenv

(* Build initial tenv from function parameters *)
let fn_param_tenv (params : param list) : tenv =
  List.fold_left
    (fun acc (p : param) ->
      match ty_base_name p.p_ty with
      | Some tn -> SMap.add p.p_name.node tn acc
      | None -> acc)
    SMap.empty params

let walk_fn_decl enums fn_ret_types (fd : fn_decl) : unit =
  let tenv = fn_param_tenv fd.fn_params in
  walk_block enums tenv fn_ret_types fd.fn_body

let walk_item enums fn_ret_types (item : item) : unit =
  match item with
  | ItemFn fd -> walk_fn_decl enums fn_ret_types fd
  | ItemImpl { i_items; _ } ->
      List.iter (walk_fn_decl enums fn_ret_types) i_items
  | ItemTraitImpl { ti_items; _ } ->
      List.iter (walk_fn_decl enums fn_ret_types) ti_items
  | ItemTrait { t_items; _ } ->
      List.iter
        (fun ti ->
          match ti with
          | TraitFnDecl fd -> walk_fn_decl enums fn_ret_types fd
          | _ -> ())
        t_items
  | ItemStruct _ | ItemEnum _ -> ()

let check_exn (prog : program) : program =
  let enums = collect_enums prog in
  let fn_ret_types = collect_fn_ret_types prog in
  List.iter (walk_item enums fn_ret_types) prog.items;
  prog

let check (prog : program) : (program, string) result =
  try Ok (check_exn prog)
  with Exhaust_error { msg; line; col } ->
    Error (Printf.sprintf "%d:%d: %s" line col msg)
