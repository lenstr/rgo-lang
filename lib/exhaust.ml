(* Exhaustiveness checking for enum match expressions.
   Runs after typechecking, before codegen. Walks the AST
   and verifies that every match on an enum covers all variants
   (or includes a wildcard/catch-all pattern). *)

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

module SSet = Set.Make (String)

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
  (* Try to get position from the scrutinee *)
  match scrutinee with
  | ExprIdent id -> (id.span.start.line, id.span.start.col)
  | ExprPath (id, _) -> (id.span.start.line, id.span.start.col)
  | ExprFieldAccess (_, id) -> (id.span.start.line, id.span.start.col)
  | ExprMethodCall (_, id, _) -> (id.span.start.line, id.span.start.col)
  | _ -> (
      (* Fall back to first arm's pattern position *)
      match arms with
      | { arm_pat = PatTuple (id, _, _); _ } :: _ ->
          (id.span.start.line, id.span.start.col)
      | { arm_pat = PatStruct (id, _, _); _ } :: _ ->
          (id.span.start.line, id.span.start.col)
      | { arm_pat = PatBind id; _ } :: _ ->
          (id.span.start.line, id.span.start.col)
      | _ -> (0, 0))

(* Check a single match expression for exhaustiveness *)
let check_match_expr enums scrutinee (arms : match_arm list) : unit =
  (* If any arm is a catch-all, the match is exhaustive *)
  if List.exists (fun (a : match_arm) -> is_catchall a.arm_pat) arms then ()
  else
    (* Determine which enum is being matched by looking at patterns *)
    let enum_name =
      List.find_map (fun (a : match_arm) -> pat_enum_name a.arm_pat) arms
    in
    match enum_name with
    | None -> () (* No enum patterns, not an enum match — skip *)
    | Some ename -> (
        match List.assoc_opt ename enums with
        | None -> () (* Unknown enum, type checker already validated *)
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
let rec walk_expr enums (e : expr) : unit =
  match e with
  | ExprMatch (scrutinee, arms) ->
      check_match_expr enums scrutinee arms;
      walk_expr enums scrutinee;
      List.iter (fun (a : match_arm) -> walk_expr enums a.arm_expr) arms
  | ExprLit _ | ExprIdent _ | ExprSelf | ExprBreak | ExprContinue -> ()
  | ExprUnary (_, e) -> walk_expr enums e
  | ExprBinary (_, e1, e2) ->
      walk_expr enums e1;
      walk_expr enums e2
  | ExprCall (f, args) ->
      walk_expr enums f;
      List.iter (walk_expr enums) args
  | ExprMethodCall (recv, _, args) ->
      walk_expr enums recv;
      List.iter (walk_expr enums) args
  | ExprFieldAccess (e, _) -> walk_expr enums e
  | ExprPath (_, _) -> ()
  | ExprStruct (_, fields) ->
      List.iter
        (fun (sf : struct_field_init) -> walk_expr enums sf.sf_expr)
        fields
  | ExprIf (cond, then_blk, else_blk) ->
      walk_expr enums cond;
      walk_block enums then_blk;
      Option.iter (walk_block enums) else_blk
  | ExprBlock blk -> walk_block enums blk
  | ExprReturn e_opt -> Option.iter (walk_expr enums) e_opt
  | ExprAssign (_, lhs, rhs) ->
      walk_expr enums lhs;
      walk_expr enums rhs
  | ExprQuestion e -> walk_expr enums e
  | ExprArray elems -> List.iter (walk_expr enums) elems
  | ExprRepeat (e, count) ->
      walk_expr enums e;
      walk_expr enums count
  | ExprIndex (e, idx) ->
      walk_expr enums e;
      walk_expr enums idx
  | ExprCast (e, _) -> walk_expr enums e
  | ExprLoop (cond, blk) ->
      Option.iter (walk_expr enums) cond;
      walk_block enums blk
  | ExprWhile (cond, blk) ->
      walk_expr enums cond;
      walk_block enums blk
  | ExprFor (_, iter, blk) ->
      walk_expr enums iter;
      walk_block enums blk

and walk_block enums (blk : block) : unit =
  List.iter (walk_stmt enums) blk.stmts;
  Option.iter (walk_expr enums) blk.final_expr

and walk_stmt enums (s : stmt) : unit =
  match s with
  | StmtLet { init; _ } -> walk_expr enums init
  | StmtExpr e -> walk_expr enums e

let walk_fn_decl enums (fd : fn_decl) : unit = walk_block enums fd.fn_body

let walk_item enums (item : item) : unit =
  match item with
  | ItemFn fd -> walk_fn_decl enums fd
  | ItemImpl { i_items; _ } -> List.iter (walk_fn_decl enums) i_items
  | ItemTraitImpl { ti_items; _ } -> List.iter (walk_fn_decl enums) ti_items
  | ItemTrait { t_items; _ } ->
      List.iter
        (fun ti ->
          match ti with TraitFnDecl fd -> walk_fn_decl enums fd | _ -> ())
        t_items
  | ItemStruct _ | ItemEnum _ -> ()

let check_exn (prog : program) : program =
  let enums = collect_enums prog in
  List.iter (walk_item enums) prog.items;
  prog

let check (prog : program) : (program, string) result =
  try Ok (check_exn prog)
  with Exhaust_error { msg; line; col } ->
    Error (Printf.sprintf "%d:%d: %s" line col msg)
