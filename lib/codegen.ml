(* Naive code generation for the hello-world subset.
   This will be replaced by a full typed codegen in Phase 7. *)

let buf_add_string_escaped buf s =
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
  Buffer.add_char buf '"'

let gen_expr buf (e : Ast.expr) =
  match e with
  | ExprLit (LitString s) -> buf_add_string_escaped buf s
  | ExprLit (LitInt s) -> Buffer.add_string buf s
  | ExprLit (LitBool true) -> Buffer.add_string buf "true"
  | ExprLit (LitBool false) -> Buffer.add_string buf "false"
  | _ -> failwith "codegen: unsupported expression"

let gen_stmt buf (s : Ast.stmt) =
  match s with
  | StmtExpr (ExprCall (ExprIdent { node = "println"; _ }, args)) ->
      Buffer.add_string buf "\tfmt.Println(";
      List.iteri
        (fun i a ->
          if i > 0 then Buffer.add_string buf ", ";
          gen_expr buf a)
        args;
      Buffer.add_string buf ")\n"
  | _ -> failwith "codegen: unsupported statement"

let needs_fmt (prog : Ast.program) =
  let check_stmt = function
    | Ast.StmtExpr (ExprCall (ExprIdent { node = "println"; _ }, _)) -> true
    | _ -> false
  in
  let check_item = function
    | Ast.ItemFn fd -> List.exists check_stmt fd.fn_body.stmts
    | _ -> false
  in
  List.exists check_item prog.items

let generate (prog : Ast.program) : string =
  let buf = Buffer.create 256 in
  Buffer.add_string buf "package main\n";
  if needs_fmt prog then Buffer.add_string buf "\nimport \"fmt\"\n";
  List.iter
    (fun (item : Ast.item) ->
      match item with
      | ItemFn fd when fd.fn_name.node = "main" ->
          Buffer.add_string buf "\nfunc main() {\n";
          List.iter (gen_stmt buf) fd.fn_body.stmts;
          Buffer.add_string buf "}\n"
      | _ -> failwith "codegen: unsupported top-level item")
    prog.items;
  Buffer.contents buf
