val generate : Ast.program -> string
(** [generate prog] emits Go source code for the given AST. Currently supports
    only the hello-world subset: a single [fn main()] calling [println] with
    string literals. *)
