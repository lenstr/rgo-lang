val generate : Ast.program -> string
(** [generate prog] emits Go source code for the given typed AST. Implements PRD
    u00a75.1u2013u00a75.12 mapping rules including structs, enums,
    Option/Result, ?, match, array literals, keyword escaping, and
    non-addressable pointer-receiver call-site handling. *)
