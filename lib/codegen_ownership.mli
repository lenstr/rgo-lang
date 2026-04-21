(** Ownership cleanup and guard-suppression helpers for codegen. *)

val type_nominal_name : Ast.ty -> string option
val is_drop_type : has_trait_impl:(string -> bool) -> Ast.ty -> bool
val is_non_consuming_builtin : string -> bool

val collect_consumed_idents :
  is_user_fn:(string -> bool) ->
  is_user_method:(string -> string -> bool) ->
  is_stdlib_call:(string -> string -> bool) ->
  lookup_value_ty:(string -> Ast.ty option) ->
  Ast.expr ->
  string list

val suppress_consumed_guards :
  lookup_guard:(string -> string option) ->
  is_user_fn:(string -> bool) ->
  is_user_method:(string -> string -> bool) ->
  is_stdlib_call:(string -> string -> bool) ->
  lookup_value_ty:(string -> Ast.ty option) ->
  Buffer.t ->
  string ->
  Ast.expr ->
  unit

val suppress_consumed_guards_inline :
  lookup_guard:(string -> string option) ->
  is_user_fn:(string -> bool) ->
  is_user_method:(string -> string -> bool) ->
  is_stdlib_call:(string -> string -> bool) ->
  lookup_value_ty:(string -> Ast.ty option) ->
  Buffer.t ->
  string ->
  Ast.expr ->
  unit

val suppress_move_guard :
  lookup_guard:(string -> string option) -> Buffer.t -> string -> string -> unit

val emit_drop_defer :
  Buffer.t -> indent:string -> guard:string -> binding:string -> unit

val emit_param_drop_defer : Buffer.t -> guard:string -> binding:string -> unit
val emit_overwrite_drop : Buffer.t -> string -> string -> unit
