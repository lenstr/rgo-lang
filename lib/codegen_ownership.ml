(* Ownership cleanup and guard-suppression helpers for codegen.
   Extracted from codegen.ml to keep ownership logic focused. *)

open Ast

(* ---------- type helpers ---------- *)

let type_nominal_name (t : Ast.ty) : string option =
  match t with
  | TyName { node = n; _ } -> Some n
  | TyGeneric ({ node = n; _ }, _) -> Some n
  | _ -> None

let is_drop_type ~(has_trait_impl : string -> bool) (t : Ast.ty) : bool =
  match type_nominal_name t with
  | Some n -> has_trait_impl ("Drop:" ^ n)
  | None -> false

(* ---------- non-consuming builtin classification ---------- *)

let is_non_consuming_builtin name =
  match name with
  | "println" | "print" | "panic" | "sqrt" | "abs" | "to_string" | "len"
  | "Some" | "Ok" | "Err" | "None" ->
      true
  | _ -> false

(* ---------- consumed-ident collection ---------- *)

(* Collect identifier names that are consumed as by-value call arguments.
   Only user-defined rgo function/method calls consume ownership;
   builtins, imported stdlib calls, and enum constructors do not. *)
let collect_consumed_idents ~(is_user_fn : string -> bool)
    ~(is_user_method : string -> string -> bool)
    ~(is_stdlib_call : string -> string -> bool)
    ~(lookup_value_ty : string -> Ast.ty option) (e : Ast.expr) : string list =
  let ident_names args =
    List.filter_map
      (fun arg ->
        match arg with ExprIdent { node = n; _ } -> Some n | _ -> None)
      args
  in
  match e with
  | ExprCall (ExprIdent { node = callee; _ }, args)
    when (not (is_non_consuming_builtin callee)) && is_user_fn callee ->
      ident_names args
  | ExprCall (ExprPath (type_name, fn_name), args) ->
      if is_stdlib_call type_name.node fn_name.node then []
      else ident_names args
  | ExprCall _ -> []
  | ExprMethodCall (receiver, method_name, args) ->
      (* Only user-defined methods consume; check impls registry *)
      let recv_type_name =
        match receiver with
        | ExprIdent { node = n; _ } -> (
            match lookup_value_ty n with
            | Some (Ast.TyName { node = tn; _ })
            | Some (Ast.TyGeneric ({ node = tn; _ }, _)) ->
                Some tn
            | _ -> None)
        | _ -> None
      in
      let is_user =
        match recv_type_name with
        | Some tn -> is_user_method tn method_name.Ast.node
        | None -> false
      in
      if is_user then ident_names args else []
  | _ -> []

(* ---------- guard suppression ---------- *)

(* After emitting a statement expression, suppress guards for any
   Drop-type identifiers that were consumed by by-value calls. *)
let suppress_consumed_guards ~(lookup_guard : string -> string option)
    ~(is_user_fn : string -> bool) ~(is_user_method : string -> string -> bool)
    ~(is_stdlib_call : string -> string -> bool)
    ~(lookup_value_ty : string -> Ast.ty option) buf indent (e : Ast.expr) :
    unit =
  let consumed =
    collect_consumed_idents ~is_user_fn ~is_user_method ~is_stdlib_call
      ~lookup_value_ty e
  in
  List.iter
    (fun name ->
      match lookup_guard name with
      | Some guard -> Printf.bprintf buf "\n%s%s = false" indent guard
      | None -> ())
    consumed

(* Inline variant: emits guard suppression without a leading newline,
   so it can be placed at the start of a fresh line. *)
let suppress_consumed_guards_inline ~(lookup_guard : string -> string option)
    ~(is_user_fn : string -> bool) ~(is_user_method : string -> string -> bool)
    ~(is_stdlib_call : string -> string -> bool)
    ~(lookup_value_ty : string -> Ast.ty option) buf indent (e : Ast.expr) :
    unit =
  let consumed =
    collect_consumed_idents ~is_user_fn ~is_user_method ~is_stdlib_call
      ~lookup_value_ty e
  in
  List.iter
    (fun name ->
      match lookup_guard name with
      | Some guard -> Printf.bprintf buf "%s%s = false\n" indent guard
      | None -> ())
    consumed

(* Suppress a single guard for a source binding (e.g. on move/return). *)
let suppress_move_guard ~(lookup_guard : string -> string option) buf indent
    name =
  match lookup_guard name with
  | Some guard -> Printf.bprintf buf "\n%s%s = false" indent guard
  | None -> ()

(* ---------- cleanup emit helpers ---------- *)

(* Emit a defer-based Drop cleanup block for a let binding. *)
let emit_drop_defer buf ~indent ~guard ~binding =
  Printf.bprintf buf "\n%s%s := true" indent guard;
  Printf.bprintf buf "\n%sdefer func() {\n" indent;
  Printf.bprintf buf "%s\tif %s {\n" indent guard;
  Printf.bprintf buf "%s\t\t%s.Drop()\n" indent binding;
  Printf.bprintf buf "%s\t}\n" indent;
  Printf.bprintf buf "%s}()" indent;
  Printf.bprintf buf "\n%s_ = %s" indent guard

(* Emit a defer-based Drop cleanup block for a by-value parameter (tab-indented). *)
let emit_param_drop_defer buf ~guard ~binding =
  Printf.bprintf buf "\t%s := true\n" guard;
  Printf.bprintf buf "\tdefer func() {\n";
  Printf.bprintf buf "\t\tif %s {\n" guard;
  Printf.bprintf buf "\t\t\t%s.Drop()\n" binding;
  Printf.bprintf buf "\t\t}\n";
  Printf.bprintf buf "\t}()\n";
  Printf.bprintf buf "\t_ = %s\n" guard

(* Emit overwrite cleanup: drop the old value before reassigning. *)
let emit_overwrite_drop buf indent binding =
  Printf.bprintf buf "%s.Drop()" binding;
  Printf.bprintf buf "\n%s" indent
