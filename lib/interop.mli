(** Interop package registry. Centralised metadata for supported Go packages.
    All consumer modules (resolver, typecheck, codegen) query this registry
    instead of pattern-matching on hardcoded package strings. *)

(** {1 Data model} *)

type pkg_fn = {
  pf_rgo_name : string;
  pf_go_name : string;
  pf_params : Types.ty list;
  pf_ret : Types.ty;
}

type pkg_type = {
  pt_rgo_name : string;
  pt_go_name : string;
  pt_go_qual : string;
  pt_is_pointer : bool;
}

type receiver_method = {
  rm_rgo_name : string;
  rm_go_name : string;
  rm_params : Types.ty list;
  rm_ret : Types.ty;
}

type receiver_field = {
  rf_rgo_name : string;
  rf_go_name : string;
  rf_ty : Types.ty;
}

type wrong_case_entry = { wc_wrong : string; wc_correct : string }

type type_surface = {
  ts_methods : receiver_method list;
  ts_fields : receiver_field list;
  ts_wrong_case : wrong_case_entry list;
}

type package_def = {
  pd_segments : string list;
  pd_alias : string;
  pd_go_import : string;
  pd_go_pkg : string;
  pd_fns : pkg_fn list;
  pd_types : pkg_type list;
  pd_type_surfaces : type_surface Map.Make(String).t;
  pd_wrong_case_fns : wrong_case_entry list;
  pd_wrong_case_types : wrong_case_entry list;
}

(** {1 Resolver queries} *)

val supported_stdlib_packages : (string list * string) list
val is_stdlib_path : string list -> bool
val alias_for_path : string list -> string option
val is_known_alias : string -> bool

(** {1 Typecheck queries} *)

type member_kind = MemberFn | MemberType
type member_info = { mi_rgo_name : string; mi_kind : member_kind }

val lookup_member : string -> string -> member_info option
val wrong_case_member : string -> string -> string option
val type_ty : string -> string -> Types.ty option

type fn_type_info = { fti_params : Types.ty list; fti_ret : Types.ty }

val fn_type : string -> string -> fn_type_info option

(** {1 Receiver queries} *)

type method_info = { rmi_params : Types.ty list; rmi_ret : Types.ty }

val receiver_method : string -> string -> string -> method_info option

type field_info = { rfi_ty : Types.ty }

val receiver_field : string -> string -> string -> field_info option
val wrong_case_receiver : string -> string -> string -> string option

(** {1 Codegen queries} *)

val find_package : string -> package_def option
val go_import_path : string -> string option
val go_pkg_name : string -> string option
val go_member_name : string -> string -> string option
val go_receiver_method_name : string -> string -> string -> string option
val go_receiver_field_name : string -> string -> string -> string option
val type_is_pointer : string -> string -> bool
val go_qualified_type : string -> string -> string -> string option
