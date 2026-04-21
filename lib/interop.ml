(* Interop package registry.
   Centralised metadata for supported Go packages so resolver, typecheck,
   and codegen can query a single registry instead of pattern-matching on
   hardcoded "http" strings.  The registry is structured to accept future
   package registrations without touching consumer code. *)

module SMap = Map.Make (String)

(* ---------- data model ---------- *)

(* A package-level function or constructor. *)
type pkg_fn = {
  pf_rgo_name : string; (* rgo-facing snake_case name *)
  pf_go_name : string; (* Go PascalCase name *)
  pf_params : Types.ty list;
  pf_ret : Types.ty;
}

(* A package-level type. *)
type pkg_type = {
  pt_rgo_name : string; (* rgo-facing PascalCase name, e.g. "Request" *)
  pt_go_name : string; (* Go name, e.g. "Request" *)
  pt_go_qual : string;
      (* fully qualified Go spelling including pointer, e.g. "*http.Request" *)
  pt_is_pointer : bool; (* Go representation is a pointer type *)
}

(* A receiver method on an imported type. *)
type receiver_method = {
  rm_rgo_name : string; (* rgo snake_case *)
  rm_go_name : string; (* Go PascalCase *)
  rm_params : Types.ty list;
  rm_ret : Types.ty;
}

(* A receiver field on an imported type. *)
type receiver_field = {
  rf_rgo_name : string;
  rf_go_name : string;
  rf_ty : Types.ty;
}

(* Wrong-case mapping: Go-cased name -> correct rgo-facing name. *)
type wrong_case_entry = { wc_wrong : string; wc_correct : string }

(* Per-type receiver surface. *)
type type_surface = {
  ts_methods : receiver_method list;
  ts_fields : receiver_field list;
  ts_wrong_case : wrong_case_entry list;
}

(* A registered package. *)
type package_def = {
  pd_segments : string list; (* e.g. ["net"; "http"] *)
  pd_alias : string; (* rgo import alias, e.g. "http" *)
  pd_go_import : string; (* Go import path, e.g. "net/http" *)
  pd_go_pkg : string; (* Go package name, e.g. "http" *)
  pd_fns : pkg_fn list;
  pd_types : pkg_type list;
  pd_type_surfaces : type_surface SMap.t; (* keyed by pt_rgo_name *)
  pd_wrong_case_fns : wrong_case_entry list;
  pd_wrong_case_types : wrong_case_entry list;
}

(* ---------- the net/http package definition ---------- *)

let net_http_pkg : package_def =
  let open Types in
  {
    pd_segments = [ "net"; "http" ];
    pd_alias = "http";
    pd_go_import = "net/http";
    pd_go_pkg = "http";
    pd_fns =
      [
        {
          pf_rgo_name = "listen_and_serve";
          pf_go_name = "ListenAndServe";
          pf_params = [ TString; TImported ("http", "ServeMux") ];
          pf_ret = TVoid;
        };
        {
          pf_rgo_name = "new_serve_mux";
          pf_go_name = "NewServeMux";
          pf_params = [];
          pf_ret = TImported ("http", "ServeMux");
        };
      ];
    pd_types =
      [
        {
          pt_rgo_name = "Request";
          pt_go_name = "Request";
          pt_go_qual = "*http.Request";
          pt_is_pointer = true;
        };
        {
          pt_rgo_name = "ResponseWriter";
          pt_go_name = "ResponseWriter";
          pt_go_qual = "http.ResponseWriter";
          pt_is_pointer = false;
        };
        {
          pt_rgo_name = "ServeMux";
          pt_go_name = "ServeMux";
          pt_go_qual = "*http.ServeMux";
          pt_is_pointer = true;
        };
      ];
    pd_type_surfaces =
      SMap.empty
      |> SMap.add "ServeMux"
           {
             ts_methods =
               [
                 {
                   rm_rgo_name = "handle_func";
                   rm_go_name = "HandleFunc";
                   rm_params =
                     [
                       TString;
                       TFn
                         ( [
                             TImported ("http", "ResponseWriter");
                             TImported ("http", "Request");
                           ],
                           TVoid );
                     ];
                   rm_ret = TVoid;
                 };
               ];
             ts_fields = [];
             ts_wrong_case =
               [ { wc_wrong = "HandleFunc"; wc_correct = "handle_func" } ];
           }
      |> SMap.add "Request"
           {
             ts_methods =
               [
                 {
                   rm_rgo_name = "form_value";
                   rm_go_name = "FormValue";
                   rm_params = [ TString ];
                   rm_ret = TString;
                 };
               ];
             ts_fields =
               [
                 {
                   rf_rgo_name = "method";
                   rf_go_name = "Method";
                   rf_ty = TString;
                 };
               ];
             ts_wrong_case =
               [
                 { wc_wrong = "FormValue"; wc_correct = "form_value" };
                 { wc_wrong = "Method"; wc_correct = "method" };
               ];
           }
      |> SMap.add "ResponseWriter"
           {
             ts_methods =
               [
                 {
                   rm_rgo_name = "write_header";
                   rm_go_name = "WriteHeader";
                   rm_params = [ TInt 64 ];
                   rm_ret = TVoid;
                 };
                 {
                   rm_rgo_name = "write";
                   rm_go_name = "Write";
                   rm_params = [ TString ];
                   rm_ret = TVoid;
                 };
               ];
             ts_fields = [];
             ts_wrong_case =
               [
                 { wc_wrong = "WriteHeader"; wc_correct = "write_header" };
                 { wc_wrong = "Write"; wc_correct = "write" };
               ];
           };
    pd_wrong_case_fns =
      [
        { wc_wrong = "ListenAndServe"; wc_correct = "listen_and_serve" };
        { wc_wrong = "NewServeMux"; wc_correct = "new_serve_mux" };
      ];
    pd_wrong_case_types =
      [
        { wc_wrong = "request"; wc_correct = "Request" };
        { wc_wrong = "response_writer"; wc_correct = "ResponseWriter" };
        { wc_wrong = "serve_mux"; wc_correct = "ServeMux" };
      ];
  }

(* ---------- registry ---------- *)

(* All registered packages, keyed by alias. *)
let registry : package_def SMap.t =
  SMap.empty |> SMap.add net_http_pkg.pd_alias net_http_pkg

(* All registered packages as (segments, alias) pairs for the resolver. *)
let supported_stdlib_packages : (string list * string) list =
  SMap.fold
    (fun _alias pkg acc -> (pkg.pd_segments, pkg.pd_alias) :: acc)
    registry []

(* ---------- resolver queries ---------- *)

let is_stdlib_path (segments : string list) : bool =
  SMap.exists (fun _alias pkg -> pkg.pd_segments = segments) registry

let alias_for_path (segments : string list) : string option =
  SMap.fold
    (fun _alias pkg acc ->
      match acc with
      | Some _ -> acc
      | None -> if pkg.pd_segments = segments then Some pkg.pd_alias else None)
    registry None

(* ---------- typecheck queries ---------- *)

type member_kind = MemberFn | MemberType
type member_info = { mi_rgo_name : string; mi_kind : member_kind }

let lookup_member (pkg_alias : string) (member : string) : member_info option =
  match SMap.find_opt pkg_alias registry with
  | None -> None
  | Some pkg -> (
      let fn_hit =
        List.find_opt (fun pf -> pf.pf_rgo_name = member) pkg.pd_fns
      in
      match fn_hit with
      | Some pf -> Some { mi_rgo_name = pf.pf_rgo_name; mi_kind = MemberFn }
      | None -> (
          let ty_hit =
            List.find_opt (fun pt -> pt.pt_rgo_name = member) pkg.pd_types
          in
          match ty_hit with
          | Some pt ->
              Some { mi_rgo_name = pt.pt_rgo_name; mi_kind = MemberType }
          | None -> None))

let is_known_alias (alias : string) : bool = SMap.mem alias registry

(* Return the correct rgo name if [member] is a Go-cased callable or
   snake-cased type that should be rejected. *)
let wrong_case_member (pkg_alias : string) (member : string) : string option =
  match SMap.find_opt pkg_alias registry with
  | None -> None
  | Some pkg -> (
      let fn_hit =
        List.find_opt (fun wc -> wc.wc_wrong = member) pkg.pd_wrong_case_fns
      in
      match fn_hit with
      | Some wc -> Some wc.wc_correct
      | None -> (
          let ty_hit =
            List.find_opt
              (fun wc -> wc.wc_wrong = member)
              pkg.pd_wrong_case_types
          in
          match ty_hit with Some wc -> Some wc.wc_correct | None -> None))

(* Look up the internal type for a stdlib package-qualified type name. *)
let type_ty (pkg_alias : string) (type_name : string) : Types.ty option =
  match SMap.find_opt pkg_alias registry with
  | None -> None
  | Some _pkg -> (
      let hit =
        SMap.fold
          (fun _alias pkg acc ->
            if pkg.pd_alias <> pkg_alias then acc
            else
              match acc with
              | Some _ -> acc
              | None ->
                  List.find_opt
                    (fun pt -> pt.pt_rgo_name = type_name)
                    pkg.pd_types)
          registry None
      in
      match hit with
      | Some _pt -> Some (Types.TImported (pkg_alias, type_name))
      | None -> None)

(* Look up a package-level function's type info. *)
type fn_type_info = { fti_params : Types.ty list; fti_ret : Types.ty }

let fn_type (pkg_alias : string) (fn_name : string) : fn_type_info option =
  match SMap.find_opt pkg_alias registry with
  | None -> None
  | Some pkg -> (
      let hit = List.find_opt (fun pf -> pf.pf_rgo_name = fn_name) pkg.pd_fns in
      match hit with
      | Some pf -> Some { fti_params = pf.pf_params; fti_ret = pf.pf_ret }
      | None -> None)

(* ---------- receiver queries ---------- *)

type method_info = { rmi_params : Types.ty list; rmi_ret : Types.ty }

let receiver_method (pkg_alias : string) (type_name : string)
    (method_name : string) : method_info option =
  match SMap.find_opt pkg_alias registry with
  | None -> None
  | Some pkg -> (
      match SMap.find_opt type_name pkg.pd_type_surfaces with
      | None -> None
      | Some ts -> (
          let hit =
            List.find_opt (fun rm -> rm.rm_rgo_name = method_name) ts.ts_methods
          in
          match hit with
          | Some rm -> Some { rmi_params = rm.rm_params; rmi_ret = rm.rm_ret }
          | None -> None))

type field_info = { rfi_ty : Types.ty }

let receiver_field (pkg_alias : string) (type_name : string)
    (field_name : string) : field_info option =
  match SMap.find_opt pkg_alias registry with
  | None -> None
  | Some pkg -> (
      match SMap.find_opt type_name pkg.pd_type_surfaces with
      | None -> None
      | Some ts -> (
          let hit =
            List.find_opt (fun rf -> rf.rf_rgo_name = field_name) ts.ts_fields
          in
          match hit with Some rf -> Some { rfi_ty = rf.rf_ty } | None -> None))

let wrong_case_receiver (pkg_alias : string) (type_name : string)
    (member : string) : string option =
  match SMap.find_opt pkg_alias registry with
  | None -> None
  | Some pkg -> (
      match SMap.find_opt type_name pkg.pd_type_surfaces with
      | None -> None
      | Some ts -> (
          let hit =
            List.find_opt (fun wc -> wc.wc_wrong = member) ts.ts_wrong_case
          in
          match hit with Some wc -> Some wc.wc_correct | None -> None))

(* ---------- codegen queries ---------- *)

(* Given an rgo package alias and member name, return the Go-facing name. *)
let go_member_name (pkg_alias : string) (rgo_name : string) : string option =
  match SMap.find_opt pkg_alias registry with
  | None -> None
  | Some pkg -> (
      let fn_hit =
        List.find_opt (fun pf -> pf.pf_rgo_name = rgo_name) pkg.pd_fns
      in
      match fn_hit with
      | Some pf -> Some pf.pf_go_name
      | None -> (
          let ty_hit =
            List.find_opt (fun pt -> pt.pt_rgo_name = rgo_name) pkg.pd_types
          in
          match ty_hit with Some pt -> Some pt.pt_go_name | None -> None))

(* Given an rgo package alias and receiver type + method name, return Go-facing method name. *)
let go_receiver_method_name (pkg_alias : string) (type_name : string)
    (rgo_name : string) : string option =
  match SMap.find_opt pkg_alias registry with
  | None -> None
  | Some pkg -> (
      match SMap.find_opt type_name pkg.pd_type_surfaces with
      | None -> None
      | Some ts -> (
          let hit =
            List.find_opt (fun rm -> rm.rm_rgo_name = rgo_name) ts.ts_methods
          in
          match hit with Some rm -> Some rm.rm_go_name | None -> None))

(* Given an rgo package alias and receiver type + field name, return Go-facing field name. *)
let go_receiver_field_name (pkg_alias : string) (type_name : string)
    (rgo_name : string) : string option =
  match SMap.find_opt pkg_alias registry with
  | None -> None
  | Some pkg -> (
      match SMap.find_opt type_name pkg.pd_type_surfaces with
      | None -> None
      | Some ts -> (
          let hit =
            List.find_opt (fun rf -> rf.rf_rgo_name = rgo_name) ts.ts_fields
          in
          match hit with Some rf -> Some rf.rf_go_name | None -> None))

(* Look up a package by alias. *)
let find_package (pkg_alias : string) : package_def option =
  SMap.find_opt pkg_alias registry

(* Get the Go import path for a package. *)
let go_import_path (pkg_alias : string) : string option =
  match SMap.find_opt pkg_alias registry with
  | None -> None
  | Some pkg -> Some pkg.pd_go_import

(* Get the Go package name (short) for codegen. *)
let go_pkg_name (pkg_alias : string) : string option =
  match SMap.find_opt pkg_alias registry with
  | None -> None
  | Some pkg -> Some pkg.pd_go_pkg

(* Check whether a type is a pointer in its Go representation. *)
let type_is_pointer (pkg_alias : string) (type_name : string) : bool =
  match SMap.find_opt pkg_alias registry with
  | None -> false
  | Some pkg -> (
      match
        List.find_opt (fun pt -> pt.pt_rgo_name = type_name) pkg.pd_types
      with
      | Some pt -> pt.pt_is_pointer
      | None -> false)

(* Get the fully qualified Go type string for a package type. *)
let go_qualified_type (pkg_alias : string) (type_name : string)
    (go_pkg : string) : string option =
  match SMap.find_opt pkg_alias registry with
  | None -> None
  | Some pkg -> (
      match
        List.find_opt (fun pt -> pt.pt_rgo_name = type_name) pkg.pd_types
      with
      | Some pt ->
          let prefix = if pt.pt_is_pointer then "*" else "" in
          Some (prefix ^ go_pkg ^ "." ^ pt.pt_go_name)
      | None -> None)
