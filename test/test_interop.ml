(* Unit tests for the Interop package registry. *)
open Rgo

(* ---------- helpers ---------- *)

let check_true msg b = Alcotest.(check bool) msg true b
let check_false msg b = Alcotest.(check bool) msg false b

let check_none msg = function
  | Some _ -> Alcotest.fail ("expected None for: " ^ msg)
  | None -> Alcotest.(check bool) msg true true

let check_str msg expected actual = Alcotest.(check string) msg expected actual

(* ======== resolver-level queries ======== *)

let test_stdlib_path_known () =
  check_true "net/http is stdlib" (Interop.is_stdlib_path [ "net"; "http" ])

let test_stdlib_path_unknown () =
  check_false "github.com/foo/bar is not stdlib"
    (Interop.is_stdlib_path [ "github"; "com"; "foo"; "bar" ])

let test_alias_for_net_http () =
  match Interop.alias_for_path [ "net"; "http" ] with
  | Some a -> check_str "alias" "http" a
  | None -> Alcotest.fail "expected alias for net/http"

let test_alias_for_unknown () =
  check_none "no alias for unknown" (Interop.alias_for_path [ "os"; "exec" ])

let test_is_known_alias () =
  check_true "http is known" (Interop.is_known_alias "http");
  check_false "foo is not known" (Interop.is_known_alias "foo")

(* ======== member lookups ======== *)

let test_lookup_fn_member () =
  match Interop.lookup_member "http" "listen_and_serve" with
  | Some mi ->
      Alcotest.(check bool) "is MemberFn" true (mi.mi_kind = Interop.MemberFn)
  | None -> Alcotest.fail "expected fn member"

let test_lookup_type_member () =
  match Interop.lookup_member "http" "Request" with
  | Some mi ->
      Alcotest.(check bool)
        "is MemberType" true
        (mi.mi_kind = Interop.MemberType)
  | None -> Alcotest.fail "expected type member"

let test_lookup_unknown_member () =
  check_none "no member" (Interop.lookup_member "http" "missing_symbol")

let test_lookup_unknown_package_member () =
  check_none "no package" (Interop.lookup_member "os" "Exit")

(* ======== wrong-case detection ======== *)

let test_wrong_case_fn () =
  match Interop.wrong_case_member "http" "ListenAndServe" with
  | Some correct -> check_str "correct" "listen_and_serve" correct
  | None -> Alcotest.fail "expected wrong-case hit"

let test_wrong_case_type () =
  match Interop.wrong_case_member "http" "request" with
  | Some correct -> check_str "correct" "Request" correct
  | None -> Alcotest.fail "expected wrong-case hit"

let test_wrong_case_none () =
  check_none "no wrong-case"
    (Interop.wrong_case_member "http" "listen_and_serve")

(* ======== type resolution ======== *)

let test_type_ty () =
  match Interop.type_ty "http" "Request" with
  | Some (Types.TImported ("http", "Request")) -> ()
  | _ -> Alcotest.fail "expected TImported http Request"

let test_type_ty_unknown () =
  check_none "no type" (Interop.type_ty "http" "MissingType")

(* ======== fn_type ======== *)

let test_fn_type () =
  match Interop.fn_type "http" "new_serve_mux" with
  | Some fti ->
      Alcotest.(check int) "no params" 0 (List.length fti.fti_params);
      Alcotest.(check bool)
        "returns ServeMux" true
        (fti.fti_ret = Types.TImported ("http", "ServeMux"))
  | None -> Alcotest.fail "expected fn_type"

let test_fn_type_unknown () =
  check_none "no fn" (Interop.fn_type "http" "missing_fn")

(* ======== receiver queries ======== *)

let test_receiver_method () =
  match Interop.receiver_method "http" "Request" "form_value" with
  | Some rm ->
      Alcotest.(check int) "1 param" 1 (List.length rm.rmi_params);
      Alcotest.(check bool) "returns string" true (rm.rmi_ret = Types.TString)
  | None -> Alcotest.fail "expected method"

let test_receiver_method_unknown () =
  check_none "no method"
    (Interop.receiver_method "http" "Request" "missing_method")

let test_receiver_field () =
  match Interop.receiver_field "http" "Request" "method" with
  | Some rf -> Alcotest.(check bool) "is string" true (rf.rfi_ty = Types.TString)
  | None -> Alcotest.fail "expected field"

let test_receiver_field_unknown () =
  check_none "no field"
    (Interop.receiver_field "http" "Request" "missing_field")

let test_wrong_case_receiver () =
  match Interop.wrong_case_receiver "http" "Request" "FormValue" with
  | Some correct -> check_str "correct" "form_value" correct
  | None -> Alcotest.fail "expected wrong-case receiver hit"

let test_wrong_case_receiver_none () =
  check_none "no wrong-case"
    (Interop.wrong_case_receiver "http" "Request" "form_value")

(* ======== codegen queries ======== *)

let test_go_member_name_fn () =
  match Interop.go_member_name "http" "listen_and_serve" with
  | Some name -> check_str "go name" "ListenAndServe" name
  | None -> Alcotest.fail "expected go name"

let test_go_member_name_type () =
  match Interop.go_member_name "http" "Request" with
  | Some name -> check_str "go name" "Request" name
  | None -> Alcotest.fail "expected go name"

let test_go_member_name_unknown () =
  check_none "no go name" (Interop.go_member_name "http" "missing")

let test_go_qualified_type () =
  match Interop.go_qualified_type "http" "Request" "http" with
  | Some qt -> check_str "qualified" "*http.Request" qt
  | None -> Alcotest.fail "expected qualified type"

let test_go_qualified_type_non_ptr () =
  match Interop.go_qualified_type "http" "ResponseWriter" "http" with
  | Some qt -> check_str "qualified" "http.ResponseWriter" qt
  | None -> Alcotest.fail "expected qualified type"

let test_type_is_pointer () =
  check_true "Request is pointer" (Interop.type_is_pointer "http" "Request");
  check_false "ResponseWriter is not pointer"
    (Interop.type_is_pointer "http" "ResponseWriter")

let test_go_receiver_method_name () =
  match Interop.go_receiver_method_name "http" "ServeMux" "handle_func" with
  | Some name -> check_str "go name" "HandleFunc" name
  | None -> Alcotest.fail "expected go method name"

let test_go_receiver_field_name () =
  match Interop.go_receiver_field_name "http" "Request" "method" with
  | Some name -> check_str "go name" "Method" name
  | None -> Alcotest.fail "expected go field name"

let test_go_import_path () =
  match Interop.go_import_path "http" with
  | Some p -> check_str "import path" "net/http" p
  | None -> Alcotest.fail "expected import path"

let test_go_pkg_name () =
  match Interop.go_pkg_name "http" with
  | Some n -> check_str "pkg name" "http" n
  | None -> Alcotest.fail "expected pkg name"

(* ======== future-package deferred branch ======== *)

let test_unknown_pkg_returns_none () =
  check_none "lookup_member" (Interop.lookup_member "os" "Exit");
  check_none "fn_type" (Interop.fn_type "os" "Exit");
  check_none "type_ty" (Interop.type_ty "os" "File");
  check_none "receiver_method" (Interop.receiver_method "os" "File" "Close");
  check_none "receiver_field" (Interop.receiver_field "os" "File" "Name");
  check_none "go_member_name" (Interop.go_member_name "os" "Exit");
  check_none "go_import_path" (Interop.go_import_path "os");
  check_none "go_pkg_name" (Interop.go_pkg_name "os");
  check_false "is_stdlib_path" (Interop.is_stdlib_path [ "os" ]);
  check_false "is_known_alias" (Interop.is_known_alias "os")

(* ======== test suite ======== *)

let () =
  Alcotest.run "interop"
    [
      ( "resolver-queries",
        [
          ("known stdlib path", `Quick, test_stdlib_path_known);
          ("unknown path", `Quick, test_stdlib_path_unknown);
          ("alias for net/http", `Quick, test_alias_for_net_http);
          ("alias for unknown", `Quick, test_alias_for_unknown);
          ("is_known_alias", `Quick, test_is_known_alias);
        ] );
      ( "member-lookups",
        [
          ("fn member", `Quick, test_lookup_fn_member);
          ("type member", `Quick, test_lookup_type_member);
          ("unknown member", `Quick, test_lookup_unknown_member);
          ("unknown package member", `Quick, test_lookup_unknown_package_member);
        ] );
      ( "wrong-case",
        [
          ("wrong-case fn", `Quick, test_wrong_case_fn);
          ("wrong-case type", `Quick, test_wrong_case_type);
          ("no wrong-case for correct name", `Quick, test_wrong_case_none);
        ] );
      ( "type-resolution",
        [
          ("type_ty Request", `Quick, test_type_ty);
          ("type_ty unknown", `Quick, test_type_ty_unknown);
        ] );
      ( "fn-type",
        [
          ("fn_type new_serve_mux", `Quick, test_fn_type);
          ("fn_type unknown", `Quick, test_fn_type_unknown);
        ] );
      ( "receiver",
        [
          ("method form_value", `Quick, test_receiver_method);
          ("method unknown", `Quick, test_receiver_method_unknown);
          ("field method", `Quick, test_receiver_field);
          ("field unknown", `Quick, test_receiver_field_unknown);
          ("wrong-case receiver", `Quick, test_wrong_case_receiver);
          ( "no wrong-case for correct receiver",
            `Quick,
            test_wrong_case_receiver_none );
        ] );
      ( "codegen-queries",
        [
          ("go_member_name fn", `Quick, test_go_member_name_fn);
          ("go_member_name type", `Quick, test_go_member_name_type);
          ("go_member_name unknown", `Quick, test_go_member_name_unknown);
          ("go_qualified_type pointer", `Quick, test_go_qualified_type);
          ( "go_qualified_type non-pointer",
            `Quick,
            test_go_qualified_type_non_ptr );
          ("type_is_pointer", `Quick, test_type_is_pointer);
          ("go_receiver_method_name", `Quick, test_go_receiver_method_name);
          ("go_receiver_field_name", `Quick, test_go_receiver_field_name);
          ("go_import_path", `Quick, test_go_import_path);
          ("go_pkg_name", `Quick, test_go_pkg_name);
        ] );
      ( "future-package-hooks",
        [
          ( "unknown package returns None everywhere",
            `Quick,
            test_unknown_pkg_returns_none );
        ] );
    ]
