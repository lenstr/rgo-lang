(* End-to-end tests for Phase 3: hello-world CLI pipeline *)

let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let n = in_channel_length ic in
      really_input_string ic n)

let write_file path contents =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc contents)

let tmp_dir = Filename.get_temp_dir_name ()

let run_cmd cmd =
  let stdout_file = Filename.temp_file "rgo_test_" ".stdout" in
  let stderr_file = Filename.temp_file "rgo_test_" ".stderr" in
  let full_cmd =
    Printf.sprintf "%s >%s 2>%s" cmd
      (Filename.quote stdout_file)
      (Filename.quote stderr_file)
  in
  let code = Sys.command full_cmd in
  let stdout = read_file stdout_file in
  let stderr = read_file stderr_file in
  Sys.remove stdout_file;
  Sys.remove stderr_file;
  (code, stdout, stderr)

let contains haystack needle =
  let nl = String.length needle in
  let hl = String.length haystack in
  if nl > hl then false
  else
    let found = ref false in
    for i = 0 to hl - nl do
      if String.sub haystack i nl = needle then found := true
    done;
    !found

let hello_source = read_file "../../examples/hello.rg"

let find_rgoc () =
  let path = "../../bin/main.exe" in
  if Sys.file_exists path then path
  else Alcotest.fail ("cannot find rgoc binary at: " ^ path)

(* ---- Tests ---- *)

let test_hello_world_compiles () =
  match Rgo.Driver.compile_string ~filename:"hello.rg" hello_source with
  | Ok go_src ->
      Alcotest.(check bool) "go source non-empty" true (String.length go_src > 0);
      Alcotest.(check bool)
        "contains package main" true
        (contains go_src "package main");
      Alcotest.(check bool)
        "contains func main" true
        (contains go_src "func main()")
  | Error _ -> Alcotest.fail "compilation should succeed"

let test_hello_world_gofmt_clean () =
  match Rgo.Driver.compile_string ~filename:"hello.rg" hello_source with
  | Error _ -> Alcotest.fail "compilation should succeed"
  | Ok go_src ->
      let out = Filename.temp_file "rgo_e2e_" ".go" in
      write_file out go_src;
      let code, diff, _ =
        run_cmd (Printf.sprintf "gofmt -d %s" (Filename.quote out))
      in
      Alcotest.(check int) "gofmt exit 0" 0 code;
      Alcotest.(check string) "gofmt no diff" "" diff;
      Sys.remove out

let test_hello_world_go_run () =
  match Rgo.Driver.compile_string ~filename:"hello.rg" hello_source with
  | Error _ -> Alcotest.fail "compilation should succeed"
  | Ok go_src ->
      let out = Filename.temp_file "rgo_e2e_" ".go" in
      write_file out go_src;
      let code, stdout, _ =
        run_cmd (Printf.sprintf "go run %s" (Filename.quote out))
      in
      Alcotest.(check int) "go run exit 0" 0 code;
      Alcotest.(check string) "prints Hello, world!" "Hello, world!\n" stdout;
      Sys.remove out

let test_no_args_fails () =
  let rgoc = find_rgoc () in
  let code, _, stderr = run_cmd (Filename.quote rgoc) in
  Alcotest.(check bool) "exit code non-zero" true (code <> 0);
  Alcotest.(check bool) "stderr has usage info" true (String.length stderr > 0)

let test_missing_input_fails () =
  let rgoc = find_rgoc () in
  let out = tmp_dir ^ "/rgo_e2e_missing.go" in
  (try Sys.remove out with Sys_error _ -> ());
  let code, _, stderr =
    run_cmd
      (Printf.sprintf "%s nonexistent.rg -o %s" (Filename.quote rgoc)
         (Filename.quote out))
  in
  Alcotest.(check bool) "exit code non-zero" true (code <> 0);
  Alcotest.(check bool) "stderr has error" true (String.length stderr > 0);
  Alcotest.(check bool) "no output file left" false (Sys.file_exists out)

let test_invalid_source_fails () =
  match
    Rgo.Driver.compile_string ~filename:"bad.rg" "this is not valid rgo @@@ !!!"
  with
  | Ok _ -> Alcotest.fail "invalid source should fail"
  | Error e ->
      let msg =
        match e with
        | Rgo.Driver.Lex_error { msg; _ } -> msg
        | Rgo.Driver.Parse_error { msg; _ } -> msg
        | Rgo.Driver.Resolve_error { msg; _ } -> msg
        | Rgo.Driver.Typecheck_error { msg; _ } -> msg
        | Rgo.Driver.Exhaust_error { msg; _ } -> msg
        | Rgo.Driver.Codegen_error msg -> msg
      in
      Alcotest.(check bool)
        "error message non-empty" true
        (String.length msg > 0)

let test_invalid_source_no_output_artifact () =
  let rgoc = find_rgoc () in
  let bad_src = Filename.temp_file "rgo_bad_" ".rg" in
  write_file bad_src "this is not valid rgo @@@ !!!";
  let out = tmp_dir ^ "/rgo_e2e_bad.go" in
  (try Sys.remove out with Sys_error _ -> ());
  let code, _, stderr =
    run_cmd
      (Printf.sprintf "%s %s -o %s" (Filename.quote rgoc)
         (Filename.quote bad_src) (Filename.quote out))
  in
  Alcotest.(check bool) "exit code non-zero" true (code <> 0);
  Alcotest.(check bool) "stderr has error" true (String.length stderr > 0);
  Alcotest.(check bool) "no output file left" false (Sys.file_exists out);
  Sys.remove bad_src

(* ---- Import diagnostic e2e tests ---- *)

let test_import_missing_no_output () =
  let src = "fn main() { let x = http::listen_and_serve; }" in
  match Rgo.Driver.compile_string ~filename:"no_import.rg" src with
  | Ok _ -> Alcotest.fail "should fail: http not imported"
  | Error e ->
      let msg =
        match e with
        | Rgo.Driver.Resolve_error { msg; _ } -> msg
        | _ -> Alcotest.fail "expected resolve error"
      in
      Alcotest.(check bool)
        "package-aware diagnostic" true
        (contains msg "not imported")

let test_import_malformed_no_output () =
  let src = "use http;\nfn main() {}" in
  match Rgo.Driver.compile_string ~filename:"malformed.rg" src with
  | Ok _ -> Alcotest.fail "should fail: malformed import"
  | Error e ->
      let msg =
        match e with
        | Rgo.Driver.Parse_error { msg; _ } -> msg
        | Rgo.Driver.Resolve_error { msg; _ } -> msg
        | _ -> Alcotest.fail "expected parse or resolve error"
      in
      Alcotest.(check bool) "error message present" true (String.length msg > 0)

let test_import_external_pkg_no_output () =
  let src = "use github::gin;\nfn main() {}" in
  match Rgo.Driver.compile_string ~filename:"external.rg" src with
  | Ok _ -> Alcotest.fail "should fail: external package"
  | Error e ->
      let msg =
        match e with
        | Rgo.Driver.Resolve_error { msg; _ } -> msg
        | _ -> Alcotest.fail "expected resolve error"
      in
      Alcotest.(check bool)
        "external package diagnostic" true
        (contains msg "unsupported external package")

let test_import_collision_no_output () =
  let src = "use net::http;\nfn http() -> i32 { 42 }\nfn main() {}" in
  match Rgo.Driver.compile_string ~filename:"collision.rg" src with
  | Ok _ -> Alcotest.fail "should fail: alias collision"
  | Error e ->
      let msg =
        match e with
        | Rgo.Driver.Resolve_error { msg; _ } -> msg
        | _ -> Alcotest.fail "expected resolve error"
      in
      Alcotest.(check bool)
        "collision diagnostic" true (contains msg "collides")

let test_import_trait_collision_no_output () =
  let src =
    "use net::http;\ntrait http { fn do_thing(&self); }\nfn main() {}"
  in
  match Rgo.Driver.compile_string ~filename:"trait_collision.rg" src with
  | Ok _ -> Alcotest.fail "should fail: trait alias collision"
  | Error e ->
      let msg =
        match e with
        | Rgo.Driver.Resolve_error { msg; _ } -> msg
        | _ -> Alcotest.fail "expected resolve error"
      in
      Alcotest.(check bool)
        "collision diagnostic" true (contains msg "collides")

let test_import_missing_type_position_no_output () =
  let src = "fn handler(req: http::Request) {}\nfn main() {}" in
  match Rgo.Driver.compile_string ~filename:"missing_type.rg" src with
  | Ok _ -> Alcotest.fail "should fail: http not imported in type position"
  | Error e ->
      let msg =
        match e with
        | Rgo.Driver.Resolve_error { msg; _ } -> msg
        | _ -> Alcotest.fail "expected resolve error"
      in
      Alcotest.(check bool)
        "package-aware diagnostic in type position" true
        (contains msg "not imported")

let test_import_unknown_member_no_output () =
  let src = "use net::http;\nfn main() { let x = http::missing_symbol; }" in
  match Rgo.Driver.compile_string ~filename:"unknown_member.rg" src with
  | Ok _ -> Alcotest.fail "should fail: unknown member"
  | Error e ->
      let msg =
        match e with
        | Rgo.Driver.Typecheck_error { msg; _ } -> msg
        | _ -> Alcotest.fail "expected typecheck error"
      in
      Alcotest.(check bool)
        "member-aware diagnostic" true
        (contains msg "undefined member" || contains msg "unknown")

(* ---- HTTP CRUD runtime regression tests ---- *)

let crud_source = read_file "../../.factory/runtime/interop-crud.rg"

let http_get url =
  let body_file = Filename.temp_file "rgo_http_" ".body" in
  let code, status, _ =
    run_cmd
      (Printf.sprintf "curl -s -o %s -w '%%{http_code}' %s"
         (Filename.quote body_file) url)
  in
  if code <> 0 then Alcotest.fail ("curl GET failed for " ^ url);
  let body = read_file body_file in
  Sys.remove body_file;
  (int_of_string (String.trim status), body)

let http_post url data =
  let body_file = Filename.temp_file "rgo_http_" ".body" in
  let code, status, _ =
    run_cmd
      (Printf.sprintf "curl -s -o %s -w '%%{http_code}' -X POST -d %s %s"
         (Filename.quote body_file) (Filename.quote data) url)
  in
  if code <> 0 then Alcotest.fail ("curl POST failed for " ^ url);
  let body = read_file body_file in
  Sys.remove body_file;
  (int_of_string (String.trim status), body)

let http_delete url =
  let code, status, _ =
    run_cmd
      (Printf.sprintf "curl -s -o /dev/null -w '%%{http_code}' -X DELETE %s" url)
  in
  if code <> 0 then Alcotest.fail ("curl DELETE failed for " ^ url);
  int_of_string (String.trim status)

let test_http_crud_runtime_regression () =
  match Rgo.Driver.compile_string ~filename:"crud.rg" crud_source with
  | Error _ -> Alcotest.fail "compilation should succeed"
  | Ok go_src ->
      let go_file = Filename.temp_file "rgo_crud_" ".go" in
      let bin = Filename.temp_file "rgo_crud_" "" in
      write_file go_file go_src;
      let build_code, _, build_stderr =
        run_cmd
          (Printf.sprintf "go build -o %s %s" (Filename.quote bin)
             (Filename.quote go_file))
      in
      if build_code <> 0 then Alcotest.fail ("go build failed:\n" ^ build_stderr);

      (* Ensure port is free before starting the server *)
      let port_check, _, _ = run_cmd "lsof -ti tcp:3111 >/dev/null 2>&1" in
      if port_check = 0 then
        Alcotest.fail "port 3111 is already in use; cannot start test server";

      (* Start server in background and capture its PID *)
      let start_cmd =
        Printf.sprintf "%s > /dev/null 2>&1 & echo $!" (Filename.quote bin)
      in
      let _, pid_str, start_stderr = run_cmd start_cmd in
      let pid = String.trim pid_str in
      if pid = "" then Alcotest.fail ("failed to start server:\n" ^ start_stderr);

      let cleanup () =
        ignore
          (run_cmd
             (Printf.sprintf "kill %s 2>/dev/null || true" (Filename.quote pid)));
        Sys.remove go_file;
        try Sys.remove bin with Sys_error _ -> ()
      in

      Fun.protect ~finally:cleanup (fun () ->
          (* Wait for server to become ready *)
          let max_wait = 20 in
          let rec wait_for_server n =
            if n > max_wait then
              Alcotest.fail "server did not start within timeout"
            else
              let ready_code, _, _ =
                run_cmd "curl -sf http://127.0.0.1:3111/items >/dev/null 2>&1"
              in
              if ready_code = 0 then ()
              else (
                ignore (Sys.command "sleep 0.5");
                wait_for_server (n + 1))
          in
          wait_for_server 0;

          (* 1. Initial GET returns empty collection *)
          let status, body = http_get "http://127.0.0.1:3111/items" in
          Alcotest.(check int) "initial GET status" 200 status;
          Alcotest.(check string) "initial GET body" "[]" body;

          (* 2. POST creates an item *)
          let post_status, post_body =
            http_post "http://127.0.0.1:3111/items" "name=alice"
          in
          Alcotest.(check int) "POST status" 201 post_status;
          Alcotest.(check string) "POST body" "created: alice" post_body;

          (* 3. GET after POST shows the created item (read-after-create) *)
          let get_status, get_body = http_get "http://127.0.0.1:3111/items" in
          Alcotest.(check int) "GET after POST status" 200 get_status;
          Alcotest.(check string) "GET after POST body" "[\"alice\"]" get_body;

          (* 4. Malformed POST returns 400 *)
          let bad_status, _ = http_post "http://127.0.0.1:3111/items" "" in
          Alcotest.(check int) "malformed POST status" 400 bad_status;

          (* 5. GET after malformed POST still shows previous state
                (malformed-create state preservation) *)
          let get2_status, get2_body = http_get "http://127.0.0.1:3111/items" in
          Alcotest.(check int) "GET after malformed POST status" 200 get2_status;
          Alcotest.(check string)
            "GET after malformed POST body" "[\"alice\"]" get2_body;

          (* 6. Unsupported method returns 405 *)
          let del_status = http_delete "http://127.0.0.1:3111/items" in
          Alcotest.(check int) "DELETE status" 405 del_status;

          (* 7. GET still works after error traffic (post-error service health) *)
          let get3_status, get3_body = http_get "http://127.0.0.1:3111/items" in
          Alcotest.(check int) "GET after DELETE status" 200 get3_status;
          Alcotest.(check string)
            "GET after DELETE body" "[\"alice\"]" get3_body;

          (* 8. Another POST works after error traffic *)
          let post2_status, post2_body =
            http_post "http://127.0.0.1:3111/items" "name=bob"
          in
          Alcotest.(check int) "second POST status" 201 post2_status;
          Alcotest.(check string) "second POST body" "created: bob" post2_body;

          (* 9. GET shows both items *)
          let _, get4_body = http_get "http://127.0.0.1:3111/items" in
          Alcotest.(check string)
            "GET with two items body" "[\"alice\", \"bob\"]" get4_body)

let () =
  Alcotest.run "e2e"
    [
      ( "hello-world",
        [
          Alcotest.test_case "compiles" `Quick test_hello_world_compiles;
          Alcotest.test_case "gofmt-clean" `Quick test_hello_world_gofmt_clean;
          Alcotest.test_case "go run prints Hello, world!" `Quick
            test_hello_world_go_run;
        ] );
      ( "cli-negative",
        [
          Alcotest.test_case "no args fails" `Quick test_no_args_fails;
          Alcotest.test_case "missing input fails" `Quick
            test_missing_input_fails;
          Alcotest.test_case "invalid source fails" `Quick
            test_invalid_source_fails;
          Alcotest.test_case "invalid source no output artifact" `Quick
            test_invalid_source_no_output_artifact;
        ] );
      ( "import-diagnostics",
        [
          Alcotest.test_case "missing import no output" `Quick
            test_import_missing_no_output;
          Alcotest.test_case "missing import type position no output" `Quick
            test_import_missing_type_position_no_output;
          Alcotest.test_case "malformed import no output" `Quick
            test_import_malformed_no_output;
          Alcotest.test_case "external package no output" `Quick
            test_import_external_pkg_no_output;
          Alcotest.test_case "alias collision no output" `Quick
            test_import_collision_no_output;
          Alcotest.test_case "trait alias collision no output" `Quick
            test_import_trait_collision_no_output;
          Alcotest.test_case "unknown stdlib member no output" `Quick
            test_import_unknown_member_no_output;
        ] );
      ( "http-crud-runtime",
        [
          Alcotest.test_case
            "read-after-create, malformed-create, post-error health" `Quick
            test_http_crud_runtime_regression;
        ] );
    ]
