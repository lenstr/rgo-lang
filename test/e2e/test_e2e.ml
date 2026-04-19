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
    ]
