(* Shared test helpers for codegen test suites. *)

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

let run_cmd cmd =
  let stdout_file = Filename.temp_file "rgo_cg_" ".stdout" in
  let stderr_file = Filename.temp_file "rgo_cg_" ".stderr" in
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

let compile_and_check ?expected_output source =
  match Rgo.Driver.compile_string ~filename:"test.rg" source with
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
      Alcotest.fail ("compilation failed: " ^ msg)
  | Ok go_src ->
      let out = Filename.temp_file "rgo_cg_" ".go" in
      write_file out go_src;
      let code, diff, _ =
        run_cmd (Printf.sprintf "gofmt -d %s" (Filename.quote out))
      in
      Alcotest.(check int) "gofmt exit 0" 0 code;
      if diff <> "" then
        Alcotest.fail ("gofmt diff:\n" ^ diff ^ "\nGenerated Go:\n" ^ go_src);
      let code, _, stderr =
        run_cmd (Printf.sprintf "go build -o /dev/null %s" (Filename.quote out))
      in
      if code <> 0 then
        Alcotest.fail
          ("go build failed:\n" ^ stderr ^ "\nGenerated Go:\n" ^ go_src);
      let code, _, stderr =
        run_cmd (Printf.sprintf "go vet %s" (Filename.quote out))
      in
      if code <> 0 then
        Alcotest.fail
          ("go vet failed:\n" ^ stderr ^ "\nGenerated Go:\n" ^ go_src);
      (match expected_output with
      | Some expected ->
          let code, stdout, stderr =
            run_cmd (Printf.sprintf "go run %s" (Filename.quote out))
          in
          if code <> 0 then
            Alcotest.fail
              ("go run failed:\n" ^ stderr ^ "\nGenerated Go:\n" ^ go_src);
          Alcotest.(check string) "runtime output" expected stdout
      | None -> ());
      Sys.remove out;
      go_src

let compile_snapshot source =
  match Rgo.Driver.compile_string ~filename:"test.rg" source with
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
      Alcotest.fail ("compilation failed: " ^ msg)
  | Ok go_src -> go_src

let compile_expect_error ~expect source () =
  match Rgo.Driver.compile_string ~filename:"test.rg" source with
  | Ok go_src ->
      Alcotest.fail
        ("expected compilation error, but got success.\nGenerated Go:\n"
       ^ go_src)
  | Error _ ->
      let msg =
        match Rgo.Driver.compile_string ~filename:"test.rg" source with
        | Error (Rgo.Driver.Resolve_error { msg; _ }) -> msg
        | Error (Rgo.Driver.Typecheck_error { msg; _ }) -> msg
        | Error (Rgo.Driver.Parse_error { msg; _ }) -> msg
        | Error (Rgo.Driver.Lex_error { msg; _ }) -> msg
        | Error (Rgo.Driver.Exhaust_error { msg; _ }) -> msg
        | Error (Rgo.Driver.Codegen_error msg) -> msg
        | Ok _ -> "unreachable"
      in
      if not (contains msg expect) then
        Alcotest.failf "error %S does not contain expected %S" msg expect
