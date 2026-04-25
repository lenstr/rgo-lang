let usage () =
  Printf.eprintf "Usage: rgoc <input.rg> -o <output.go> [--fix-format]\n";
  Printf.eprintf "       rgoc --version\n";
  exit 1

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

let error fmt =
  Printf.ksprintf
    (fun msg ->
      Printf.eprintf "error: %s\n" msg;
      exit 1)
    fmt

let run_cmd cmd =
  let stdout_file = Filename.temp_file "rgoc_" ".stdout" in
  let stderr_file = Filename.temp_file "rgoc_" ".stderr" in
  let cleanup () =
    (try Sys.remove stdout_file with Sys_error _ -> ());
    try Sys.remove stderr_file with Sys_error _ -> ()
  in
  Fun.protect ~finally:cleanup (fun () ->
      let full_cmd =
        Printf.sprintf "%s >%s 2>%s" cmd
          (Filename.quote stdout_file)
          (Filename.quote stderr_file)
      in
      let code = Sys.command full_cmd in
      let stdout = read_file stdout_file in
      let stderr = read_file stderr_file in
      (code, stdout, stderr))

type gofmt_mode = Check | Fix

let run_gofmt mode path =
  let flag = match mode with Check -> "-d" | Fix -> "-w" in
  let code, stdout, stderr =
    run_cmd (Printf.sprintf "gofmt %s %s" flag (Filename.quote path))
  in
  if code <> 0 then
    error "gofmt failed on %s (exit %d)\n\nstderr:\n%s\nstdout:\n%s" path code
      stderr stdout;
  if mode = Check && stdout <> "" then
    error
      "generated Go is not gofmt-clean; this is a compiler codegen bug\n\n\
       Run with --fix-format if you only need a formatted output file.\n\n\
       gofmt diff:\n\
       %s"
      stdout

let compile ~fix_format input_path output_path =
  if not (Sys.file_exists input_path) then
    error "input file not found: %s" input_path;
  let source = read_file input_path in
  match Rgo.Driver.compile_string ~filename:input_path source with
  | Ok go_src ->
      write_file output_path go_src;
      run_gofmt (if fix_format then Fix else Check) output_path
  | Error (Lex_error { msg; line; col }) ->
      (* Remove output file if it was created *)
      if Sys.file_exists output_path then Sys.remove output_path;
      error "%s:%d:%d: %s" input_path line col msg
  | Error (Parse_error { msg; line; col }) ->
      if Sys.file_exists output_path then Sys.remove output_path;
      error "%s:%d:%d: %s" input_path line col msg
  | Error (Resolve_error { msg; line; col }) ->
      if Sys.file_exists output_path then Sys.remove output_path;
      error "%s:%d:%d: %s" input_path line col msg
  | Error (Typecheck_error { msg; line; col }) ->
      if Sys.file_exists output_path then Sys.remove output_path;
      error "%s:%d:%d: %s" input_path line col msg
  | Error (Exhaust_error { msg; line; col }) ->
      if Sys.file_exists output_path then Sys.remove output_path;
      error "%s:%d:%d: %s" input_path line col msg
  | Error (Codegen_error msg) ->
      if Sys.file_exists output_path then Sys.remove output_path;
      error "%s" msg

let () =
  let argv = Sys.argv in
  let argc = Array.length argv in
  if argc = 2 && argv.(1) = "--version" then print_endline Rgo.Version.version
  else if argc = 4 && argv.(2) = "-o" then
    compile ~fix_format:false argv.(1) argv.(3)
  else if argc = 5 && argv.(2) = "-o" && argv.(4) = "--fix-format" then
    compile ~fix_format:true argv.(1) argv.(3)
  else usage ()
