let usage () =
  Printf.eprintf "Usage: rgoc <input.rg> -o <output.go>\n";
  Printf.eprintf "       rgoc --version\n";
  exit 1

let error fmt =
  Printf.ksprintf
    (fun msg ->
      Printf.eprintf "error: %s\n" msg;
      exit 1)
    fmt

let run_gofmt path =
  let cmd = Printf.sprintf "gofmt -w %s" (Filename.quote path) in
  let code = Sys.command cmd in
  if code <> 0 then error "gofmt failed on %s (exit %d)" path code

let compile input_path output_path =
  if not (Sys.file_exists input_path) then
    error "input file not found: %s" input_path;
  let source =
    let ic = open_in input_path in
    Fun.protect
      ~finally:(fun () -> close_in ic)
      (fun () ->
        let n = in_channel_length ic in
        really_input_string ic n)
  in
  match Rgo.Driver.compile_string ~filename:input_path source with
  | Ok go_src ->
      let oc = open_out output_path in
      Fun.protect
        ~finally:(fun () -> close_out oc)
        (fun () -> output_string oc go_src);
      run_gofmt output_path
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
  | Error (Codegen_error msg) ->
      if Sys.file_exists output_path then Sys.remove output_path;
      error "%s" msg

let () =
  let argv = Sys.argv in
  let argc = Array.length argv in
  if argc = 2 && argv.(1) = "--version" then print_endline Rgo.Version.version
  else if argc = 4 && argv.(2) = "-o" then compile argv.(1) argv.(3)
  else usage ()
