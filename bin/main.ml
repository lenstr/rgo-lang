let usage () =
  Printf.eprintf "Usage: rgoc <input.rg> -o <output.go>\n";
  Printf.eprintf "       rgoc --version\n";
  Printf.eprintf "       rgoc <input.rg> --emit-ast\n";
  Printf.eprintf "\nOptions:\n";
  Printf.eprintf "  -o <file>       Write Go output to <file>\n";
  Printf.eprintf "  --no-gofmt      Skip gofmt formatting of output\n";
  Printf.eprintf
    "  --emit-ast      Print parsed AST to stdout instead of compiling\n";
  Printf.eprintf "  --version       Print version and exit\n";
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

let compile input_path output_path run_gofmt_flag =
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
      if run_gofmt_flag then run_gofmt output_path
  | Error (Lex_error { msg; line; col }) ->
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

let emit_ast input_path =
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
  match Rgo.Driver.emit_ast_string ~filename:input_path source with
  | Ok ast_str -> print_endline ast_str
  | Error (Lex_error { msg; line; col }) ->
      error "%s:%d:%d: %s" input_path line col msg
  | Error (Parse_error { msg; line; col }) ->
      error "%s:%d:%d: %s" input_path line col msg
  | Error _ -> error "unexpected error during AST emission"

let () =
  let argv = Sys.argv in
  let argc = Array.length argv in
  (* Parse flags and positional arguments *)
  let rec loop i input output gofmt emit_ast_flag =
    if i >= argc then (input, output, gofmt, emit_ast_flag)
    else
      let arg = argv.(i) in
      if arg = "--version" then (
        print_endline Rgo.Version.version;
        exit 0)
      else if arg = "--no-gofmt" then
        loop (i + 1) input output false emit_ast_flag
      else if arg = "--emit-ast" then loop (i + 1) input output gofmt true
      else if arg = "-o" then
        if i + 1 < argc then
          loop (i + 2) input (Some argv.(i + 1)) gofmt emit_ast_flag
        else error "-o requires an argument"
      else if String.length arg > 0 && arg.[0] = '-' then
        error "unknown flag: %s" arg
      else
        match input with
        | None -> loop (i + 1) (Some arg) output gofmt emit_ast_flag
        | Some _ -> error "multiple input files specified"
  in
  let input, output, gofmt, emit_ast_flag = loop 1 None None true false in
  match (input, output, emit_ast_flag) with
  | None, _, _ -> usage ()
  | Some inp, _, true -> emit_ast inp
  | Some inp, Some out, false -> compile inp out gofmt
  | Some _, None, false -> error "no output file specified; use -o <file>"
