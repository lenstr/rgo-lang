let parse_go_version_line (ver_line : string) : (string, string) result =
  let parts = String.split_on_char ' ' ver_line in
  let go_ver =
    List.find_opt
      (fun s -> String.length s > 2 && String.sub s 0 2 = "go")
      parts
  in
  match go_ver with
  | Some v -> (
      let num = String.sub v 2 (String.length v - 2) in
      let major_minor = String.split_on_char '.' num in
      match major_minor with
      | major :: minor :: _ -> (
          try
            let maj = int_of_string major in
            let min = int_of_string minor in
            if maj > 1 || (maj = 1 && min >= 26) then Ok v
            else
              Error
                (Printf.sprintf
                   "Go %s detected; trait Self support requires Go 1.26+" v)
          with _ -> Ok v)
      | _ -> Ok v)
  | None -> Error ("cannot parse Go version from: " ^ ver_line)

let check_go_version_adequate () : (string, string) result =
  let stdout_file = Filename.temp_file "rgo_gover_" ".stdout" in
  let cmd =
    Printf.sprintf "go version > %s 2>&1" (Filename.quote stdout_file)
  in
  let code = Sys.command cmd in
  if code <> 0 then begin
    (try Sys.remove stdout_file with Sys_error _ -> ());
    Error "go: command not found or failed"
  end
  else begin
    let ic = open_in stdout_file in
    let ver_line =
      Fun.protect
        ~finally:(fun () -> close_in ic)
        (fun () -> try input_line ic with End_of_file -> "")
    in
    Sys.remove stdout_file;
    parse_go_version_line ver_line
  end

type compile_error =
  | Lex_error of { msg : string; line : int; col : int }
  | Parse_error of { msg : string; line : int; col : int }
  | Resolve_error of { msg : string; line : int; col : int }
  | Typecheck_error of { msg : string; line : int; col : int }
  | Exhaust_error of { msg : string; line : int; col : int }
  | Codegen_error of string

let compile_string ?(filename = "<input>") (source : string) :
    (string, compile_error) result =
  try
    let ast = Parse_driver.parse_string ~filename source in
    let ast = Resolver.resolve_exn ast in
    let ast = Typecheck.typecheck_exn ast in
    let ast = Exhaust.check_exn ast in
    let go_src = Codegen.generate ast in
    Ok go_src
  with
  | Lexer.Lexer_error { msg; pos } ->
      Error (Lex_error { msg; line = pos.line; col = pos.col })
  | Parse_driver.Parse_error { msg; line; col } ->
      Error (Parse_error { msg; line; col })
  | Resolver.Resolve_error { msg; line; col } ->
      Error (Resolve_error { msg; line; col })
  | Typecheck.Typecheck_error { msg; line; col } ->
      Error (Typecheck_error { msg; line; col })
  | Exhaust.Exhaust_error { msg; line; col } ->
      Error (Exhaust_error { msg; line; col })
  | Failure msg -> Error (Codegen_error msg)
