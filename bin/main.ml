let () =
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "--version" then
    print_endline Rgo.Version.version
  else print_endline "Usage: rgoc [--version]"
