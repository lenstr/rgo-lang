let test_version () =
  Alcotest.(check string) "version string" "rgoc v0.0.1" Rgo.Version.version

let () =
  Alcotest.run "rgo"
    [ ("version", [ Alcotest.test_case "version string" `Quick test_version ]) ]
