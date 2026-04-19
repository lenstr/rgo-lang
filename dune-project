(lang dune 3.15)

(using menhir 3.0)

(name rgo)

(generate_opam_files true)

(package
 (name rgo)
 (synopsis "rgo compiler: Rust-like syntax targeting Go")
 (depends
  (ocaml (>= 5.1))
  (dune (>= 3.15))
  (menhir (>= 20230608))
  (sedlex (>= 3.2))
  (ppx_deriving (>= 5.2))
  (alcotest :with-test)
  (ppx_expect :with-test)))
