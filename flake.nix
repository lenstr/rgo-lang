{
  description = "rgo development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        ocamlPkgs = pkgs.ocamlPackages;
      in {
        formatter = pkgs.nixpkgs-fmt;

        devShells.default = pkgs.mkShell {
          packages = [
            pkgs.git
            pkgs.gnumake
            pkgs.pkg-config
            pkgs.bubblewrap
            pkgs.m4
            pkgs.patch
            pkgs.go_1_26
            pkgs.opam

            ocamlPkgs.ocaml
            ocamlPkgs.findlib
            ocamlPkgs.dune_3
            ocamlPkgs.menhir
            ocamlPkgs.sedlex
            ocamlPkgs.ppx_deriving
            ocamlPkgs.alcotest
            ocamlPkgs.ppx_expect
            ocamlPkgs.ocamlformat
            ocamlPkgs.ocaml-lsp
            ocamlPkgs.bisect_ppx
          ];

          shellHook = ''
            echo "rgo nix dev shell"
            echo "  go:    $(go version | awk '{print $3}')"
            echo "  ocaml: $(ocaml -version | awk '{print $NF}')"
            echo "  dune:  $(dune --version)"
            echo "  opam:  $(opam --version)"
          '';
        };
      });
}
