{
  description = "Obelisk: pretty-printing for Menhir files";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ocamlPackages = pkgs.ocamlPackages;

        duneProjectVersion = builtins.head (
          builtins.head (
            builtins.filter (match: match != null) (
              map (builtins.match "\\(version ([^)]+)\\)") (
                pkgs.lib.splitString "\n" (builtins.readFile ./dune-project)
              )
            )
          )
        );

        latexTestDeps = pkgs.texliveSmall.withPackages (
          ps: with ps; [
            standalone
            preview
            l3packages
            tools
            tabu
            collcell
            simplebnf
            tabularray
            mdwtools
            backnaur
          ]
        );
      in
      {
        packages.default = ocamlPackages.buildDunePackage {
          pname = "obelisk";
          version = duneProjectVersion;
          src = ./.;

          strictDeps = true;

          nativeBuildInputs = [ ocamlPackages.menhir ];
          buildInputs = [ ocamlPackages.re ];
        };

        devShells.default = pkgs.mkShell {
          name = "obelisk-shell";
          inputsFrom = [ self.packages.${system}.default ];
          packages = [
            ocamlPackages.ocaml-lsp
            ocamlPackages.merlin
            ocamlPackages.ocamlformat
            ocamlPackages.ocp-indent
            ocamlPackages.utop
            latexTestDeps
            pkgs.html-tidy
          ];
        };
      }
    );
}
