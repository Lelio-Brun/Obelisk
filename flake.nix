{
  description = "Obelisk: pretty-printing for Menhir files";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      treefmt-nix,
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

        formatDuneFilesInPlace = pkgs.writeShellApplication {
          name = "format-dune-files-in-place";
          runtimeInputs = [
            ocamlPackages.dune_3
            pkgs.diffutils
          ];
          text = ''
            for file in "$@"; do
              formatted=$(mktemp)
              dune format-dune-file "$file" > "$formatted"
              if ! cmp -s "$formatted" "$file"; then
                cat "$formatted" > "$file"
              fi
              rm "$formatted"
            done
          '';
        };

        treefmtEval = treefmt-nix.lib.evalModule pkgs {
          programs.nixfmt.enable = true;
          programs.ocamlformat = {
            enable = true;
            configFile = ./.ocamlformat;
          };
          settings.formatter.dune = {
            command = pkgs.lib.getExe formatDuneFilesInPlace;
            includes = [
              "dune"
              "*/dune"
            ];
          };
        };
      in
      {
        formatter = treefmtEval.config.build.wrapper;
        checks.formatting = treefmtEval.config.build.check self;

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
