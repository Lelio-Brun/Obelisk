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
        lib = pkgs.lib;
        ocamlPackages = pkgs.ocamlPackages;

        captureFromFirstMatchingLine =
          regex: file:
          let
            matches = builtins.filter (match: match != null) (
              map (builtins.match regex) (lib.splitString "\n" (builtins.readFile file))
            );
          in
          if matches == [ ] then
            throw "no line of ${toString file} matches ${regex}"
          else
            builtins.head (builtins.head matches);

        duneProjectVersion = captureFromFirstMatchingLine "\\(version ([^)]+)\\)" ./dune-project;

        ocamlformatVersion = captureFromFirstMatchingLine "version *= *([^ ]+) *" ./.ocamlformat;
        ocamlformat = pkgs."ocamlformat_${lib.replaceStrings [ "." ] [ "_" ] ocamlformatVersion}";

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
            package = ocamlformat;
          };
          settings.formatter.dune = {
            command = lib.getExe formatDuneFilesInPlace;
            includes = [
              "dune"
              "*/dune"
            ];
          };
        };
      in
      {
        formatter = treefmtEval.config.build.wrapper;
        checks = {
          formatting = treefmtEval.config.build.check self;
          package = self.packages.${system}.default;
        };

        packages.default = ocamlPackages.buildDunePackage {
          pname = "obelisk";
          version = duneProjectVersion;
          src = lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions [
              ./dune-project
              ./obelisk.opam
              ./misc/reco.mly
              ./src
              ./tests
            ];
          };

          strictDeps = true;

          nativeBuildInputs = [ ocamlPackages.menhir ];
          buildInputs = [ ocamlPackages.re ];

          doCheck = true;

          meta.mainProgram = "obelisk";
        };

        devShells.default = pkgs.mkShell {
          name = "obelisk-shell";
          inputsFrom = [ self.packages.${system}.default ];
          packages = [
            ocamlPackages.ocaml-lsp
            ocamlPackages.merlin
            ocamlformat
            ocamlPackages.ocp-indent
            ocamlPackages.utop
            latexTestDeps
            pkgs.html-tidy
          ];
        };
      }
    );
}
