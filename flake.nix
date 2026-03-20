# https://github.com/brendanzab/ocaml-flake-example/blob/main/flake.nix
{
  description = "Ocaml flake";

  # Flake dependency specification
  #
  # To update all flake inputs:
  #
  #     $ nix flake update --commit-lock-file
  #
  # To update individual flake inputs:
  #
  #     $ nix flake lock --update-input <input> ... --commit-lock-file
  #
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    systems.url = "github:nix-systems/default";
  };

  outputs = {
    self,
    nixpkgs,
    systems,
  }: let
    lib = nixpkgs.lib;
    eachSystem = lib.genAttrs (import systems);
  in {
    # Exposed packages that can be built or run with `nix build` or
    # `nix run` respectively:
    #
    #     $ nix build .#<name>
    #     $ nix run .#<name> -- <args?>
    #
    packages = eachSystem (
      system: let
        legacyPackages = nixpkgs.legacyPackages.${system};
        ocamlPackages = legacyPackages.ocamlPackages;
      in {
        default = self.packages.${system}.acl;

        acl = ocamlPackages.buildDunePackage {
          pname = "acl";
          version = "0.0.1";
          duneVersion = "3";
          src = ./.;

          buildInputs = [
            # OCaml package dependencies go here.
          ];

          strictDeps = true;
        };
      }
    );

    # Flake checks
    #
    #     $ nix flake check
    #
    checks = eachSystem (system: let
      legacyPackages = nixpkgs.legacyPackages.${system};
      ocamlPackages = legacyPackages.ocamlPackages;
    in {
      # Run tests for the `acl` package
      acl = let
        # Patches calls to dune commands to produce log-friendly output
        # when using `nix ... --print-build-log`. Ideally there would be
        # support for one or more of the following:
        #
        # In Dune:
        #
        # - have workspace-specific dune configuration files
        #
        # In NixPkgs:
        #
        # - allow dune flags to be set in in `ocamlPackages.buildDunePackage`
        # - alter `ocamlPackages.buildDunePackage` to use `--display=short`
        # - alter `ocamlPackages.buildDunePackage` to allow `--config-file=FILE` to be set
        patchDuneCommand = let
          subcmds = ["build" "test" "runtest" "install"];
        in
          lib.replaceStrings
          (lib.lists.map (subcmd: "dune ${subcmd}") subcmds)
          (lib.lists.map (subcmd: "dune ${subcmd} --display=short") subcmds);
      in
        self.packages.${system}.acl.overrideAttrs
        (oldAttrs: {
          name = "check-${oldAttrs.name}";
          doCheck = true;
          buildPhase = patchDuneCommand oldAttrs.buildPhase;
          checkPhase = patchDuneCommand oldAttrs.checkPhase;
          # installPhase = patchDuneCommand oldAttrs.checkPhase;
        });

      # Check Dune and OCaml formatting
      dune-fmt =
        legacyPackages.runCommand "check-dune-fmt"
        {
          nativeBuildInputs = [
            ocamlPackages.dune_3
            ocamlPackages.ocaml
            legacyPackages.ocamlformat
          ];
        }
        ''
          echo "checking dune and ocaml formatting"
          dune build \
            --display=short \
            --no-print-directory \
            --root="${./.}" \
            --build-dir="$(pwd)/_build" \
            @fmt
          touch $out
        '';

      # Check documentation generation
      dune-doc =
        legacyPackages.runCommand "check-dune-doc"
        {
          ODOC_WARN_ERROR = "true";
          nativeBuildInputs = [
            ocamlPackages.dune_3
            ocamlPackages.ocaml
            ocamlPackages.odoc
          ];
        }
        ''
          echo "checking ocaml documentation"
          dune build \
            --display=short \
            --no-print-directory \
            --root="${./.}" \
            --build-dir="$(pwd)/_build" \
            @doc
          touch $out
        '';

      # Check Nix formatting
      nixpkgs-fmt =
        legacyPackages.runCommand "check-nixpkgs-fmt"
        {nativeBuildInputs = [legacyPackages.nixpkgs-fmt];}
        ''
          echo "checking nix formatting"
          nixpkgs-fmt --check ${./.}
          touch $out
        '';
    });

    devShells = eachSystem (system: let
      legacyPackages = nixpkgs.legacyPackages.${system};
      ocamlPackages = legacyPackages.ocamlPackages;
    in {
      default = legacyPackages.mkShell {
        # Development tools
        packages = [
          # Source file formatting
          legacyPackages.nixpkgs-fmt
          legacyPackages.ocamlformat
          # For `dune build --watch ...`
          legacyPackages.fswatch
          # For `dune build @doc`
          ocamlPackages.odoc
          # OCaml editor support
          ocamlPackages.ocaml-lsp
          # Fancy REPL thing
          ocamlPackages.utop
          # LR(1) parser generator
          ocamlPackages.menhir
          # Pretty printing
          ocamlPackages.printbox
          ocamlPackages.printbox-text
          # Regular expressions
          ocamlPackages.re
        ];

        # Tools from packages
        inputsFrom = [
          self.packages.${system}.acl
        ];
      };
    });

    # devShells.x86_64-linux.default = pkgs.mkShell {
    #   buildInputs = with pkgs; [
    #     ocaml
    #     dune_3
    #     ocamlformat
    #     ocamlPackages.ocaml-lsp
    #     ocamlPackages.findlib
    #     ocamlPackages.merlin
    #     ocamlPackages.utop
    #     ocamlPackages.menhir
    #     ocamlPackages.printbox
    #     ocamlPackages.printbox-text
    #     ocamlPackages.re
    #   ];
    # };
  };
}
