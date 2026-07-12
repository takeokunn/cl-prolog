{
  description = "Dependency-free Common Lisp Prolog engine";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  # cl-weave is the testing library used by the cl-prolog/tests ASDF system.
  # suite.  It follows this flake's nixpkgs so both share a single SBCL.
  inputs.cl-weave.url = "github:takeokunn/cl-weave/v0.3.0";
  inputs.cl-weave.inputs.nixpkgs.follows = "nixpkgs";
  inputs.cl-weave.inputs.paredit-cli.follows = "paredit-cli";

  # paredit-cli provides structural S-expression tooling for this repo's
  # Lisp sources: a dev-shell binary for agent-driven refactors and a
  # structural-parse lint gate reused in `checks`.
  inputs.paredit-cli.url = "github:takeokunn/paredit-cli";
  inputs.paredit-cli.inputs.nixpkgs.follows = "nixpkgs";

  outputs =
    {
      self,
      nixpkgs,
      cl-weave,
      paredit-cli,
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems =
        f:
        builtins.listToAttrs (
          map (system: {
            name = system;
            value = f system;
          }) systems
        );
      sourceFor =
        pkgs:
        pkgs.lib.cleanSourceWith {
          src = ./.;
          filter =
            path: type:
            (
              (pkgs.lib.cleanSourceFilter path type)
              # Keep test sources available to Nix checks before they are staged.
              || (
                let
                  tests-directory = "${toString ./.}/tests";
                  path-string = toString path;
                in
                path-string == tests-directory || pkgs.lib.hasPrefix "${tests-directory}/" path-string
              )
            )
            && (
              let
                name = builtins.baseNameOf path;
              in
              !(
                pkgs.lib.hasSuffix ".fasl" name
                || pkgs.lib.hasSuffix ".cfasl" name
                || pkgs.lib.hasSuffix ".dfsl" name
                || pkgs.lib.hasSuffix ".ufasl" name
                || pkgs.lib.hasSuffix ".core" name
                || pkgs.lib.hasSuffix ".o" name
              )
            );
        };
    in
    {
      formatter = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        pkgs.nixpkgs-fmt
      );

      packages = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          src = sourceFor pkgs;
        in
        {
          default = pkgs.sbcl.buildASDFSystem {
            pname = "cl-prolog";
            version = "0.3.0";
            src = src;
            systems = [ "cl-prolog" ];
          };
        }
      );

      checks = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          src = sourceFor pkgs;
        in
        {
          # The complete suite is an ASDF system.  cl-weave is exposed through
          # CL_SOURCE_REGISTRY, so no project-local test runner is required.
          default =
            pkgs.runCommand "cl-prolog-weave-tests"
              {
                nativeBuildInputs = [ pkgs.sbcl ];
              }
              ''
                cp -R ${src} source
                chmod -R u+w source
                cd source
                export HOME="$TMPDIR/home"
                export XDG_CACHE_HOME="$TMPDIR/cache"
                mkdir -p "$HOME" "$XDG_CACHE_HOME"
                export CL_SOURCE_REGISTRY="${
                  cl-weave.packages.${system}.default
                }/share/common-lisp/source//:$PWD//:"
                sbcl --non-interactive \
                  --eval '(require :asdf)' \
                  --eval '(asdf:load-asd (truename "cl-prolog.asd"))' \
                  --eval '(asdf:test-system :cl-prolog/tests)'
                touch $out
              '';

          # Structural parse gate over every tracked Lisp source: fails if
          # any .lisp/.asd file is not a balanced S-expression document.
          paredit-lint = paredit-cli.lib.${system}.mkLintCheck {
            src = src;
            name = "cl-prolog-paredit-lint";
          };
        }
      );

      apps = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          clWeavePackage = cl-weave.packages.${system}.default;
          test = pkgs.writeShellApplication {
            name = "cl-prolog-test";
            runtimeInputs = [ clWeavePackage ];
            text = ''
              export CL_SOURCE_REGISTRY="${clWeavePackage}/share/common-lisp/source//:$PWD//:''${CL_SOURCE_REGISTRY:-}"
              exec cl-weave run cl-prolog/tests "$@"
            '';
          };
        in
        {
          test = {
            type = "app";
            program = "${test}/bin/cl-prolog-test";
            meta.description = "Run the cl-prolog cl-weave ASDF test suite";
          };
          default = self.apps.${system}.test;
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          clWeavePackage = cl-weave.packages.${system}.default;
        in
        {
          default = pkgs.mkShell {
            packages = [
              pkgs.nixpkgs-fmt
              pkgs.sbcl
              self.packages.${system}.default
              clWeavePackage
              paredit-cli.packages.${system}.default
            ];
            shellHook = ''
              export CL_SOURCE_REGISTRY="${clWeavePackage}/share/common-lisp/source//:$PWD//:''${CL_SOURCE_REGISTRY:-}"
            '';
          };
        }
      );
    };
}
