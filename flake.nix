{
  description = "Dependency-free Common Lisp Prolog engine";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  # cl-weave is the testing library used by the cl-prolog/tests ASDF system.
  # suite.  It follows this flake's nixpkgs so both share a single SBCL.
  inputs.cl-weave.url = "github:takeokunn/cl-weave/v0.6.0";
  inputs.cl-weave.inputs.nixpkgs.follows = "nixpkgs";
  inputs.cl-weave.inputs.paredit-cli.follows = "paredit-cli";

  # paredit-cli provides structural S-expression tooling for this repo's
  # Lisp sources: a dev-shell binary for agent-driven refactors and a
  # structural-parse lint gate reused in `checks`.
  inputs.paredit-cli.url = "github:takeokunn/paredit-cli/v0.4.0";
  inputs.paredit-cli.inputs.nixpkgs.follows = "nixpkgs";

  outputs =
    { self
    , nixpkgs
    , cl-weave
    , paredit-cli
    ,
    }:
    let
      projectVersion = "0.6.0";
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      forAllSystems =
        f:
        builtins.listToAttrs (
          map
            (system: {
              name = system;
              value = f system;
            })
            systems
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
      mkDocs =
        pkgs:
        pkgs.stdenvNoCC.mkDerivation {
          pname = "cl-prolog-docs";
          version = projectVersion;
          src = pkgs.lib.fileset.toSource {
            root = ./docs;
            fileset = pkgs.lib.fileset.unions [
              ./docs/book.toml
              ./docs/src
            ];
          };
          nativeBuildInputs = [ pkgs.mdbook ];
          buildPhase = ''
            runHook preBuild
            mdbook build --dest-dir "$out" .
            runHook postBuild
          '';
          dontInstall = true;
          meta = {
            description = "Rendered mdBook documentation for cl-prolog";
            license = pkgs.lib.licenses.mit;
          };
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
          cl-prolog = pkgs.sbcl.buildASDFSystem {
            pname = "cl-prolog";
            version = projectVersion;
            src = src;
            systems = [ "cl-prolog" ];
          };
        in
        {
          inherit cl-prolog;
          default = cl-prolog;
          docs = mkDocs pkgs;
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

          # Instrument production sources, retain the HTML report, and reject
          # regressions from the current expression and branch coverage ratios.
          coverage =
            pkgs.runCommand "cl-prolog-coverage"
              {
                nativeBuildInputs = [ pkgs.sbcl pkgs.perl ];
              }
              ''
                cp -R ${src} source
                chmod -R u+w source
                cd source
                export HOME="$TMPDIR/home"
                export XDG_CACHE_HOME="$TMPDIR/cache"
                export COVERAGE_OUTPUT="$out/html/"
                mkdir -p "$HOME" "$XDG_CACHE_HOME" "$COVERAGE_OUTPUT"
                export CL_SOURCE_REGISTRY="${
                  cl-weave.packages.${system}.default
                }/share/common-lisp/source//:$PWD//:"
                sbcl --noinform --non-interactive \
                  --eval '(require :asdf)' \
                  --eval '(require :sb-cover)' \
                  --load tests/coverage-runner.lisp

                perl -e '
                  use strict;
                  use warnings;
                  my ($expression_covered, $expression_total) = (0, 0);
                  my ($branch_covered, $branch_total) = (0, 0);
                  while (<>) {
                    while (m{<tr class=.?(?:odd|even).+?</a></td><td>(\d+)</td><td>(\d+)</td><td>.*?</td><td>(\d+)</td><td>(\d+)</td>}g) {
                      $expression_covered += $1;
                      $expression_total += $2;
                      $branch_covered += $3;
                      $branch_total += $4;
                    }
                  }
                  die "coverage report contained no instrumented expressions\n"
                    unless $expression_total;
                  my $expression_percent = 100 * $expression_covered / $expression_total;
                  my $branch_percent = $branch_total
                    ? 100 * $branch_covered / $branch_total
                    : 100;
                  open my $json, ">", "$ENV{COVERAGE_OUTPUT}/summary.json"
                    or die "cannot write coverage summary: $!\n";
                  printf {$json}
                    qq|{"expression":{"covered":%d,"total":%d,"percent":%.2f},| .
                    qq|"branch":{"covered":%d,"total":%d,"percent":%.2f}}\n|,
                    $expression_covered, $expression_total, $expression_percent,
                    $branch_covered, $branch_total, $branch_percent;
                  close $json;
                  printf "Expression coverage: %d/%d (%.2f%%)\n",
                    $expression_covered, $expression_total, $expression_percent;
                  printf "Branch coverage: %d/%d (%.2f%%)\n",
                    $branch_covered, $branch_total, $branch_percent;
                  die "expression coverage regressed below 13001/13977\n"
                    if $expression_covered * 13977 < 13001 * $expression_total;
                  die "branch coverage regressed below 1568/1806\n"
                    if $branch_covered * 1806 < 1568 * $branch_total;
                ' "$COVERAGE_OUTPUT/cover-index.html"
              '';

          # Structural parse gate over every tracked Lisp source: fails if
          # any .lisp/.asd file is not a balanced S-expression document.
          paredit-lint = paredit-cli.lib.${system}.mkLintCheck {
            src = src;
            name = "cl-prolog-paredit-lint";
          };

          # Fails if the mdBook site does not build to a valid index.html.
          documentation =
            pkgs.runCommand "cl-prolog-documentation" { docs = self.packages.${system}.docs; }
              ''
                test -f "$docs/index.html"
                touch $out
              '';
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
              pkgs.mdbook
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
