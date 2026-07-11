{
  description = "Dependency-free Common Lisp Prolog engine";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  # cl-weave is the Vitest-shaped testing library used by the weave-tests
  # suite.  It follows this flake's nixpkgs so both share a single SBCL.
  inputs.cl-weave.url = "github:takeokunn/cl-weave";
  inputs.cl-weave.inputs.nixpkgs.follows = "nixpkgs";

  outputs = { self, nixpkgs, cl-weave }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f: builtins.listToAttrs (map
        (system: {
          name = system;
          value = f system;
        })
        systems);
      sourceFor = pkgs: pkgs.lib.cleanSourceWith {
        src = ./.;
        filter = path: type:
          (pkgs.lib.cleanSourceFilter path type)
          && (let
            name = builtins.baseNameOf path;
          in
            !(pkgs.lib.hasSuffix ".fasl" name
              || pkgs.lib.hasSuffix ".cfasl" name
              || pkgs.lib.hasSuffix ".dfsl" name
              || pkgs.lib.hasSuffix ".ufasl" name
              || pkgs.lib.hasSuffix ".core" name
              || pkgs.lib.hasSuffix ".o" name));
      };
    in
    {
      formatter = forAllSystems (system:
        let pkgs = import nixpkgs { inherit system; };
        in pkgs.nixpkgs-fmt);

      packages = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
          src = sourceFor pkgs;
        in
        {
          default = pkgs.sbcl.buildASDFSystem {
            pname = "cl-prolog";
            version = "0.2.0";
            src = src;
            systems = [ "cl-prolog" ];
          };
        });

      checks = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
          src = sourceFor pkgs;
        in
        {
          default = pkgs.runCommand "cl-prolog-tests"
            {
              nativeBuildInputs = [ pkgs.sbcl self.packages.${system}.default ];
            } ''
              cp -R ${src} source
              chmod -R u+w source
              cd source
              export HOME="$TMPDIR/home"
              export XDG_CACHE_HOME="$TMPDIR/cache"
              mkdir -p "$HOME" "$XDG_CACHE_HOME"
              sbcl --non-interactive \
                --eval '(require :asdf)' \
                --eval '(asdf:load-asd (truename "cl-prolog.asd"))' \
                --eval '(asdf:test-system :cl-prolog)'
              touch $out
            '';

          # Run the cl-weave based suite.  cl-weave is exposed to ASDF through
          # CL_SOURCE_REGISTRY; its flake package installs its source tree under
          # share/common-lisp/source/cl-weave.
          weave-tests = pkgs.runCommand "cl-prolog-weave-tests"
            {
              nativeBuildInputs = [ pkgs.sbcl ];
            } ''
              cp -R ${src} source
              chmod -R u+w source
              cd source
              export HOME="$TMPDIR/home"
              export XDG_CACHE_HOME="$TMPDIR/cache"
              mkdir -p "$HOME" "$XDG_CACHE_HOME"
              export CL_SOURCE_REGISTRY="${cl-weave.packages.${system}.default}/share/common-lisp/source//:$PWD//:"
              sbcl --script scripts/run-weave-tests.lisp
              touch $out
            '';
        });

      apps = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
          test = pkgs.writeShellApplication {
            name = "cl-prolog-test";
            runtimeInputs = [ pkgs.sbcl self.packages.${system}.default ];
            text = ''
              export XDG_CACHE_HOME="''${XDG_CACHE_HOME:-$PWD/.cache}"
              mkdir -p "$XDG_CACHE_HOME"
              sbcl --non-interactive \
                --eval '(require :asdf)' \
                --eval '(asdf:load-asd (truename "cl-prolog.asd"))' \
                --eval '(asdf:test-system :cl-prolog)'
            '';
          };
        in
        {
          test = {
            type = "app";
            program = "${test}/bin/cl-prolog-test";
            meta.description = "Run the cl-prolog ASDF test suite";
          };
          default = self.apps.${system}.test;
        });

      devShells = forAllSystems (system:
        let pkgs = import nixpkgs { inherit system; };
        in {
          default = pkgs.mkShell {
            packages = [
              pkgs.nixpkgs-fmt
              pkgs.sbcl
              self.packages.${system}.default
            ];
          };
        });
    };
}
