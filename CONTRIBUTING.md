# Contributing to cl-prolog

## Scope

`cl-prolog` is a macro-first Prolog engine for Common Lisp.
Changes should preserve the documented `cl-prolog` public API, the CPS query
surface, and the explicit separation between rule data and solver logic.

Community conduct expectations are defined in [`CODE_OF_CONDUCT.md`](CODE_OF_CONDUCT.md).
User support and triage routing are defined in [`SUPPORT.md`](SUPPORT.md).

## Development workflow

1. Enter the development environment with `nix develop` when Nix is available.
2. Run the library test suite before and after changes:
   `sbcl --non-interactive --eval '(require :asdf)' --load cl-prolog.asd --eval '(asdf:test-system :cl-prolog)'`
3. Run `nix flake check` for packaging and reproducibility coverage when the
   change affects distribution, ASDF loading, or documentation examples.
4. When adding release artifacts exposed from `README.md` or `docs/`,
   ensure the files are tracked in git before relying on `nix flake check`:
   this repository's `cleanSourceWith` flake source omits untracked files,
   so docs, example scripts, alias `.asd` files, and verifier entrypoints can
   disappear from Nix builds even when they exist in a dirty worktree.

## Compatibility expectations

- Treat `README.md` and `docs/api-reference.md` as the public contract.
- Treat `docs/release-checklist.md` as the minimum evidence gate before a
  change is considered releasable.
- Treat `docs/release-audit.md` as the normative maintainer reference for the
  bundled release gate CLI and result semantics.
- Add or update tests when changing built-ins, unification, proof search, DCG,
  or macro expansion behavior.
- Do not reintroduce compatibility aliases, alternate package surfaces, or
  deleted release artifacts into shipped docs or policy files.

## Documentation expectations

- Update `README.md` when user-visible behavior changes.
- Update `CHANGELOG.md` when a user-visible feature, public contract, or
  maintenance policy changes.
- Update `docs/release-checklist.md` when release evidence or ship criteria
  change.
- Update `docs/release-audit.md` when the bundled release gate CLI, JSON
  output, or exit-code contract changes.
- Update `docs/public-contract-verifier.md` when the verifier's manifest keys,
  result classes, or content checks change.
- Update `SECURITY.md` when the supported-version or reporting policy changes.
- Track newly added docs, examples, and verifier scripts in git
  before treating `nix flake check` as release evidence.

## Change review checklist

- public API changes are documented
- tests cover the new behavior or regression
- release claims are evidence-backed
- example scripts still load
