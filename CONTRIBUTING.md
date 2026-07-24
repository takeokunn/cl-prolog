# Contributing to cl-prolog

## Scope

`cl-prolog` is a macro-first Prolog engine for Common Lisp.
Changes should preserve the documented `cl-prolog` public API, the CPS query
surface, and the explicit separation between rule data and solver logic.

Community conduct expectations are defined in [`CODE_OF_CONDUCT.md`](CODE_OF_CONDUCT.md).
User support and triage routing are defined in [`SUPPORT.md`](SUPPORT.md).

## Development workflow

1. On Linux, enter the pinned development environment with `nix develop`.
   The current flake exposes Linux systems only. On Darwin, use a local SBCL
   and ASDF setup instead; ensure both `cl-prolog` and
   [`cl-weave`](https://github.com/nerima-lisp/cl-weave) are discoverable through
   ASDF before running tests.
2. Run the cl-weave-backed library test suite before and after changes:

   ```sh
   sbcl --non-interactive \
     --eval '(require :asdf)' \
     --load cl-prolog.asd \
     --eval '(asdf:test-system :cl-prolog)'
   ```

   This command requires `cl-weave`; loading `cl-prolog.asd` alone does not
   provide that dependency. On Linux, `nix run .` supplies the pinned dependency
   and runs the same ASDF test system.
3. On Linux, run `nix flake check` for packaging and reproducibility coverage
   when the change affects distribution, ASDF loading, or documentation
   examples. This also runs `checks.paredit-lint`, which fails if any tracked
   `.lisp`/`.asd` file is not a balanced S-expression document and loads the
   shipped examples through `checks.examples`. On Darwin, `nix flake check`
   may exit successfully after omitting all Linux-only checks; that result is
   not verification. Run the ASDF test command above and rely on Linux CI for
   the flake-level gate.
4. When adding release artifacts exposed from `README.md` or `docs/`,
   ensure the files are tracked in git before relying on `nix flake check`:
   Git-backed flake input selection excludes untracked files before this
   repository's `cleanSourceWith` filter runs, so docs, examples, and alias
   `.asd` files can disappear from Nix builds even when they exist in a dirty
   worktree.

## Structural refactors

`nix develop` puts [`paredit`](https://github.com/nerima-lisp/paredit-cli) on
`PATH`. Prefer it over hand-editing parentheses for renames, moves, and other
structural changes to Lisp sources:

```sh
paredit inspect check --file src/engine.lisp
paredit refactor rename-function --from old-name --to new-name --output json src/*.lisp
```

Run a plan/preview command without `--write` first, review the JSON, then
re-run with `--write`. See the tool's own docs for the full command surface.

## Compatibility expectations

- Keep `README.md` and `docs/src/api-reference.md` aligned with the public API.
- Treat `docs/src/release-checklist.md` as the minimum evidence gate before a
  change is considered releasable.
- Add or update tests when changing built-ins, unification, proof search, DCG,
  or macro expansion behavior.
- Do not reintroduce compatibility aliases, alternate package surfaces, or
  deleted release artifacts into shipped docs or policy files.

## Documentation expectations

- Update `README.md` when user-visible behavior changes.
- Update `CHANGELOG.md` when a user-visible feature, API, or maintenance policy
  changes.
- Update `docs/src/release-checklist.md` when release evidence or ship criteria
  change.
- Update `SECURITY.md` when the supported-version or reporting policy changes.
- Track newly added docs and examples in git
  before treating `nix flake check` as release evidence.

## Change review checklist

- public API changes are documented
- tests cover the new behavior or regression
- release claims are evidence-backed
- examples still load through ASDF
