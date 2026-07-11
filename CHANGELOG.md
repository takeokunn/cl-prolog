# Changelog

All notable changes to `cl-prolog` are recorded in this file.

The format follows a simple Keep a Changelog style with an `Unreleased`
section at the top of the file.

## Unreleased

### Added

- cl-weave (Vitest-shaped) testing library integration: the new
  `cl-prolog/weave-tests` ASDF system exercises the public engine surface with
  `describe` / `it` / `expect` suites (unification, family relations, list and
  control-flow builtins, goal validation)
- `flake.nix` gains a `cl-weave` input and a `checks.weave-tests` derivation, so
  `nix flake check` runs the cl-weave suite locally and in CI with no extra
  entrypoint; `scripts/run-weave-tests.lisp` is the standalone runner

## 0.2.0 - 2026-07-10

Engine and API overhaul. The public surface was re-cut to the ideal API;
compatibility aliases were removed rather than deprecated.

### Added

- `map-prolog-solutions`: streaming query primitive exposing the engine's
  CPS contract directly (per-solution callback, `:limit`, `:project`,
  `:environment`, `:max-depth`)
- `:limit` keyword on `query-prolog`; `query-prolog-first` and
  `prolog-succeeds-p` now stop searching at the first proof
- `define-builtin`: public, arity-checked registry for builtin goal solvers;
  the head name may be a list of aliases (used by `!=` / `/=`)
- `invalid-goal-error` condition (with `invalid-goal-error-goal` reader) for
  malformed goals: wrong builtin arity, non-callable goal terms, and
  non-function `:when` guards now fail fast instead of failing silently
- `fresh-logic-variable` for writing custom builtins
- compiled `(:when EXPR)` guards: the `prolog` / `def-rule` / DCG `brace`
  macros compile guard expressions into closures over their logic variables
- `phrase` returns `(values remainder matched-p)`, distinguishing "no parse"
  from "full parse"
- table-driven test DSL (`deftest-queries`) and a broader regression suite
  (goal shapes, cut semantics, clause ordering, streaming, extensibility)
- `scripts/run-tests-noasdf.lisp`: ASDF-free core test entry point
- GitHub Actions CI running tests, the public-contract verifier, and the
  benchmark smoke
- `.github` community files and CI badge in `README.md`

### Changed

- sources moved from the repository root to `src/`, split by concern:
  `engine.lisp` now holds only the CPS provers and builtin registry, with
  builtins in `builtins.lisp`, DCG runtime in `dcg-runtime.lisp`, and the
  query API in `query.lisp`
- builtin and DCG solvers now stream through `EMIT` continuations end to end
  instead of collecting intermediate solution lists
- cut is structured around `%with-cut-barrier` / `%propagate-cut` with
  documented semantics: facts before rules, cut prunes the running clause's
  choice points and the predicate's remaining rule clauses
- variables inside facts are freshly renamed per use, fixing binding leaks
  across goals (`((same ?x ?x))` used to contaminate later goals)
- `unify` failure is now `(values nil nil)`; the `:unify-fail` sentinel is
  gone
- `prolog-match` documents fall-through to `nil`; `extend-rulebase` no
  longer copies clause lists it freshly consed

### Removed (breaking)

- `substitute-term` (use `logic-substitute`)
- `*max-proof-depth*` (use `*max-prolog-depth*`)
- `unify-failed-p`, `when-unify-succeeds`, `when-unify-fails`
  (use the two-value protocol of `unify`)
- `query-prolog-cps`, `prolog-succeeds-p-cps`
  (use `map-prolog-solutions`; the old functions were collect-then-callback
  wrappers, not CPS)
- runtime evaluation of `(:when EXPR)` goals in query data: `:when` now
  requires a function object; expression guards belong in the DSL macros

## 0.1.0 - 2026-07-09

Initial release: macro-first rulebase DSL, CPS proof search, builtin goals,
DCG support, examples, benchmark scenarios, public-contract verifier,
release audit tooling, Nix packaging, and repository policy files
(`SECURITY.md`, `CODE_OF_CONDUCT.md`, `SUPPORT.md`, `CONTRIBUTING.md`,
release checklist and troubleshooting documentation).
