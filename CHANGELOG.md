# Changelog

All notable changes to `cl-prolog` are recorded in this file.

The format follows a simple Keep a Changelog style with an `Unreleased`
section at the top of the file.

## Unreleased

## 0.7.0 - 2026-07-24

### Added

- an SLG tabling engine (`table-variant.lisp`, `tabling.lisp`): variant-check
  keys, per-query table sessions, answer tables, and fixpoint iteration,
  threaded through the proof-state continuation so builtin-dispatched goals
  inherit the active tabling context
- a benchmarks harness (`benchmarks/`): in-process micro-benchmarks and a
  cross-engine comparison script against SWI-Prolog, Trealla, and Scryer
  Prolog on a shared, checksum-verified workload
- a CI matrix covering `x86_64-linux` and `aarch64-linux`

### Changed

- `get_code`/`peek_code` track end-of-stream identically to
  `get_char`/`peek_char` per ISO 8.11.3/8.11.4; stream alias validation is
  shared across the IO builtins
- goal-dispatch and DCG expansion hardened; finite-domain constraint
  handling refined
- environment indexing in unification uses a bounded overlay over an
  immutable base table instead of a full rehash on every binding, avoiding
  an O(n) rebuild per variable binding
- the parser, term/atom builtins, and source-loader were split into focused
  modules (lexer/grammar layers, term-inspect/compare/construct,
  atom-ops/text-conversion/atom-number-conversion,
  source-io/directives/registry/rollback) with no behavior change
- the regression suite was reorganized into thematic files (engine-runtime
  index-and-depth/foreign-and-registration/error-contract,
  builtin-collections/list/dynamic-database/arithmetic-and-flags) with a
  shared query helper lifted into `tests/support/core.lisp`
- documentation (architecture, API reference, builtin goals, querying,
  semantics, development, troubleshooting) synced with the module split and
  the new tabling and benchmarking surfaces

## 0.6.0 - 2026-07-19

### Added

- parser resource limits exported as configurable specials
  (`*max-prolog-source-characters*`, `*max-prolog-tokens*`,
  `*max-prolog-parser-depth*`, `*max-prolog-delimiter-depth*`,
  `*max-prolog-identifier-length*`, `*max-prolog-quoted-lexeme-length*`,
  `*max-prolog-numeric-lexeme-length*`, `*max-prolog-interned-symbols*`)
  with a new `prolog-parser-resource-error` condition, surfaced to Prolog
  code as catchable ISO `resource_error/1` terms

### Changed

- quoted `?`-prefixed atoms such as `'?x'` are now real atoms interned in
  `cl-prolog.user-atoms` instead of being misread as logic variables
- goal dispatch validates goals per ISO: variable goals raise
  `instantiation_error` and non-callable or improper-list goals raise
  `type_error`
- untrusted input no longer permanently interns symbols: syntax-error
  descriptions, missing-source pathnames, stream handles, operator
  specifiers, and arithmetic operator keys use uninterned symbols or table
  lookups; exponentiation results are bounded
- unification environments are hash-indexed and substitution is iterative,
  `assertz` and tabled-answer deduplication are O(1), left recursion is
  detected via strongly connected components, `bagof`/`setof` grouping is
  O(n log n), and `all_different` uses augmenting-path matching
- documentation matches the shipped API: real install/run instructions
  (cl-prolog is not on Quicklisp), accurate `phrase`/`phrase-all` and
  `unify` contracts, the complete exported-symbol reference, Linux-only
  flake outputs stated explicitly, and concrete security-reporting and
  code-of-conduct procedures

### Fixed

- parsed finite-domain ranges work: `X in 1..5` produced the prefix term
  `('..' 1 5)`, which the finite-domain store rejected; both the parsed
  prefix and Lisp-shaped infix range forms are now accepted
- closing the stream selected as `current_input`/`current_output` resets
  the selection to `user_input`/`user_output` instead of leaving a dangling
  stream entry
- `member/2` and `append/3` terminate on cyclic lists, and cyclic source
  lists passed to `consult`/`load_files` raise a resource error instead of
  looping
- a `.` inside `{...}` no longer ends a clause early during source
  splitting
- finite-domain arithmetic expressions no longer raise program errors from
  an outdated internal call site

## 0.5.0 - 2026-07-13

### Added

- predicate indexing for faster clause selection, with behavior preserved for
  variables, cyclic terms, dynamic predicates, and module-qualified calls
- explicit tabling, depth-limited calls, cyclic-term predicates, finite-domain
  helpers, and additional control and collection predicates
- broader ISO conformance coverage for arithmetic, meta-calls, modules,
  streams, term reading, exceptions, and relational list operations

### Changed

- CI and supported Nix flake systems now target Linux only; GitHub Actions runs
  the complete checks on Ubuntu
- module, query, depth-limit, constraint, dynamic-predicate, and stream state
  handling now preserve their execution context consistently
- documentation now covers the expanded runtime surface, Linux-only CI, and
  current release verification workflow

### Fixed

- unification, substitution, and query projection terminate safely for cyclic
  terms
- qualified and meta-callable goals now validate bindings, visibility, arity,
  and error terms consistently
- `length/2`, finite-domain equality, Prolog number recognition,
  `read_term/3` singleton reporting, and end-of-stream transitions now follow
  their intended relational or ISO semantics

## 0.4.1 - 2026-07-12

### Changed

- the stream read/write builtins (`read`/`read_term`, `write`-family, `nl`,
  `flush_output`, character/byte I/O, `at_end_of_stream`) moved from
  `io.lisp` into the new `io-streams.lisp`, defined through a shared
  `%define-io-dual-builtin` macro that derives the current-stream and
  explicit-stream variants from one specification; the macro validates its
  clause plist at macroexpansion time so a malformed definition fails the
  build instead of compiling a builtin that silently always fails
- `%io-options` and `%io-read-options` share one option-list parser
  (`%io-parse-option-list`) instead of two divergent copies
- `query-prolog` and `query-prolog-first` reuse the solution-mapping core
  of `map-prolog-solutions` instead of re-decoding their options through
  the public entry point
- `deftest-table` / `deftest-unification` expand each table spec into its
  own named cl-weave case, so a failing spec is reported individually

### Added

- a regression test that `read_term/2` rejects unsupported read options
  with an ISO domain error

## 0.4.0 - 2026-07-12

### Added

- the remaining ISO 13211-1 built-ins: `open/3`, `write_canonical/1,2`,
  `halt/0,1` (raising the exported `prolog-halt` condition so embedders
  choose how to exit; `catch/3` does not intercept it), `char_conversion/2`,
  and `current_char_conversion/2`; conversions are rulebase-local and apply
  during `read_term` and `consult` when the `char_conversion` flag is `on`,
  leaving quoted tokens untouched
- the remaining ISO source directives: `include/1` splices the included
  file's terms into the including source unit, `set_prolog_flag/2` and
  `char_conversion/2` execute during loading and affect subsequent terms,
  and `discontiguous/1` / `multifile/1` declarations are validated and
  accepted (the engine already resolves clauses independently of textual
  grouping)

- cl-weave (Vitest-shaped) testing library integration: the new
  `cl-prolog/tests` ASDF system exercises the public engine surface with
  `describe` / `it` / `expect` suites (unification, family relations, list and
  control-flow builtins, goal validation)
- `flake.nix` gains a `cl-weave` input and a `cl-prolog/tests` check, so
  `nix flake check` runs the complete ASDF suite locally and in CI with no
  project-local runner
- `flake.nix` gains a `paredit-cli` input: `nix develop` puts the `paredit`
  structural S-expression CLI on `PATH` for renames, moves, and other
  refactors of Lisp sources, and `checks.paredit-lint` fails `nix flake
  check` if any tracked `.lisp`/`.asd` file is not a balanced S-expression
  document

### Changed

- Prolog flag names use their ISO spellings (`max_arity`,
  `integer_rounding_function`, `char_conversion`, `double_quotes`), and
  `integer_rounding_function` reports `toward_zero`, matching the
  `truncate`-based `//`
- `define-builtin` no longer emits a per-call arity re-check: solver
  dispatch already guarantees the arity, so the check was unreachable
- cut (`!`) is now implemented with `CATCH`/`THROW` tags carried through the
  proof state instead of dynamically-scoped condition handlers, so a cut in a
  clause body correctly prunes both the alternatives of goals to its left and
  the remaining clauses of its own predicate — without leaking through
  predicate invocations that merely run in its continuation
- `and`, `or`, and the taken branch of `->` / `*->` are transparent to cut,
  while `call/1`, `once/1`, `\+/1`, `findall`-family goals, `catch/3`, and the
  condition of `->` keep cuts local, matching ISO
- calling a builtin name at an unsupported arity (e.g. `=/1`) signals the ISO
  `existence_error` for that predicate indicator instead of an engine-specific
  arity error

### Fixed

- the bitwise arithmetic operator names `\\`, `/\\`, and `\\/` and the
  `\\+` / `\\==` exports were written with a single backslash inside
  multiple-escape bars, which the reader treats as an escaped `|`; the
  runaway token silently swallowed the entire binary arithmetic table, so
  every `is/2` query over a binary operator died on an unbound table
- `atan/2` now checks both operands are real and evaluates in double-float,
  so `atan(0, -1) =:= pi` holds
- `stream_property/2` no longer crashes computing `end_of_stream` for input
  streams (an internal helper was called with the wrong argument count)
- `(and true !)`-style conjunctions whose first goal is a bare atom are no
  longer misread as a single compound goal

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
- GitHub Actions CI running tests and the benchmark smoke
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
DCG support, examples, benchmark scenarios, Nix packaging, and repository policy files
(`SECURITY.md`, `CODE_OF_CONDUCT.md`, `SUPPORT.md`, `CONTRIBUTING.md`,
release checklist and troubleshooting documentation).
