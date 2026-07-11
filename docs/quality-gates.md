# Quality Gates

This repository treats release quality as explicit evidence, not as a vague
checklist.

## Core Gates

```sh
sbcl --script scripts/verify-public-contract.lisp --json
sbcl --script tests.lisp
sbcl --script scripts/run-tests-noasdf.lisp
sbcl --script scripts/benchmark.lisp --json --iterations 1
sbcl --script scripts/release-audit.lisp --with-benchmarks --json
```

Add:

```sh
nix flake check
```

when Nix packaging is part of the release path.

CI (`.github/workflows/ci.yml`) runs all of the above on Linux and macOS
for every push and pull request.

## Public Contract Gate

`scripts/verify-public-contract.lisp` verifies:

- exact exports
- exact nicknames
- documented script targets
- fresh-image loadability for selected systems
- example scripts
- release docs
- policy files
- shipped CI workflow structure and timeout declarations
- timeout-bearing subprocess contracts for shipped scripts
- forbidden retired surface references in shipped documentation
- stable CLI scripts

This is the narrowest ship/no-ship signal for the release tree.

## Regression Gate

`tests.lisp` is the behavioral regression gate.

It should cover:

- fact queries
- recursive rules
- variable projection
- cut semantics
- DCG behavior
- macro-oriented usage

The CLI JSON-contract layer is opt-in: set `CL_PROLOG_TEST_SCRIPTS=1`
before running `tests.lisp` when you need the fresh-SBCL script-contract
checks.

`scripts/run-tests-noasdf.lisp` runs the same core suites (minus the
script-contract tests, which spawn nested SBCL images) with plain `load`
and no ASDF dependency.

The script JSON-contract tests (`tests/scripts.lisp`) are opt-in: set
`CL_PROLOG_TEST_SCRIPTS=1` before running `tests.lisp`. They spawn a tree of
fresh SBCL images, which is too heavy for nix build sandboxes and
memory-constrained CI runners; CI runs the underlying scripts directly as
workflow steps instead.

## Coverage Gate

```sh
sbcl --script scripts/coverage.lisp
sbcl --script scripts/coverage.lisp --help
sbcl --script scripts/coverage.lisp --version
```

compiles `src/` with `sb-cover` instrumentation, runs the core suites, and
writes an HTML report to `coverage/cover-index.html`.

The bar: every reachable expression and branch is exercised. In the current
tree, the generated `sb-cover` report shows 100.0% expression coverage and
100.0% branch coverage for all files in `src/`.

## Benchmark Gate

`scripts/benchmark.lisp` is a smoke gate, not a hard performance threshold.

It protects against obvious semantic drift in the benchmark scenarios while
still reporting timings.

## Release Gate

`scripts/release-audit.lisp` is the aggregation layer.

Use it when you want one machine-readable release report instead of several
independent command outputs.

By default it runs the public-contract gate and `tests.lisp`. Add
`--with-benchmarks` when benchmark smoke belongs in the same release report.
Add `--with-script-contracts` when the release report should also exercise
`tests/scripts.lisp` through `CL_PROLOG_TEST_SCRIPTS=1`.
