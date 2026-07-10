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
- documented ASDF systems
- fresh-image loadability for selected systems
- example scripts
- release docs
- policy files
- forbidden legacy references in shipped documentation
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
- JSON contracts for shipped maintenance scripts

`scripts/run-tests-noasdf.lisp` runs the same core suites (minus the
script-contract tests, which spawn nested SBCL images) with plain `load`
and no ASDF dependency.

## Coverage Gate

```sh
sbcl --script scripts/coverage.lisp
```

compiles `src/` with `sb-cover` instrumentation, runs the core suites, and
writes an HTML report to `coverage/cover-index.html`.

The bar: every reachable expression and branch is exercised. As of 0.2.0
all files report 100% expression coverage except constant initforms that
SBCL folds away before instrumentation (five slot defaults in `data.lisp`,
two `(project t)` keyword defaults in `query.lisp`) — those never execute
as code, so `sb-cover` can never observe them.

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
