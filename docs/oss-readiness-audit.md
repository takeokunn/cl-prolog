# OSS Readiness Audit

This document describes the current OSS quality bar for `cl-prolog`.

## What Exists

- a machine-readable public contract in `contracts/public-contract.sexp`,
  verified by `scripts/verify-public-contract.lisp`
- the public contract also checking that `.github/workflows/ci.yml` remains
  tracked and still contains the documented release gates plus explicit
  timeout declarations
- a split regression suite under `tests/` with a table-driven expectation
  DSL, runnable through `tests.lisp` and the fast ASDF-free core runner
- an expression/branch coverage gate (`scripts/coverage.lisp`, sb-cover):
  100% on every reachable expression in the current tree
- benchmark smoke in `scripts/benchmark.lisp`
- a maintainer release gate in `scripts/release-audit.lisp`
- GitHub Actions CI (`.github/workflows/ci.yml`) running tests, the
  contract verifier, benchmark smoke, examples, and `nix flake check`
  on Linux and macOS
- release documentation and top-level policy files treated as shipped,
  git-tracked artifacts
- Nix packaging with clean-source filtering

## What This Means

- the public API is explicit and machine-checked — an export can neither
  appear nor disappear without failing a gate
- release artifacts and verification commands are explicit
- every push is exercised on two platforms in CI

## Remaining Gaps

- publish to Quicklisp/Ultralisp so `(ql:quickload :cl-prolog)` works from
  the public dist rather than a local checkout
- cut a tagged release once CI is green on `main`
- multi-implementation support (CCL, ECL) is untested; the core avoids
  implementation-specific code but only SBCL is exercised

## Practical Release Bar

1. `scripts/verify-public-contract.lisp` passes
2. `tests.lisp` passes (core regression suite; include `CL_PROLOG_TEST_SCRIPTS=1` to run the script JSON-contract meta-tests)
3. `scripts/coverage.lisp` reports no newly-uncovered expressions
4. `scripts/benchmark.lisp` smoke passes
5. `scripts/release-audit.lisp` passes for the requested checks
6. `nix flake check` passes when Nix packaging is part of the release claim
7. CI is green on the release commit

Back any release claim with fresh command output from the current tree,
not with this document alone.
