# Release Checklist

This is the minimum evidence bar for calling a revision releasable.

## Documentation Review

Confirm that these files still describe the current code:

- `README.md`
- `docs/api-reference.md`
- `docs/architecture.md`
- `docs/oss-readiness-audit.md`
- `docs/performance.md`
- `docs/public-contract-verifier.md`
- `docs/release-audit.md`
- `docs/quality-gates.md`
- `docs/troubleshooting.md`
- `CHANGELOG.md`
- `CONTRIBUTING.md`
- `CODE_OF_CONDUCT.md`
- `SECURITY.md`
- `SUPPORT.md`

If the public surface changed, update the docs in the same change.

## Verification Commands

Run:

```sh
sbcl --script scripts/verify-public-contract.lisp
sbcl --script tests.lisp
sbcl --script scripts/benchmark.lisp --json --iterations 1
sbcl --script scripts/release-audit.lisp --with-benchmarks
sbcl --script scripts/release-audit.lisp --with-script-contracts
nix flake check
```

Use `--json` variants when you need machine-readable evidence.

`scripts/release-audit.lisp` runs the public-contract verifier and
`tests.lisp` by default. Add `--with-benchmarks` when the release evidence
should include benchmark smoke in the same report. Add
`--with-script-contracts` when the report should also run
`tests/scripts.lisp` through `CL_PROLOG_TEST_SCRIPTS=1`.

## What Must Be Green

- public contract verification
- split regression suite
- benchmark smoke
- release audit
- Nix packaging check when Nix is part of the release process

## Refuse To Ship When

Do not ship when:

- exports changed without manifest and doc updates
- examples no longer execute
- release docs or policy files are missing from the tracked tree
- tests pass only by silently narrowing the public contract
- benchmark smoke reports semantic drift
