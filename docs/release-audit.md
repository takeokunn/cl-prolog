# Release Audit

`scripts/release-audit.lisp` is the broad maintainer gate.

It orchestrates the smaller evidence layers and emits one summary.

## Commands

```sh
sbcl --script scripts/release-audit.lisp --help
sbcl --script scripts/release-audit.lisp --version
sbcl --script scripts/release-audit.lisp
sbcl --script scripts/release-audit.lisp --with-benchmarks
sbcl --script scripts/release-audit.lisp --with-nix
sbcl --script scripts/release-audit.lisp --with-script-contracts
sbcl --script scripts/release-audit.lisp --with-nix --with-benchmarks --json
sbcl --script scripts/release-audit.lisp --dry-run --json
```

## Checks

Default execution includes:

- `core`
- `tests`

`core` runs:

- `scripts/verify-public-contract.lisp --json`
- tracked release-artifact validation

That release-artifact set includes the shipped stable scripts
`benchmark.lisp`, `coverage.lisp`, `release-audit.lisp`, and
`verify-public-contract.lisp`.

`tests` runs:

- `tests.lisp`

Add `--with-script-contracts` when you also want the nested
`CL_PROLOG_TEST_SCRIPTS=1` layer that exercises `tests/scripts.lisp`.

Optional checks:

- `nix`: `nix flake check`
- `benchmarks`: `scripts/benchmark.lisp --json --iterations 100`
- `tests` with `--with-script-contracts`: `CL_PROLOG_TEST_SCRIPTS=1 sbcl --script tests.lisp`

## Exit Codes

- `0`: all requested checks passed
- `1`: at least one requested check failed
- `2`: invalid CLI usage

## JSON Contract

`--json` emits:

- `report_type`
- `project_version`
- `requested_checks`
- `dry_run`
- `ok`
- `exit_code`
- `results`

Each result contains:

- `status`
- `check`
- `command`
- `message`
- `details`

## Why This Exists

`verify-public-contract.lisp` is intentionally narrow. `release-audit.lisp`
adds orchestration around it, the regression suite, and optional packaging or
benchmark checks so maintainers can capture one release report before drilling
into individual failures.

## Diagnosis

When `core` fails:

- rerun `sbcl --script scripts/verify-public-contract.lisp`
- inspect which shipped file or export drifted

When `tests` fail:

- rerun `sbcl --script tests.lisp`
- rerun `sbcl --script scripts/release-audit.lisp --with-script-contracts`
- inspect the split test files under `tests/`

When `benchmarks` fail:

- rerun `sbcl --script scripts/benchmark.lisp --json --iterations 100`

When `nix` fails:

- rerun `nix flake check`
- confirm the clean source snapshot still includes release artifacts
