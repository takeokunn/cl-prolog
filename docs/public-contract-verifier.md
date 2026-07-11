# Public Contract Verifier

`scripts/verify-public-contract.lisp` is the narrowest machine check in this
repository.

It answers one question: does the shipped tree still match
`contracts/public-contract.sexp` exactly?

## Commands

```sh
sbcl --script scripts/verify-public-contract.lisp --help
sbcl --script scripts/verify-public-contract.lisp --version
sbcl --script scripts/verify-public-contract.lisp
sbcl --script scripts/verify-public-contract.lisp --json
```

## What It Checks

- exact export set of the documented public packages
- exact package nickname set
- documented script targets run successfully
- selected targets load in a fresh SBCL image
- example scripts exist, are tracked, and execute
- release docs exist and are tracked
- policy files exist and are tracked
- shipped CI workflows exist, are tracked, and still encode the documented
  release commands plus explicit timeout declarations
- shipped docs do not mention retired public surfaces or removed release
  artifacts
- subprocess-owning scripts keep explicit timeout declarations in the tracked
  source tree
- stable CLI scripts exist, are tracked, and satisfy `--help` and `--version`

The stable script set currently includes:

- `scripts/benchmark.lisp`
- `scripts/coverage.lisp`
- `scripts/release-audit.lisp`
- `scripts/verify-public-contract.lisp`

The contract names only canonical shipped artifacts.

## Exit Codes

- `0`: every check passed
- `1`: at least one check failed
- `2`: invalid CLI usage

## JSON Contract

`--json` emits a single object with:

- `manifest`
- `report_type`
- `project_version`
- `ok`
- `summary`
- `manifest_version`
- `results`

Each result object contains:

- `status`
- `check`
- `message`

## Tracked File Failures

Checks such as `doc-git/...`, `policy-git/...`, `example-git/...`, and
`script-git/...` fail when the file exists locally but is not tracked in git.

The same is true for `workflow-git/...`: CI configuration is part of the
shipped release evidence, not local-only scaffolding.

That is intentional. The verifier protects the release tree, not only the
current worktree.

## Content Contract Failures

Checks such as `content/retired-public-surface-references/...` fail when
shipped documentation still mentions retired public packages, removed `.asd`
artifacts, or retired verifier/doc paths.

That is intentional. Public documentation drift is treated as a release
regression, not as a soft editorial issue.

Checks such as `content-contract/scripts/release-audit-main.lisp` fail when a
tracked subprocess script drops one of the required timeout-bearing call sites
or the shared timeout runner stops encoding the timeout wrapper.

That is intentional. This repository treats explicit subprocess timeouts as a
release-quality invariant, not as a local implementation detail.

## Fresh-Image Checks

Fresh-image validation exists to catch accidental load-order dependencies. A
system passing in the current SBCL image is weaker evidence than loading it
from a new process.

## Workflow Contract Checks

Checks such as `workflow-contract/.github/workflows/ci.yml` fail when the
tracked CI workflow no longer contains the documented gate commands or drops
explicit `timeout-minutes` declarations.

That is intentional. This repository documents CI as part of the public
quality story, so workflow drift is treated as contract drift.

## Relationship To Other Gates

Use this script when you need exact contract drift detection.

Use [`docs/release-audit.md`](release-audit.md) when you need the broader
maintainer gate.
