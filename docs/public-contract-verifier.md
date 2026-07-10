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
- documented ASDF systems are defined and loadable
- selected systems load in a fresh SBCL image
- example scripts exist, are tracked, and execute
- release docs exist and are tracked
- policy files exist and are tracked
- shipped docs do not mention deleted compatibility surfaces or removed
  release artifacts
- stable CLI scripts exist, are tracked, and satisfy `--help` and `--version`

Alias ASD files are no longer part of the contract.

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

That is intentional. The verifier protects the release tree, not only the
current worktree.

## Content Contract Failures

Checks such as `content/legacy-compatibility-references/...` fail when shipped
documentation still mentions deleted compatibility packages, removed `.asd`
aliases, or retired verifier/doc paths.

That is intentional. Public documentation drift is treated as a release
regression, not as a soft editorial issue.

## Fresh-Image Checks

Fresh-image validation exists to catch accidental load-order dependencies. A
system passing in the current SBCL image is weaker evidence than loading it
from a new process.

## Relationship To Other Gates

Use this script when you need exact contract drift detection.

Use [`docs/release-audit.md`](release-audit.md) when you need the broader
maintainer gate.
