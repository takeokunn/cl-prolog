# Release Checklist

This is the minimum evidence bar for calling a revision releasable.

## Documentation Review

Confirm that these files still describe the current code:

- `README.md`
- `docs/src/api-reference.md`
- `docs/src/architecture.md`
- `docs/src/troubleshooting.md`
- `CHANGELOG.md`
- `CONTRIBUTING.md`
- `CODE_OF_CONDUCT.md`
- `SECURITY.md`
- `SUPPORT.md`

If the public surface changed, update the docs in the same change.

## Verification Commands

Run:

```sh
nix run .
nix flake check
```

## What Must Be Green

- ASDF/cl-weave regression suite
- Nix packaging check when Nix is part of the release process
- the mdBook documentation build (`checks.documentation`)

## Refuse To Ship When

Do not ship when:

- public API changed without matching documentation updates
- examples no longer execute
- release docs or policy files are missing from the tracked tree
- tests pass only because regression coverage was silently removed
