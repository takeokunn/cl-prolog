# Support

`cl-prolog` is a library. Fast support depends on routing questions to the
right channel with enough concrete evidence to reproduce the issue.

## Usage questions

Open a GitHub issue when you need help with:

- choosing between immutable construction and explicit dynamic rulebase
  mutation
- understanding solver limits such as `:max-depth`

Include:

- the exact forms you evaluated
- the expected and actual results
- your Common Lisp implementation and version
- whether you loaded via Quicklisp, ASDF, or Nix

Before opening an issue, check
[`docs/src/troubleshooting.md`](docs/src/troubleshooting.md)
for common cases such as `(nil)` ground success, `:unify-fail`, `:max-depth`
limits, and release-gate failures.

## Bug reports

Open a GitHub issue for reproducible library defects.

Useful evidence:

- a minimal failing query or rulebase
- the public API entry point involved
- the exact command used to reproduce the failure
- whether `nix run .` passes locally
- whether `nix flake check` passes locally

## Security issues

Do not report suspected vulnerabilities in a public issue first.
Follow [`SECURITY.md`](SECURITY.md).

## Maintenance boundaries

The project prioritizes:

- documented public `cl-prolog` behavior
- macro-first rule definition and CPS query semantics
- reproducible packaging and example execution

The project does not promise support for:

- undocumented private `%...` internals
- full ISO Prolog behavior
- unbounded relational enumeration in every argument direction
