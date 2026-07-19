# Troubleshooting

## `query-prolog` returned `(nil)`

That means the query succeeded and projected no variables.

```lisp
(query-prolog rulebase '(parent tom bob))
;; => (nil)
```

Success with variables returns binding alists. Failure returns `nil`.

## `query-prolog-first` returned `nil`

That means no proof succeeded.

If you need the first successful binding only, this is the right entry point.
If you need every solution, use `query-prolog`.

## `solution-binding` returned `nil`

The variable may be unbound in that solution, or the variable may not exist in
the projected environment.

Inspect the whole solution first:

```lisp
(first (query-prolog rulebase '(ancestor tom ?who)))
```

## `unify` returned `nil`, `nil`

`unify` returns two values.

- first value: extended environment on success, or `nil` on failure
- second value: success flag

Failure is `(values nil nil)`. Ground success is `(values nil t)`, so callers
must inspect the second value rather than treating a `nil` environment as
failure.

## `:max-depth 0` rejected a derived rule

That is expected. Depth limits apply to user-rule expansion, and exhaustion
signals `prolog-depth-limit-exceeded` so an incomplete search is never
reported as logical failure.

- `0`: facts and built-ins only
- `1`: one rule expansion
- larger values: deeper derived proofs

## A foreign predicate did not run

`define-foreign-predicate` dispatches by exact name and arity. Check both
parts of the predicate indicator. Builtins remain authoritative when names
overlap.

## `phrase` returned `nil`

This has two meanings depending on the grammar:

- full token consumption on success
- no parse when the caller expected a non-`nil` remainder

Use `phrase-all` when you need to inspect every successful remainder stream.

## Nix checks fail only in a clean source tree

Check whether the file is tracked in git. The release checks intentionally
validate the tracked tree, not only the working directory.

## Script entry point cannot find the repository root

Confirm:

- you are running the script with `sbcl --script`
- you are invoking it from inside the repository checkout
- the script has not been copied away from the tree it expects to load

Direct smoke:

```sh
nix run .
```

The packaged Nix runner is Linux-only. On Darwin, skip this smoke step and
use ASDF directly for local library work.

## What To Include In A Bug Report

- exact query or macro form
- expected result
- actual result
- SBCL version
- output of `nix run .` when reproducing on Linux
- output of `nix flake check --print-build-logs`
