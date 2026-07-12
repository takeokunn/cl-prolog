# Development

## Environment

```sh
nix develop        # sbcl, cl-weave, paredit-cli, nixpkgs-fmt, mdbook
```

## Examples

```sh
sbcl --script examples/quick-start.lisp
sbcl --script examples/family-tree.lisp
sbcl --script examples/relational-lists.lisp
```

## Testing

The `cl-prolog/tests` ASDF system depends on
[cl-weave](https://github.com/takeokunn/cl-weave) and runs the complete
regression suite, including isolated table cases, per-query cases, fixtures,
and generated relational properties. Nix provides the self-contained runner:

```sh
nix run .
```

The packaged Nix app is supported on Linux only. On Darwin, use the Quicklisp
or ASDF workflow for library development and rely on `nix flake check` in CI
for the Linux verification path.

Pass any cl-weave CLI options after `--`; for example, to produce a JSON
result:

```sh
nix run . -- --reporter json --output cl-prolog-weave-results.json
```

The full Nix verification suite is:

```sh
nix flake check
```

This also runs `checks.paredit-lint`, a structural parse gate over every
tracked `.lisp`/`.asd` file, and `checks.documentation`, which builds the
mdBook site and fails if it does not produce a valid `index.html`.

### Query test helpers

Load the `cl-prolog/weave` ASDF system to use the public query test helpers:

```lisp
(asdf:load-system :cl-prolog/weave)
```

`deftest-queries` creates an independent cl-weave
case and a fresh rulebase for every query. A leading case label is optional;
without one, the printed query is used.

```lisp
(cl-prolog/weave:deftest-queries family-queries ((make-family-rulebase))
  ("keeps proof order" (parent alice ?child) :ordered
   (((?child . bob)) ((?child . carol))))
  ((parent alice ?child) :set
   (((?child . carol)) ((?child . bob))))
  ((parent alice ?child) :first ((?child . bob)))
  ((parent alice bob) :succeeds)
  ((parent bob alice) :fails)
  ((parent alice bob) :signals cl-prolog:invalid-max-depth-error
   :max-depth :invalid))
```

`:ordered`, `:set`, and `:first` take an expected value. `:set` ignores only
the order of complete solutions; it still compares the structure within each
solution with `equal`. `:signals` optionally takes a condition type. Query
options follow the expected value or assertion kind.

Use `assert-query` inside an existing cl-weave case when a table is not needed:

```lisp
(cl-weave:it "finds Alice's first child"
  (cl-prolog/weave:assert-query (make-family-rulebase)
    (parent alice ?child) :first ((?child . bob))))
```

## Documentation

```sh
nix build .#docs   # rendered site in ./result
mdbook serve docs  # live-reloading preview from the dev shell
```

## Design Constraints

- no runtime dependencies, SBCL-tested, ANSI-leaning core
- a single canonical public API surface

See [Release checklist](release-checklist.md) for the evidence bar a change
must clear before shipping.
