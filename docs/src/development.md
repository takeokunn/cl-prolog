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
