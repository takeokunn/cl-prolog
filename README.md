# cl-prolog

[![CI](https://github.com/takeokunn/cl-prolog/actions/workflows/ci.yml/badge.svg)](https://github.com/takeokunn/cl-prolog/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

A small, dependency-free Prolog engine for Common Lisp, built around three ideas:

- **macro-first rule definition** — clauses are data, macros own the syntax
- **CPS proof search** — the engine emits solutions through continuations; callers
  choose streaming or collection
- **data / logic separation** — rulebases are plain structs the engine walks

The public package is `cl-prolog`.

## Quick Start

```lisp
(require :asdf)
(asdf:load-asd (truename "cl-prolog.asd")) ; run from the repository root
(asdf:load-system :cl-prolog)

(in-package #:cl-prolog)

(define-rulebase *family*
  ((parent tom bob))
  ((parent bob alice))
  ((ancestor ?x ?y) (parent ?x ?y))
  ((ancestor ?x ?y) (parent ?x ?z) (ancestor ?z ?y)))

(query-prolog *family* '(ancestor tom ?who))
;; => (((?WHO . BOB)) ((?WHO . ALICE)))
```

Start with the [documentation source](docs/src/README.md) for querying,
builtin goals, the rule DSL, DCG support, and semantics. The published site
is available at <https://takeokunn.github.io/cl-prolog/>.

## Install

cl-prolog is not currently distributed by Quicklisp. Clone the repository and
either load its ASDF definition directly or place the checkout in a directory
configured in your [ASDF source registry](https://asdf.common-lisp.dev/asdf.html#Configuring-ASDF).

```sh
git clone https://github.com/takeokunn/cl-prolog.git
cd cl-prolog
sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(asdf:load-asd (truename "cl-prolog.asd"))' \
  --eval '(asdf:load-system :cl-prolog)'
```

```sh
nix run github:takeokunn/cl-prolog
```

This runs the cl-weave regression suite through the Linux-only Nix runner.

See [Development](docs/src/development.md) for building from a local
checkout, running the test suite, and previewing the documentation site.

## Development

The flake currently defines outputs for `x86_64-linux` and `aarch64-linux`
only. Run these commands on Linux:

```sh
nix develop
nix run .          # cl-weave-backed ASDF regression suite
nix flake check    # full verification suite
```

On Darwin and other platforms, use the local ASDF workflow above. The flake
does not currently expose development shells, packages, checks, or apps for
those systems; Linux Nix verification runs in CI.

## Project Policy

- [Changelog](CHANGELOG.md)
- [Contributing](CONTRIBUTING.md)
- [Code of Conduct](CODE_OF_CONDUCT.md)
- [Security](SECURITY.md)
- [Support](SUPPORT.md)

## License

MIT — see [LICENSE](LICENSE).
