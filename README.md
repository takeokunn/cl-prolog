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
(ql:quickload :cl-prolog)

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

```lisp
(ql:quickload :cl-prolog)             ; via Quicklisp
```

```sh
nix run github:takeokunn/cl-prolog    # cl-weave regression suite, Linux-only Nix runner
```

See [Development](docs/src/development.md) for building from a local
checkout, running the test suite, and previewing the documentation site.

## Development

```sh
nix develop
nix run .          # cl-weave-backed ASDF regression suite (Linux only)
nix flake check    # full verification suite
```

## Project Policy

- [Changelog](CHANGELOG.md)
- [Contributing](CONTRIBUTING.md)
- [Code of Conduct](CODE_OF_CONDUCT.md)
- [Security](SECURITY.md)
- [Support](SUPPORT.md)

## License

MIT — see [LICENSE](LICENSE).
