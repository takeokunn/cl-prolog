# cl-prolog

`cl-prolog` is a small, dependency-free Prolog engine for Common Lisp, built
around three ideas:

- **macro-first rule definition** — clauses are data, macros own the syntax
- **CPS proof search** — the engine emits solutions through continuations;
  callers choose streaming or collection
- **data / logic separation** — rulebases are plain structs the engine walks

The public package is `cl-prolog`.

## Quick start

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

Facts are one-element clauses; rules are a head followed by body goals.
Logic variables are `?`-prefixed symbols.

## Where to go next

- [Querying](querying.md) — the query entry points and their contracts
- [Builtin goals](builtin-goals.md) — unification, control, collection,
  database, arithmetic, and list goals
- [Rule DSL](rule-dsl.md) — rulebase construction, `:when` guards, foreign
  predicates
- [DCG](dcg.md) — grammar rules and combinators
- [Semantics](semantics.md) — clause order, cut, depth bounds, occurs check
- [API reference](api-reference.md) — the complete symbol index
- [Architecture](architecture.md) — module layout and the CPS engine
- [Development](development.md) — testing, examples, and design constraints
- [Troubleshooting](troubleshooting.md) — common surprises and fixes
- [Release checklist](release-checklist.md) — the evidence bar for shipping
- [Repository documentation](repository.md) — changelog, contributing, policy

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

To run the cl-weave regression suite through the Linux-only Nix app:

```sh
nix run github:takeokunn/cl-prolog
```
