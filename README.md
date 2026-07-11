# cl-prolog

[![CI](https://github.com/takeokunn/cl-prolog/actions/workflows/ci.yml/badge.svg)](https://github.com/takeokunn/cl-prolog/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

A small, dependency-free Prolog engine for Common Lisp, built around three ideas:

- **macro-first rule definition** — clauses are data, macros own the syntax
- **CPS proof search** — solutions stream through continuations, nothing buffers
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

Facts are one-element clauses; rules are a head followed by body goals.
Logic variables are `?`-prefixed symbols.

## Querying

```lisp
(query-prolog rb '(ancestor tom ?who))          ; all solutions
(query-prolog rb '(ancestor tom ?who) :limit 2) ; bounded search
(query-prolog-first rb '(ancestor ?x bob))      ; first solution or NIL
(prolog-succeeds-p rb '(ancestor tom eve))      ; boolean, stops at first proof

;; streaming: the function is called as each solution is proven
(map-prolog-solutions
 (lambda (solution) (format t "~&=> ~S~%" solution))
 rb '(ancestor tom ?who))
```

`with-prolog-query` binds variables from the first solution; `prolog-match`
dispatches like `cond` over queries.

## Builtin Goals

| Category | Goals | Meaning |
|---|---|---|
| Unification | `(= a b)`, `(/= a b)`, `(!= a b)` | unify two terms, or require that they cannot unify |
| Control | `!`, `(not g)`, `(and g...)`, `(or g...)` | cut, negation as failure, conjunction, and disjunction |
| Meta-call | `(call g)`, `(once g)`, `(repeat)` | invoke a goal, keep its first proof, or generate repeated proofs |
| Collection | `(findall t g ?bag)`, `(bagof t g ?bag)`, `(setof t g ?set)` | collect templates; `bagof` groups free variables and `setof` also removes duplicates and sorts |
| Dynamic database | `(asserta c)`, `(assertz c)`, `(retract c)`, `(abolish (p / n))`, `(clause h b)` | inspect or mutate clauses in the rulebase passed to the query |
| Arithmetic | `(is ?x expr)`, `(=:= a b)`, `(=\= a b)`, `(< a b)`, `(=< a b)`, `(> a b)`, `(>= a b)` | evaluate arithmetic expressions and compare their numeric values |
| Lists | `(member ?x list)`, `(append ?a ?b ?c)`, `(reverse ?a ?b)`, `(length ?l ?n)` | list relations in their supported proper-list modes |
| Lisp guard | `(:when fn ?x...)` | call a Lisp predicate with solved values |

Arithmetic expressions use prefix Lisp-shaped terms. The supported operators are
binary `+`, `-`, `*`, `/`, and `mod`, plus unary `-` and `abs`.

Collection and dynamic-database goals can be used like any other query:

```lisp
(query-prolog *family* '(findall ?child (parent tom ?child) ?children))
;; => (((?CHILDREN BOB)))

(query-prolog *family* '(assertz (parent tom eve)))
(query-prolog *family* '(parent tom ?child))
;; includes EVE

(query-prolog (make-rulebase) '(is ?total (+ 20 (* 2 11))))
;; => (((?TOTAL . 42)))
```

For explicit rulebase composition, `define-rulebase` creates a named rulebase
and `extend-rulebase` returns a new rulebase with additional clauses, leaving the
base object unchanged:

```lisp
(define-rulebase *base*
  ((color apple red)))

(defparameter *extended*
  (extend-rulebase *base*
    ((color lime green))))

(query-prolog *extended* '(color ?fruit ?color))
```

In the `prolog` / `def-rule` DSL you write guards as expressions —
`(:when (> ?n 10))` — and the macro compiles them to closures. The engine
never evaluates user expressions at runtime.

The builtin set is extensible:

```lisp
(define-builtin (twice input output) (rulebase environment depth emit)
  (let ((value (logic-substitute input environment)))
    (when (numberp value)
      (multiple-value-bind (extended ok) (unify output (* 2 value) environment)
        (when ok (funcall emit extended))))))
```

For a Lisp-side truth predicate without new bindings, specialize
`predicate-true-p` instead.

## DCG

```lisp
(def-dcg-rule noun (terminal :noun))
(def-dcg-rule verb (terminal :verb))

(def-dcg-rule sentence
  (dcg-star noun)
  (verb)
  (brace (= 1 1)))          ; Lisp guard, like (:when ...)

(phrase 'sentence '(:noun :noun :verb))
;; => NIL, T   (remainder, matched-p)
```

Combinators: `dcg-alt`, `dcg-opt`, `dcg-star`, `dcg-plus`,
`dcg-error-recovery`, plus token matchers `dcg-token-match` and
`dcg-token-match-value`.

## Semantics Notes

- **Clause order**: facts are always tried before rules; within each group,
  definition order is preserved.
- **Cut** prunes the running clause's remaining choice points and the
  predicate's remaining rule clauses.
- **Optional depth bound**: rule resolution is unbounded by default.  Set
  `:max-depth` to a non-negative integer to bound user-rule resolution;
  exhaustion signals `prolog-depth-limit-exceeded` rather than masquerading
  as logical failure.
- **Occurs check** is always on; unification never builds cyclic terms.

## Documentation

- [API reference](docs/api-reference.md)
- [Architecture](docs/architecture.md)
- [OSS readiness audit](docs/oss-readiness-audit.md)
- [Performance notes](docs/performance.md)
- [Public contract verifier](docs/public-contract-verifier.md)
- [Release audit](docs/release-audit.md)
- [Release checklist](docs/release-checklist.md)
- [Troubleshooting](docs/troubleshooting.md)
- [Quality gates](docs/quality-gates.md)

## Examples

```sh
sbcl --script examples/quick-start.lisp
sbcl --script examples/family-tree.lisp
sbcl --script examples/relational-lists.lisp
```

## Testing

```sh
sbcl --script tests.lisp
```

The core suite excludes the CLI-contract tests that spawn fresh SBCL
images. Enable them with `CL_PROLOG_TEST_SCRIPTS=1` before running
`sbcl --script tests.lisp` when you need the full script-contract layer.
For a fast, ASDF-free core run:

```sh
sbcl --script scripts/run-tests-noasdf.lisp
```

Release-level verification:

```sh
sbcl --script scripts/coverage.lisp
sbcl --script scripts/verify-public-contract.lisp
sbcl --script scripts/release-audit.lisp --with-benchmarks
sbcl --script scripts/release-audit.lisp --with-script-contracts
nix flake check
```

`scripts/verify-public-contract.lisp` also verifies that the shipped CI
workflow still contains the documented release gates and explicit timeout
declarations.

## Design Constraints

- no runtime dependencies, SBCL-tested, ANSI-leaning core
- a single canonical public API surface
- the exact export set is machine-checked against
  [`contracts/public-contract.sexp`](contracts/public-contract.sexp)

## Project Policy

- [Changelog](CHANGELOG.md)
- [Contributing](CONTRIBUTING.md)
- [Code of Conduct](CODE_OF_CONDUCT.md)
- [Security](SECURITY.md)
- [Support](SUPPORT.md)

## License

MIT — see [LICENSE](LICENSE).
