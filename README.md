# cl-prolog

`cl-prolog` is a small, dependency-free Common Lisp Prolog engine.

The public API is shaped to replace the lightweight Prolog layers currently
embedded in `cl-cc`, `private-trade-fx`, and `nshell`.

It provides:

- `fx.prolog` as the primary package
- `cl-cc/prolog` as a compatibility nickname
- immutable rulebase construction helpers and mutable assertion helpers
- unification with occurs-check
- depth-first proof search
- `query-prolog`, `query-prolog-first`, `prolog-succeeds-p`
- CPS adapters for `query-prolog`, `prolog-succeeds-p`, and `merge-rulebase-facts`
- `define-rulebase`, `extend-rulebase`, `with-prolog-query`, `prolog-match`
- `def-rule`, `register-prolog-rule`, `query-all`
- `cl-cc/prolog` globals such as `*prolog-rules*` and peephole rule data
- built-ins for `=`, `!=`, `/=`, `not`, `and`, `or`, `:when`, and cut (`!`)
- raw list/atom fact compatibility
- a minimal DCG layer with `def-dcg-rule`, `phrase`, and `phrase-all`

## Quick start

```lisp
(ql:quickload :cl-prolog)

(in-package #:fx.prolog)

(define-rulebase *family*
  (:facts (parent tom bob) (parent bob alice))
  (:rules
   ((ancestor ?x ?y) (parent ?x ?y))
   ((ancestor ?x ?y) (parent ?x ?z) (ancestor ?z ?y))))

(query-prolog *family* '(ancestor tom ?who))
```

`query-prolog` returns projected variable bindings. `query-all` uses the
global rule registry populated by `def-rule`/`register-prolog-rule` and
returns substituted goals for `cl-cc/prolog` compatibility.

Ground success returns a single empty binding list:

```lisp
(query-prolog *family* '(parent tom bob))
;; => (nil)
```

For lower-level `cl-cc/prolog` compatibility, `unify` returns two values:
`(env success-p)`. On failure, the first value is `:unify-fail`, so existing
single-value callers can use `unify-failed-p`. Ground success still returns
`nil` as the environment and `t` as the second value.

```lisp
(unify '(:const ?r ?v) '(:const :r1 42))
;; => ((?v . 42) (?r . :r1)), T

(unify 1 2)
;; => :UNIFY-FAIL, NIL
```

The `cl-cc/prolog` package nickname also exposes `*prolog-rules*`,
`*peephole-copy-prop-rules*`, `*peephole-arithmetic-rules*`,
`*peephole-control-flow-rules*`, and `*peephole-rules*`.

## Rulebase APIs

```lisp
(let ((kb (make-empty-rule-knowledge-base)))
  (assert-fact! kb (make-fact :predicate 'parent :args '(tom bob)))
  (prove-all kb '(parent tom ?child)))
```

`make-rulebase` also accepts existing raw fact forms for compatibility:

```lisp
(query-prolog (make-rulebase :facts '((tagged ok) :ready))
              '(tagged ?value))
```

## DCG

```lisp
(clear-global-rulebase!)

(def-dcg-rule accept-int
  (terminal :T-INT))

(phrase 'accept-int '((:T-INT . 1) (:T-EOF . nil)))
;; => ((:T-EOF . nil))
```

`terminal` matches both atom tokens and `(kind . value)` token pairs. The
`dcg-token-match-value` builtin can bind token values when needed.

## Nix

```sh
nix develop
nix run .#test
nix flake check
```

## Testing

```lisp
(asdf:test-system :cl-prolog)
```
