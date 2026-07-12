# Rule DSL

For explicit rulebase composition, `define-rulebase` creates a named rulebase
and `extend-rulebase` returns a new rulebase with additional clauses, leaving
the base object unchanged:

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

## Available forms

- `prolog` — build a rulebase from clauses
- `define-rulebase` — `defparameter` + `prolog`
- `extend-rulebase` — functional extension; the new clauses shadow the base
- `def-rule` — define a reusable clause-producing form
- `with-prolog-query` — bind variables from the first solution
- `prolog-match` — `cond` over queries

Clause shape: `((pred args...))` is a fact, `(head goal...)` is a rule.

## Foreign predicates

Extend the goal set with `define-foreign-predicate`. Foreign predicates use
the engine's CPS solution protocol and dispatch by exact name and arity:

```lisp
(define-foreign-predicate (choose output) (rulebase environment depth emit)
  (declare (ignore rulebase depth))
  (dolist (value '(left right))
    (multiple-value-bind (extended ok) (unify output value environment)
      (when ok (funcall emit extended)))))
```
