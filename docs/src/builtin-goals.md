# Builtin Goals

| Category | Goals | Meaning |
|---|---|---|
| Unification | `(= a b)`, `(\= a b)`, `(unifiable a b ?unifier)` | unify two terms, require that they cannot unify, or return a unifier without binding either input |
| Control | `!`, `(not g)`, `(and g...)`, `(or g...)` | cut, negation as failure, conjunction, and disjunction |
| Meta-call | `(call g)`, `(once g)`, `(repeat)` | invoke a goal, keep its first proof, or generate repeated proofs |
| Collection | `(findall t g ?bag)`, `(bagof t g ?bag)`, `(setof t g ?set)` | collect templates; `bagof` groups free variables and `setof` also removes duplicates and sorts |
| Dynamic database | `(asserta c)`, `(assertz c)`, `(retract c)`, `(abolish (p / n))`, `(clause h b)` | inspect or mutate clauses in the rulebase passed to the query |
| Arithmetic | `(is ?x expr)`, `(=:= a b)`, `(=\= a b)`, `(< a b)`, `(=< a b)`, `(> a b)`, `(>= a b)` | evaluate arithmetic expressions and compare their numeric values |
| Lists | `(member ?x list)`, `(append ?a ?b ?c)`, `(reverse ?a ?b)`, `(length ?l ?n)` | list relations in their supported proper-list modes |
| Lisp guard | `(:when fn ?x...)` | call a Lisp predicate with solved values |

Arithmetic expressions use prefix Lisp-shaped terms. Binary operators are `+`,
`-`, `*`, `/`, `//`, `div`, `rem`, `mod`, `min`, `max`, `**`, and `^`. Unary
operators are `+`, `-`, `abs`, `sign`, `truncate`, `round`, `ceiling`, `floor`,
and `sqrt`.

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

## Structurally unusable goals

A non-function `:when` guard, or a goal that is not a symbol or symbol-headed
list, signals `invalid-goal-error` (`invalid-goal-error-goal` returns the
offending goal). Calling a known name at an unsupported arity denotes a
different, undefined procedure and signals the ISO `existence_error` instead.
`halt/0` and `halt/1` raise the `prolog-halt` condition (readers:
`prolog-halt-code`); it is deliberately not a `prolog-exception`, so
`catch/3` never intercepts it and the embedding application decides how to
exit.

## Extending the builtin set

```lisp
(define-builtin (twice input output) (rulebase environment depth emit)
  (let ((value (logic-substitute input environment)))
    (when (numberp value)
      (multiple-value-bind (extended ok) (unify output (* 2 value) environment)
        (when ok (funcall emit extended))))))
```

- the head lambda list supports required parameters and `&rest`; solver
  dispatch is keyed on the predicate indicator, so the body only ever sees
  goals of a matching arity
- the head name may be a list of symbols sharing one solver
- EMIT must be called once per solution with the extended environment;
  builtins must not collect result lists
- `define-foreign-predicate` registers one exact name/arity pair; its body
  uses the same zero-to-many `EMIT` CPS protocol — see [Rule DSL](rule-dsl.md)
