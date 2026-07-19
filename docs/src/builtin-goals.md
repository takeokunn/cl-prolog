# Builtin Goals

The list below is a representative, task-oriented overview, not an exhaustive
list. The authoritative list of builtin symbols exported to Lisp callers is
the [API Reference](api-reference.md#builtin-goal-symbols). Parsed Prolog
source also uses implemented goal names that need not be exported as Common
Lisp package symbols.

- **Unification:** `(= a b)`, `(\= a b)`, and
  `(unifiable a b ?unifier)` unify two terms, require that they cannot unify,
  or return a unifier without binding either input.
- **Control:** `!`, `(not g)`, `(and g...)`, and `(or g...)` provide cut,
  negation as failure, conjunction, and disjunction.
- **Meta-call:** `(call g)`, `(once g)`, `(ignore g)`, `(not g)`,
  `(forall c a)`, and `(repeat)` invoke goals, control their solutions, or
  generate repeated proofs.
- **Collection:** `(findall t g ?bag)`, `(bagof t g ?bag)`, and
  `(setof t g ?set)` collect templates. `bagof` groups free variables, while
  `setof` also removes duplicates and sorts.
- **Dynamic database:** `(asserta c)`, `(assertz c)`, `(retract c)`,
  `(abolish (p / n))`, and `(clause h b)` inspect or mutate clauses in the
  rulebase passed to the query.
- **Arithmetic:** `(is ?x expr)`, `(=:= a b)`, `(=\= a b)`, `(< a b)`,
  `(=< a b)`, `(> a b)`, and `(>= a b)` evaluate arithmetic expressions and
  compare their numeric values.
- **Lists:** `(member ?x list)`, `(append ?a ?b ?c)`, `(reverse ?a ?b)`, and
  `(length ?l ?n)` define relations over proper and partially instantiated
  lists.
- **Finite domains:** `(#= a b)`, `(#\= a b)`, `(#< a b)`, `(#=< a b)`,
  `(#> a b)`, `(#>= a b)`, `(in x range)`, and `(indomain x)` constrain
  integers and enumerate finite-domain solutions.
- **Lisp guard:** `(:when fn ?x...)` calls a Lisp predicate with solved values.

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

## Relational list length

`length/2` can check a proper list, bind its length, complete a partial list to
a requested non-negative length, or generate lists of increasing length when
both arguments are variables. A non-integer length raises `type_error(integer,
...)`; a negative length raises `domain_error(not_less_than_zero, ...)`.
Cyclic lists fail rather than making the solver loop.

## Meta-call validation

Goals executed by `call/1`, `once/1`, `ignore/1`, negation, `forall/2`,
conditionals, `catch/3`, and cleanup predicates must be callable after applying
the current bindings. Invalid goals raise the corresponding ISO instantiation
or callable type error. Validation happens when that goal is actually reached,
so an unreachable branch is not inspected eagerly.

## Finite-domain equality

`#=/2` binds an unconstrained variable when the other operand is an integer;
the binding is visible to ordinary unification and later goals. Comparing a
variable with itself succeeds for `#=`, `#=<`, and `#>=`, and fails for `#\=`,
`#<`, and `#>`.

## Term and stream I/O

`read_term/3` supports `variable_names/1` and `singletons/1`. The latter
returns `Name=Variable` entries for named variables that occur exactly once;
the anonymous variable `_` is omitted.

For input streams, `stream_property/2` reports `end_of_stream(not)`,
`end_of_stream(at)`, or `end_of_stream(past)`. Peeking at EOF establishes
`at`; an attempted consuming read advances it to `past`. Repositioning a
stream resets the state to `not`.

## Structurally unusable goals

A non-function `:when` guard, or a goal that is not a symbol or symbol-headed
list, signals `invalid-goal-error` (`invalid-goal-error-goal` returns the
offending goal). Calling a known name at an unsupported arity denotes a
different, undefined procedure and signals the ISO `existence_error` instead.
`halt/0` and `halt/1` raise the `prolog-halt` condition (readers:
`prolog-halt-code`); it is deliberately not a `prolog-exception`, so
`catch/3` never intercepts it and the embedding application decides how to
exit.

Prolog numbers are integers and floating-point values. Common Lisp ratios and
complex values are not accepted by numeric or atomic term predicates and do
not receive numeric term ordering.

## Extending the builtin set

```lisp
(define-foreign-predicate (twice input output)
    (rulebase environment depth emit)
  (declare (ignore rulebase depth))
  (let ((value (logic-substitute input environment)))
    (when (numberp value)
      (multiple-value-bind (extended ok) (unify output (* 2 value) environment)
        (when ok (funcall emit extended))))))
```

- `define-foreign-predicate` is the public extension API and registers one
  exact predicate name/arity pair
- the predicate argument list has fixed arity; lambda-list keywords such as
  `&rest` are not supported
- `rulebase`, `environment`, and `depth` expose the current solver context
- `emit` must be called once per solution with its resulting environment;
  call it zero times to fail and do not collect result lists

The internal builtin-definition machinery is not exported. See
[Rule DSL](rule-dsl.md) for additional foreign-predicate examples.
