# API Reference

`cl-prolog` exposes one public package: `cl-prolog`.

The surface is intentionally small. Data construction, unification, proof
search, the rule DSL, and DCG support are public; everything else is
internal. The exact export set is machine-checked against
`contracts/public-contract.sexp`.

## Data

- `clause`, `clause-p`, `clause-head`, `clause-body`, `make-clause`
- `rulebase`, `rulebase-p`, `rulebase-clauses`, `make-rulebase`
- `rulebase-insert-clause!`, `rulebase-remove-clause!`

A clause with an empty body is a fact. Rulebases are always explicit values;
the library has no global rulebase. Prefer `prolog` and `extend-rulebase` for
declarative construction, and use the mutators only for dynamic database
semantics such as `asserta`, `assertz`, `retract`, and `abolish`.

## Unification

- `logic-var-p` — true for `?`-prefixed non-keyword symbols
- `fresh-logic-variable` — a gensym that satisfies `logic-var-p`
- `unify` — `(unify left right &optional env)` returns
  `(values extended-env t)` on success and `(values nil nil)` on failure;
  the occurs check is always on
- `logic-substitute` — apply an environment to a term, preserving dotted
  structure

Environments are persistent association lists: `unify` extends, never
mutates, so callers can keep older environments for backtracking.

## Queries

- `map-prolog-solutions` — the primitive. Calls a function once per solution
  **as it is proven** (streaming CPS). Keywords: `:max-depth`,
  `:environment`, `:project`, `:limit`.
- `query-prolog` — collect solutions into a list. Same keywords.
- `query-prolog-first` — first solution or `nil` (searches with `:limit 1`).
- `prolog-succeeds-p` — boolean; stops at the first proof.
- `solution-binding` — look one variable up in a solution alist.

Conventions:

- a solution is an alist of query-variable bindings; ground success is `nil`,
  so "one ground proof" is `(nil)` and failure is `()`
- `:project nil` returns raw proof environments instead
- `:max-depth` optionally bounds user-rule resolution (the default is `NIL`,
  meaning unbounded); exhaustion signals `prolog-depth-limit-exceeded`;
  `0` disables rule expansion entirely, facts still match
- `:limit` must be `nil` or a positive integer

## Rule DSL

- `prolog` — build a rulebase from clauses
- `define-rulebase` — `defparameter` + `prolog`
- `extend-rulebase` — functional extension; the new clauses shadow the base
- `def-rule` — define a reusable clause-producing form
- `with-prolog-query` — bind variables from the first solution
- `prolog-match` — `cond` over queries

Clause shape: `((pred args...))` is a fact, `(head goal...)` is a rule.

`(:when EXPR)` goals inside `prolog` / `def-rule` / DCG `brace` are compiled
at macroexpansion time into `(:when FUNCTION ?var...)` — a closure over the
expression's logic variables. The engine substitutes each variable's solved
value and calls the function; it never evaluates user expressions.

## Builtin Goals

- `(= left right)` — unify
- `(/= left right)` — fail when the terms unify
- `!` — cut: prunes the running clause's remaining choice points and the
  predicate's remaining rule clauses
- `(not goal)` — negation as failure
- `(and goal...)` — conjunction; `(and)` is true
- `(or goal-or-conjunction...)` — disjunction; `(or)` fails
- `(:when function ?var...)` — succeed when FUNCTION returns true for the
  solved values (see Rule DSL for the expression form)
- `(member ?x list)` — enumerate elements
- `(append ?left ?right ?whole)` — forward append or split enumeration
- `(reverse ?forward ?backward)` — works in either direction
- `(length ?list ?n)` — measure, or generate a fresh-variable list of length
  `?n`

Malformed goals — wrong arity, non-function `:when` guard, a goal that is
not a symbol or symbol-headed list — signal `invalid-goal-error`
(`invalid-goal-error-goal` returns the offending goal).

### Extending the builtin set

```lisp
(define-builtin (twice input output) (rulebase environment depth emit)
  (let ((value (logic-substitute input environment)))
    (when (numberp value)
      (multiple-value-bind (extended ok) (unify output (* 2 value) environment)
        (when ok (funcall emit extended))))))
```

- the head lambda list supports required parameters and `&rest`; arity is
  enforced before the body runs
- the head name may be a list of symbols sharing one solver
- EMIT must be called once per solution with the extended environment;
  builtins must not collect result lists
- `predicate-true-p` is the simpler hook: specialize it with
  `(eql 'name)` for a Lisp-side truth check that introduces no bindings

## Proof-Search Semantics

- goals are resolved against `predicate-true-p`, then facts, then rules —
  facts always come before rules, in definition order within each group
- rule variables (and variables inside facts) are freshly renamed per use
- an explicit depth bound decrements per user-rule resolution, so left recursion
  terminates with the solutions found within the bound

## DCG

- `def-dcg-rule` — compile a grammar body into a rule with two stream
  arguments; body elements: `(terminal KIND...)`, `(brace EXPR)`,
  non-terminal calls, and combinator forms
- `phrase` — `(values remainder matched-p)` for the first parse
- `phrase-all` — remainders of every parse
- combinators (usable as goals): `dcg-alt`, `dcg-opt`, `dcg-star`,
  `dcg-plus`, `dcg-error-recovery`
- token matchers: `(dcg-token-match kind input rest)`,
  `(dcg-token-match-value kind value input rest)`

Tokens are bare kind symbols or `(kind . value)` conses.
`dcg-error-recovery` skips ahead to the next token whose kind is in
`*dcg-sync-tokens*` (internal parameter: `:t-rparen`, `:t-semi`, `:t-eof`).
`dcg-star` refuses to repeat a rule that consumed no input, so nullable
rules terminate.

## Conditions

- `invalid-goal-error` (`error`) — structurally unusable goal;
  readers: `invalid-goal-error-goal`
- errors signalled by `:when` guard functions propagate to the caller

## Script Entry Points

- `sbcl --script tests.lisp` — regression suite
- `sbcl --script scripts/run-tests-noasdf.lisp` — fast core suite without script-contract tests
- `sbcl --script examples/quick-start.lisp` — runnable example script
- `sbcl --script scripts/benchmark.lisp` — benchmark scenario runner

Anything not exported from `cl-prolog` should be treated as internal.
