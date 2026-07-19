# API Reference

`cl-prolog` exposes one public package: `cl-prolog`. This page lists every
symbol exported by that package. Anything not listed here is internal.

See [Querying](querying.md), [Builtin goals](builtin-goals.md),
[Rule DSL](rule-dsl.md), and [DCG](dcg.md) for narrative explanations and
examples.

## Data

- Clause representation: `clause`, `clause-p`, `clause-head`, `clause-body`,
  `make-clause`
- Rulebase representation: `rulebase`, `rulebase-p`,
  `rulebase-visible-clauses`, `make-rulebase`, `copy-rulebase`,
  `rulebase-extend`, `rulebase-insert-clause!`

A clause with an empty body is a fact. Rulebases are explicit values; the
library has no global rulebase. Prefer `prolog` and `extend-rulebase` for
declarative construction. `rulebase-insert-clause!` and the dynamic database
goals provide intentional mutation with logical-update semantics.

`copy-rulebase` is useful when dynamic updates must not affect a reusable
base. It copies stored terms and mutable registries while allowing immutable
metadata to be shared.

The exported symbol `clause` names the clause type/accessor API and is also
the Lisp representation of the exported `clause/2` builtin goal.

## Unification

- `logic-var-p` — true for `?`-prefixed non-keyword symbols
- `fresh-logic-variable` — return a fresh symbol satisfying `logic-var-p`
- `unify` — `(unify left right &optional environment)` returns
  `(values extended-environment t)` on success and `(values nil nil)` on
  failure; the occurs check is always enabled
- `logic-substitute` — apply an environment to a term while preserving dotted
  structure

Environments are persistent association lists. `unify` extends rather than
mutates an environment, so older environments remain valid choice points.

## Proof Search and Depth

- `*max-prolog-depth*` — default maximum user-rule depth; `nil` means
  unbounded
- `define-foreign-predicate` — register one exact predicate name/arity pair
  using the solver's zero-to-many `emit` continuation protocol

`define-foreign-predicate` is the supported extension surface. Its form is:

```lisp
(define-foreign-predicate (name argument...)
    (rulebase environment depth emit)
  body...)
```

The argument list has fixed arity. Call `emit` once for each solution
environment and do not collect solutions in the predicate. The builtin
registry and its defining machinery are internal implementation details.

## Queries

- `map-prolog-solutions`
- `query-prolog`
- `query-prolog-first`
- `prolog-succeeds-p`
- `solution-binding`

The query functions accept an explicit rulebase and query. The mapping and
list-returning APIs support `:max-depth`, `:environment`, `:project`, and
`:limit`; `prolog-succeeds-p` supports `:max-depth`. See
[Querying](querying.md) for contracts and result shapes.

## Parser, Writer, and Loader

- Readers and parser: `read-prolog-term`, `read-prolog-clause`, `parse-prolog`
- Writer: `write-prolog-term`, `prolog-term-string`
- Source loading: `consult-prolog`, `ensure-prolog-loaded`
- Loader goal symbols: `consult`, `ensure_loaded`, `load_files`

`read-prolog-term` and `read-prolog-clause` accept a string or stream and an
optional operator table. `parse-prolog` parses Prolog source into clause data.
`write-prolog-term` writes to its optional stream, while
`prolog-term-string` returns a string.

`consult-prolog` validates a source before replacing its registered clauses.
`ensure-prolog-loaded` loads a pathname only when that source is not already
registered. The goal symbols correspond to `consult/1`, `ensure_loaded/1`,
and `load_files/1` in parsed or Lisp-shaped queries.

## Rule DSL

- `prolog`
- `define-rulebase`
- `extend-rulebase`
- `def-rule`
- `with-prolog-query`
- `prolog-match`

See [Rule DSL](rule-dsl.md) for macro forms and examples.

## Builtin Goal Symbols

These exported symbols form the public Lisp package surface for builtin goals.
The same names can be written in parsed Prolog syntax with their supported
arities. See [Builtin goals](builtin-goals.md) for behavior-oriented guidance.

- Control and meta-call: `!`, `call`, `call_nth`,
  `call_with_depth_limit`, `once`, `setup_call_cleanup`, `call_cleanup`,
  `forall`, `if-then-else`, `soft-if-then-else`, `catch`, `throw`,
  `unify_with_occurs_check`, `repeat`, `true`, `fail`, `false`, `\+`
- Collection and sorting: `findall`, `bagof`, `setof`, `sort`, `msort`,
  `keysort`
- Dynamic database and reflection: `asserta`, `assert`, `assertz`, `retract`,
  `retractall`, `current_predicate`, `predicate_property`, `abolish`, `clause`
- Unification and term construction: `\=`, `..`, `=..`
- Arithmetic evaluation and comparison: `is`, `=:=`, `=\=`, `<`, `=<`, `>`,
  `>=`
- Finite domains: `in`, `ins`, `#=`, `#\=`, `#<`, `#=<`, `#>`, `#>=`,
  `all_different`, `labeling`, `indomain`
- Type and term tests: `var`, `nonvar`, `atom`, `atomic`, `number`, `integer`,
  `float`, `compound`, `callable`, `ground`, `acyclic_term`, `cyclic_term`
- Standard term order and comparison: `==`, `\==`, `@<`, `@=<`, `@>`, `@>=`,
  `compare`, `unifiable`
- Term inspection and copying: `term_variables`, `functor`, `arg`, `copy_term`,
  `numbervars`

## DCG

- Grammar definition and execution: `def-dcg-rule`, `phrase`, `phrase-all`
- Combinators: `dcg-alt`, `dcg-opt`, `dcg-star`, `dcg-plus`,
  `dcg-error-recovery`
- Token matchers: `dcg-token-match`, `dcg-token-match-value`

DCG tokens are either a bare token-kind symbol or a `(kind . value)` cons.
`dcg-token-match` matches the kind and returns the remaining input;
`dcg-token-match-value` matches both kind and value. See [DCG](dcg.md).

## Conditions

- Depth configuration: `invalid-max-depth-error` and reader
  `invalid-max-depth-error-value`
- Depth exhaustion: `prolog-depth-limit-exceeded` and reader
  `prolog-depth-limit-exceeded-goal`
- Structurally invalid goals: `invalid-goal-error` and reader
  `invalid-goal-error-goal`
- Prolog throws: `prolog-exception` and reader `prolog-exception-term`
- Runtime hierarchy: `prolog-runtime-error`, `prolog-instantiation-error`,
  `prolog-type-error`, `prolog-domain-error`, `prolog-permission-error`,
  `prolog-existence-error`, `prolog-evaluation-error`,
  `prolog-resource-error`
- Process-level halt: `prolog-halt` and reader `prolog-halt-code`
- Arithmetic diagnostics: `arithmetic-evaluation-error` and readers
  `arithmetic-error-expression`, `arithmetic-error-reason`

`prolog-halt` is not a `prolog-exception`, so `catch/3` does not intercept it.
The embedding application decides how to translate the condition into process
termination.

## Script Entry Points

- `nix run .` — run the cl-weave-backed ASDF regression suite on Linux
- `asdf:load-system :cl-prolog/examples` — load runnable examples
