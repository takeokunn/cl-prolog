# API Reference

`cl-prolog` exposes one public package: `cl-prolog`.

The surface is intentionally small. Data construction, unification, proof
search, the rule DSL, and DCG support are public; everything else is
internal. This page is the complete symbol index — see
[Querying](querying.md), [Builtin goals](builtin-goals.md),
[Rule DSL](rule-dsl.md), and [DCG](dcg.md) for narrative treatment with
examples.

## Data

- `clause`, `clause-p`, `clause-head`, `clause-body`, `make-clause`
- `rulebase`, `rulebase-p`, `rulebase-visible-clauses`, `make-rulebase`
- `rulebase-insert-clause!`

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

See [Querying](querying.md) for the full contract. Entry points:
`map-prolog-solutions`, `query-prolog`, `query-prolog-first`,
`prolog-succeeds-p`, `solution-binding`.

## Rule DSL

See [Rule DSL](rule-dsl.md). Forms: `prolog`, `define-rulebase`,
`extend-rulebase`, `def-rule`, `with-prolog-query`, `prolog-match`,
`define-builtin`, `define-foreign-predicate`.

## Builtin Goals

See [Builtin goals](builtin-goals.md) for the full table and the extension
protocol.

## DCG

See [DCG](dcg.md). Forms: `def-dcg-rule`, `phrase`, `phrase-all`, and the
combinators `dcg-alt`, `dcg-opt`, `dcg-star`, `dcg-plus`,
`dcg-error-recovery`.

## Conditions

- `invalid-goal-error` (`error`) — structurally unusable goal;
  readers: `invalid-goal-error-goal`
- `prolog-halt` (`serious-condition`) — raised by `halt/0,1`; readers:
  `prolog-halt-code`
- errors signalled by `:when` guard functions propagate to the caller

## Script Entry Points

- `nix run .` — cl-weave-backed ASDF regression suite
- `asdf:load-system :cl-prolog/examples` — runnable examples

Anything not exported from `cl-prolog` should be treated as internal.
