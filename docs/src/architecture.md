# Architecture

## Design Direction

`cl-prolog` is a relational programming library embedded in Common Lisp. Its
main design choices are:

- macro-first authoring: macros own syntax and produce runtime data
- continuation-passing proof search: solutions stream through continuations
- explicit rulebase and query state rather than a process-global database
- one ordered clause representation for facts and rules
- a narrow exported package surface with internal solver machinery

It implements a focused Prolog runtime rather than attempting to mirror every
facility of a standalone ISO Prolog system.

## ASDF Load Order

`cl-prolog.asd` is serial. The production system loads these components in
this exact dependency order:

1. `package.lisp`
2. `operator-table.lisp`
3. `module-system.lisp`
4. `data.lisp`
5. `unification.lisp`
6. `parser.lisp`
7. `term-writer.lisp`
8. `engine.lisp`
9. `io-context.lisp`
10. `prover.lisp`
11. `builtins/core.lisp`
12. `builtins/control.lisp`
13. `builtins/collection.lisp`
14. `builtins/dynamic.lisp`
15. `builtins/arithmetic.lisp`
16. `builtins/list.lisp`
17. `builtins/atom.lisp`
18. `builtins/operator.lisp`
19. `builtins/io.lisp`
20. `builtins/io-streams.lisp`
21. `builtins/io-code.lisp`
22. `fd-store.lisp`
23. `builtins/fd.lisp`
24. `builtin-term.lisp`
25. `dcg-runtime.lisp`
26. `query.lisp`
27. `source-loader.lisp`
28. `dsl-compiler.lisp`
29. `dsl.lisp`
30. `dcg.lisp`

The important boundaries are:

- `data.lisp` owns clauses, the logical-update rulebase, indexes, and mutable
  registries
- `engine.lisp` owns conditions plus builtin and foreign-predicate registries
- `prover.lisp` owns normalization, proof state, dispatch, clause resolution,
  cut barriers, depth accounting, and tabling
- `query.lisp` turns the continuation protocol into the public mapping and
  result APIs
- `source-loader.lisp`, `dsl*.lisp`, and `dcg*.lisp` are separate front ends
  that produce or consume the same clause and query representation

## Unified Clause Store

Facts and rules use the same `clause` structure. A fact is a clause whose body
is empty. The rulebase stores clauses in one definition-ordered sequence and a
predicate index; it does not search a separate fact collection before a rule
collection.

Stored entries carry their module, source identity, and born/died revisions.
A predicate call takes the visible entries for its logical-update snapshot and
tries them in definition order. Each selected clause is freshened before
unification, so variables do not leak between uses.

## Proof-State Prover

The prover streams solutions in continuation-passing style. Its internal
`proof-state` carries:

- the explicit rulebase
- the current persistent binding environment
- remaining user-rule depth
- the active module
- the table session
- the current cut tag

State transitions construct updated proof states rather than replacing the
rulebase with hidden global state. Goal dispatch first recognizes registered
builtin or foreign solvers; otherwise it resolves the goal against the visible
user clauses. Foreign predicates are keyed by exact name and arity and use the
same zero-to-many `emit` continuation contract as internal solvers.

`map-prolog-solutions` exposes this streaming model. Convenience query APIs
fold or stop that stream instead of requiring the prover to accumulate all
answers.

### Cut

Cut uses dynamically scoped `catch`/`throw` tags:

- each user-predicate invocation establishes a fresh cut barrier
- `!` emits the current state and throws to that invocation's tag
- the throw abandons remaining alternatives for the invocation
- opaque meta-calls establish their own barrier
- transparent control constructs deliberately reuse the caller's barrier

This keeps a rule-body cut local to the predicate invocation while preserving
the intended transparency of control constructs.

### Depth and tabling

Depth decreases when proof enters a user rule, not for every builtin or
unification step. `nil` means unbounded search. Declared tabled predicates and
detected left recursion can use a per-query table session; depth-limited or
active finite-domain searches bypass that tabling path where required.

### Guards

`(:when expression)` guards are compiled by the DSL macros into
`(:when function variable...)` goals. At runtime the solver substitutes bound
values and calls the function. Relational rule data therefore does not require
runtime `eval`.

## Explicit Dynamic Mutation

There is no process-global rulebase. `prolog` constructs one, `extend-rulebase`
derives another, and every query receives its rulebase explicitly.

The default authoring style is immutable, but mutation is a supported and
visible operation:

- `asserta/1`, `assert/1`, and `assertz/1` insert into the supplied rulebase
- `retract/1`, `retractall/1`, and `abolish/1` retire matching entries
- `consult-prolog` transactionally replaces the clauses registered for a
  source after validation
- `rulebase-insert-clause!` exposes insertion to Lisp callers

Born/died revisions preserve logical-update behavior: a running predicate
invocation continues over its snapshot even when a dynamic goal changes the
database. Later invocations observe the newer revision. Callers that require
isolation can use `copy-rulebase` before mutation.

## Macro-First Surface

```lisp
(prolog
  ((parent tom bob))
  ((score bob 42))
  ((rich ?x) (score ?x ?n) (:when (> ?n 10))))
```

The DSL expands into clause construction, including precompiled guard
closures. Parsed Prolog source, Lisp DSL forms, dynamic goals, and DCGs all
converge on the same runtime terms and rulebase.

## Verification Layers

1. `nix run .` — run the cl-weave-backed ASDF regression behavior on Linux
2. `nix flake check` — verify packaging and clean-source behavior

When architecture changes, update the narrowest affected verification layer
first.
