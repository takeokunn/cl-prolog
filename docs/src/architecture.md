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
this exact dependency order, which groups into layers:

**Foundations** — packages, operator/module/source registries, and the clause
and tabling data models:

1. `package.lisp`
2. `operator-table.lisp`
3. `module-system.lisp`
4. `source-registry.lisp`
5. `data.lisp`
6. `table-variant.lisp`
7. `unification.lisp`

**Text front end** — the lexer/parser split and the term writer:

8. `lexer.lisp`
9. `lexer-operator-lexemes.lisp`
10. `lexer-tokenizer.lisp`
11. `grammar.lisp`
12. `term-writer.lisp`

**Search core** — conditions and registries, the I/O context, the CPS prover,
and the tabling layer:

13. `engine.lisp`
14. `io-context.lisp`
15. `prover.lisp`
16. `tabling.lisp`

**Builtin goal set** — the `define-builtin` machinery and the builtin modules:

17. `builtins/core.lisp`
18. `builtins/control.lisp`
19. `builtins/collection.lisp`
20. `builtins/dynamic.lisp`
21. `builtins/arithmetic.lisp`
22. `builtins/list.lisp`
23. `builtins/text-conversion.lisp`
24. `builtins/atom-ops.lisp`
25. `builtins/atom-number-conversion.lisp`
26. `builtins/operator.lisp`
27. `builtins/io.lisp`
28. `builtins/io-streams.lisp`
29. `builtins/io-code.lisp`
30. `fd-store.lisp`
31. `builtins/fd.lisp`
32. `term-inspect.lisp`
33. `term-compare.lisp`
34. `term-construct.lisp`

**Front ends** — DCG runtime, the public query API, the transactional source
loader, and the authoring macros:

35. `dcg-runtime.lisp`
36. `query.lisp`
37. `source-io.lisp`
38. `source-directives.lisp`
39. `source-rollback.lisp`
40. `source-loader.lisp`
41. `dsl-compiler.lisp`
42. `dsl.lisp`
43. `dcg.lisp`

The important boundaries are:

- `data.lisp` owns clauses, the logical-update rulebase, the predicate index,
  and mutable registries; `table-variant.lisp` owns the tabling data model
- `lexer.lisp` tokenizes source text and enforces the parser resource limits,
  `lexer-operator-lexemes.lisp` holds the standard/symbolic operator lexeme
  tables the tokenizer matches against, and `lexer-tokenizer.lisp` is the
  tokenizer itself; `grammar.lisp` runs the precedence-climbing parser on top
  and exposes the public reader API
- `engine.lisp` owns conditions plus the builtin and foreign-predicate
  registries and the CPS `emit` protocol
- `prover.lisp` owns normalization, proof state, dispatch, clause resolution,
  cut barriers, and depth accounting; `tabling.lisp` layers memoized resolution
  and left-recursion detection on top
- `query.lisp` turns the continuation protocol into the public mapping and
  result APIs
- the builtin set is split by concern — control, collection, dynamic database,
  arithmetic, lists, atom/text conversion, operators, stream I/O, finite
  domains, and term inspection/comparison/construction
- the `source-*.lisp` files, `dsl*.lisp`, and `dcg*.lisp` are separate front
  ends that produce or consume the same clause and query representation

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

## Transactional Source Loading

Loading Prolog source is all-or-nothing. `consult-prolog` and
`ensure-prolog-loaded` run inside a loading transaction that copies the live
rulebase, applies every clause and directive to the detached copy, runs any
`:- initialization` goals, and only publishes the copy back on success. Any
parse error, failed directive, or resource-limit violation aborts before the
live rulebase is touched.

The pipeline is split across five files:

- `source-registry.lisp` records one entry per canonical source pathname,
  tracking its load state and the effects it applied
- `source-io.lisp` resolves pathnames and streams and translates parser and
  I/O failures into ISO source-loading errors (including the catchable
  `resource_error/1` form of a parser resource-limit breach)
- `source-directives.lisp` evaluates one directive or clause at a time —
  `op`, `dynamic`, `table`, `use_module`, `include`, `initialization`,
  `consult`, and `load_files` — recording each operator, predicate-property,
  and table declaration for later rollback
- `source-rollback.lisp` undoes a previously loaded unit on reload: it removes
  the unit's clauses and replays or restores the operator, predicate-property,
  and table effects it recorded
- `source-loader.lisp` orchestrates the transaction and exposes the public
  `consult`, `load_files`, and `ensure_loaded` surface

Load-state tracking breaks reload cycles and honors the `if-loaded` policy that
distinguishes `consult` (always reload) from `ensure_loaded` (load once).

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
