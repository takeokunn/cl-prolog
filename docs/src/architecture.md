# Architecture

## Design Direction

`cl-prolog` is a small relational programming library for Common Lisp. The
codebase optimizes for:

- macro-first authoring: macros own syntax, the runtime only sees data
- CPS proof search: solutions stream through continuations
- explicit separation between data and logic
- immutable rulebase construction as the default style
- a narrow, machine-verified public API with one canonical surface

It is not trying to emulate a full ISO Prolog runtime. It is a focused Lisp
library.

## Module Layout

Sources live under `src/`, in dependency (and load) order:

- `package.lisp` — public package and export boundary
- `operator-table.lisp` and `module-system.lisp` — parser operator state and
  module metadata
- `data-types.lisp` and `data.lisp` — clauses, rulebases, constructors, and
  immutable rulebase operations
- `unification.lisp` — unification, substitution, variable renaming
- `parser.lisp` and `parser-source.lisp` — term parsing and source boundaries
- `term-writer.lisp` and `io-context.lisp` — term rendering and explicit I/O
  context
- `engine.lisp`, `prover-state.lisp`, and `prover.lisp` — CPS proof state,
  dispatch, cut, depth bounds, and clause search
- `builtins/` — control, collection, database, arithmetic, list, atom,
  operator, and split stream/I/O builtins
- `fd-store.lisp` and `builtins/fd.lisp` — finite-domain state and predicates
- `term-order.lisp` and `builtin-term.lisp` — term ordering and ISO-style term
  inspection builtins
- `dcg-runtime.lisp` — DCG combinator builtins
- `query.lisp` — public query API over the engine
- `source-loader.lisp`, `source-directives.lisp`, and
  `source-loader-builtins.lisp` — source loading and directive processing
- `dsl-compiler.lisp` and `dsl.lisp` — `prolog`, `def-rule`, and friends;
  compile `(:when EXPR)` guards to closures
- `dcg.lisp` — `def-dcg-rule` expansion, `phrase`
- `tests/` — split regression suite plus a table-driven expectation DSL

## Data Versus Logic

- data layer: `fact`, `rule`, `rulebase`, constructors, accessors
- logic layer: unification, CPS proof search, builtin solvers, DSL expansion

A caller can build or transform a rulebase as plain data without depending
on the query engine, and the engine treats rulebases as read-only input.

## The CPS Engine

Every prover receives an `EMIT` continuation and calls it once per solution
environment:

```
%prove-goal-sequence (goals rb env depth emit)  ; conjunction
%prove-goal          (goal  rb env depth emit)  ; dispatch
%prove-with-clauses  (goal  rb env depth emit)  ; facts -> rules
%prove-with-rule     (goal rule rb env depth emit)
```

Nothing in the engine accumulates result lists — `query-prolog` is a fold
over `map-prolog-solutions`, which exposes the CPS contract directly.
Builtins and foreign predicates follow the same contract. `define-builtin`
and `define-foreign-predicate` register CPS solvers through generic dispatch;
foreign predicates are keyed by exact name and arity.

### Cut

Cut uses a dynamically scoped tag stored in the proof state:

- each user-defined predicate invocation establishes a fresh `catch` tag
- `!` emits its solution, then `throw`s to the current tag
- the invocation boundary consumes that transfer, pruning remaining clauses
  without escaping into the caller
- opaque meta-calls establish their own boundary, while transparent control
  branches retain the containing proof state's tag

### Guards

`(:when EXPR)` guards are compiled by the DSL macros into
`(:when FUNCTION ?var...)` goals at macroexpansion time. The engine
substitutes solved values and funcalls — there is no `eval` anywhere in the
runtime, so rule data can be treated as inert.

### Search order and termination

- registered builtins and foreign predicates are authoritative; unregistered
  indicators search facts and then rules
- facts and rules keep definition order within their group
- rule and fact variables are freshly renamed per use
- `:max-depth` decrements only per user-rule resolution; `NIL` is unbounded
- `dcg-star` refuses zero-progress repetitions, bounding nullable grammars

## Macro-First Surface

```lisp
(prolog
  ((parent tom bob))
  ((score bob 42))
  ((rich ?x) (score ?x ?n) (:when (> ?n 10))))
```

expands into `make-clause` forms (guards become closures).
Macros keep relational source compact; runtime code only executes
normalized data; tests can assert both syntax-level intent and runtime
behavior.

## Explicit State

There is no process-global rulebase. Build a value with `prolog`, derive a
new value with `extend-rulebase`, and pass that value to every query. Dynamic
database predicates mutate only the explicitly supplied rulebase, keeping
state ownership visible at the call site.

## Verification Layers

1. `nix run .` — cl-weave-backed ASDF regression behavior on Linux
2. `nix flake check` — regression tests, Paredit structural checks, and mdBook
   documentation verification
3. `nix build .#cl-prolog` — release package artifact construction

When architecture changes land, update the narrowest affected layer first.
