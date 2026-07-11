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
- `data.lisp` — facts, rules, rulebase structs (data only, no logic)
- `unification.lisp` — unification, substitution, variable renaming
- `engine.lisp` — CPS provers, cut, depth bound, the builtin registry,
  the `predicate-true-p` hook
- `builtin-term.lisp` — ISO-style term inspection and ordering builtins
- `builtins/` — control, collection, database, arithmetic, and list builtins
- `dcg-runtime.lisp` — DCG combinator builtins
- `query.lisp` — public query API over the engine
- `dsl.lisp` — `prolog`, `def-rule`, and friends; compiles `(:when EXPR)`
  guards to closures
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
%prove-with-clauses  (goal  rb env depth emit)  ; hook -> facts -> rules
%prove-with-rule     (goal rule rb env depth emit)
```

Nothing in the engine accumulates result lists — `query-prolog` is a fold
over `map-prolog-solutions`, which exposes the CPS contract directly.
Builtins follow the same contract; `define-builtin` registers a solver
function in a hash table the dispatcher consults, which is also the public
extension point.

### Cut

Cut is implemented with the condition system, the idiomatic Lisp tool for
dynamically scoped control flow:

- `!` emits its solution, then signals the internal `%cut` condition
- conjunction barriers propagate the signal through earlier choice points
  in the same invocation
- each user-defined predicate invocation owns the boundary that consumes the
  signal, so a rule-body cut prunes remaining clauses without escaping into
  its caller
- opaque meta-calls consume their nested cut, while transparent control
  branches explicitly re-signal it into the containing invocation

### Guards

`(:when EXPR)` guards are compiled by the DSL macros into
`(:when FUNCTION ?var...)` goals at macroexpansion time. The engine
substitutes solved values and funcalls — there is no `eval` anywhere in the
runtime, so rule data can be treated as inert.

### Search order and termination

- for each goal: `predicate-true-p` hook, then facts, then rules
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

1. `scripts/verify-public-contract.lisp` — exact public surface and shipped files
2. `sbcl --script tests.lisp` — regression behavior
3. `sbcl --script scripts/run-tests-noasdf.lisp` — same core suite without script-contract tests
4. `scripts/benchmark.lisp` — proof-search and DCG performance smoke
5. `scripts/release-audit.lisp` — release orchestration
6. `nix flake check` — packaging and clean-source verification

When architecture changes land, update the narrowest affected layer first.
