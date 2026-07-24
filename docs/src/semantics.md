# Semantics

- **Clause order**: clauses are tried in definition order. Facts and rules
  share one ordered sequence per predicate; a fact (a clause with an empty
  body) is not tried ahead of a rule defined before it.
- **Cut** prunes the running clause's remaining choice points and the
  predicate's remaining rule clauses.
- **Optional depth bound**: rule resolution is unbounded by default. Set
  `:max-depth` to a non-negative integer to bound user-rule resolution;
  exhaustion signals `prolog-depth-limit-exceeded` rather than masquerading
  as logical failure.
- **Occurs check** is always on; unification never introduces cyclic terms.
  Host-provided cyclic cons structures are nevertheless handled safely:
  unification compares them coinductively, substitution preserves cycles and
  sharing, and variable collection terminates.

## Modules

Unqualified calls first resolve predicates in the current module and then its
imports. `current_predicate/1` follows the same visible-predicate view, including
imported predicates. In a qualified call `Module:Goal`, `Module` is resolved
through the current bindings and must become an atom naming an existing module;
violations raise catchable ISO instantiation, type, or existence errors.

## Proof-search semantics

- registered builtins and foreign predicates are authoritative for their
  predicate indicator; otherwise the predicate's clauses are resolved in
  definition order, with facts and rules interleaved as written
- rule variables (and variables inside facts) are freshly renamed per use
- an explicit depth bound decrements per user-rule resolution, so left
  recursion terminates with the solutions found within the bound

## Tabling

A predicate declared with a `:- table name/arity` directive resolves through a
per-query memo table instead of plain clause resolution. The engine also
detects left recursion automatically and routes it through the same table so
that a left-recursive definition terminates with the answers reachable within
the query. Answers are keyed up to variance, so calls that differ only in
variable naming share memoized results. The table lives for one public query
and is discarded afterward; depth-limited searches and active finite-domain
constraints bypass the table where memoization would be unsound. Tabling has no
exported Common Lisp symbols — it is engaged only through the source directive.

## Parser resource limits

Reading Prolog *text* is bounded by configurable resource limits (source
length, nesting depth, token count, lexeme sizes, and interned-symbol count).
Exceeding one signals `prolog-parser-resource-error` from the direct reader
APIs, or a catchable ISO `resource_error/1` term when the limit is hit while a
`consult`/`load_files` goal runs. See
[API reference](api-reference.md#parser-resource-limits) for the full list of
specials and their defaults.

See [Architecture](architecture.md) for how cut and guards are implemented in
the CPS engine.
