# Semantics

- **Clause order**: facts are always tried before rules; within each group,
  definition order is preserved.
- **Cut** prunes the running clause's remaining choice points and the
  predicate's remaining rule clauses.
- **Optional depth bound**: rule resolution is unbounded by default. Set
  `:max-depth` to a non-negative integer to bound user-rule resolution;
  exhaustion signals `prolog-depth-limit-exceeded` rather than masquerading
  as logical failure.
- **Occurs check** is always on; unification never builds cyclic terms.

## Proof-search semantics

- registered builtins and foreign predicates are authoritative for their
  predicate indicator; otherwise facts precede rules in definition order
- rule variables (and variables inside facts) are freshly renamed per use
- an explicit depth bound decrements per user-rule resolution, so left
  recursion terminates with the solutions found within the bound

See [Architecture](architecture.md) for how cut and guards are implemented in
the CPS engine.
