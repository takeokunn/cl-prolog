# Querying

```lisp
(query-prolog rb '(ancestor tom ?who))          ; all solutions
(query-prolog rb '(ancestor tom ?who) :limit 2) ; bounded search
(query-prolog-first rb '(ancestor ?x bob))      ; first solution or NIL
(prolog-succeeds-p rb '(ancestor tom eve))      ; boolean, stops at first proof

;; streaming: the function is called as each solution is proven
(map-prolog-solutions
 (lambda (solution) (format t "~&=> ~S~%" solution))
 rb '(ancestor tom ?who))
```

`with-prolog-query` binds variables from the first solution; `prolog-match`
dispatches like `cond` over queries.

## Entry points

- `map-prolog-solutions` — the primitive. Calls a function once per solution
  **as it is proven** (streaming CPS). Keywords: `:max-depth`,
  `:environment`, `:project`, `:limit`.
- `query-prolog` — collect solutions into a list. Same keywords.
- `query-prolog-first` — first solution or `nil` (searches with `:limit 1`).
- `prolog-succeeds-p` — boolean; stops at the first proof.
- `solution-binding` — look one variable up in a solution alist.

## Conventions

- a solution is an alist of query-variable bindings; ground success is `nil`,
  so "one ground proof" is `(nil)` and failure is `()`
- `:project nil` returns raw proof environments instead
- `:max-depth` optionally bounds user-rule resolution (the default is `NIL`,
  meaning unbounded); exhaustion signals `prolog-depth-limit-exceeded`;
  `0` disables rule expansion entirely, facts still match
- `:limit` must be `nil` or a positive integer; any other value signals a
  `type-error`
- the option list is validated: an odd-length list or an unrecognized keyword
  signals a `program-error`, so a mistyped option fails loudly instead of being
  silently ignored
