# DCG

```lisp
(def-dcg-rule noun (terminal :noun))
(def-dcg-rule verb (terminal :verb))

(def-dcg-rule sentence
  (dcg-star noun)
  (verb)
  (brace (= 1 1)))          ; Lisp guard, like (:when ...)

(phrase 'sentence '(:noun :noun :verb))
;; => NIL, T   (remainder, matched-p)
```

Combinators: `dcg-alt`, `dcg-opt`, `dcg-star`, `dcg-plus`,
`dcg-error-recovery`, plus token matchers `dcg-token-match` and
`dcg-token-match-value`.

## Reference

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
