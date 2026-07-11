(defpackage #:cl-prolog
  (:use #:cl)
  (:shadow #:!)
  (:documentation
   "A small, dependency-free Prolog engine.

Rulebases are plain data (see PROLOG, DEFINE-RULEBASE), proof search is
continuation-passing (see MAP-PROLOG-SOLUTIONS), and the builtin goal set
is extensible (see DEFINE-BUILTIN).")
  (:export
   ;; data
   #:clause
   #:clause-p
   #:clause-head
   #:clause-body
   #:make-clause
   #:rulebase
   #:rulebase-p
   #:rulebase-clauses
   #:make-rulebase
   #:rulebase-insert-clause!
   #:rulebase-remove-clause!
   ;; unification
   #:logic-var-p
   #:fresh-logic-variable
   #:unify
   #:logic-substitute
   ;; engine
   #:*max-prolog-depth*
   #:invalid-goal-error
   #:invalid-goal-error-goal
   #:arithmetic-evaluation-error
   #:arithmetic-error-expression
   #:arithmetic-error-reason
   #:predicate-true-p
   ;; queries
   #:map-prolog-solutions
   #:query-prolog
   #:query-prolog-first
   #:prolog-succeeds-p
   #:solution-binding
   ;; rule DSL
   #:prolog
   #:define-rulebase
   #:extend-rulebase
   #:def-rule
   #:with-prolog-query
   #:prolog-match
   ;; builtin goal names
   #:!
   #:call
   #:once
   #:repeat
   #:findall
   #:bagof
   #:setof
   #:true
   #:asserta
   #:assertz
   #:retract
   #:abolish
   #:clause
   #:!=
   #:/=
   #:is
   #:|=:=|
   #:|=\\=|
   #:<
   #:=<
   #:>
   #:>=
   #:var
   #:nonvar
   #:atom
   #:number
   #:compound
   #:ground
   #:functor
   #:arg
   #:copy-term
   #:|=..|
   ;; DCG
   #:def-dcg-rule
   #:phrase
   #:phrase-all
   #:dcg-alt
   #:dcg-opt
   #:dcg-star
   #:dcg-plus
   #:dcg-error-recovery
   #:dcg-token-match
   #:dcg-token-match-value))
