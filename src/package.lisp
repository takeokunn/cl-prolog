(defpackage #:cl-prolog.user-atoms
  (:use)
  (:documentation
   "Interned Prolog atoms whose names would otherwise resolve to inherited Common Lisp symbols."))

(defpackage #:cl-prolog
  (:use #:cl)
  (:shadow #:! #:catch #:throw)
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
   #:rulebase-visible-clauses
   #:make-rulebase
   #:rulebase-insert-clause!
   ;; unification
   #:logic-var-p
   #:fresh-logic-variable
   #:unify
   #:logic-substitute
   ;; engine
   #:*max-prolog-depth*
   #:invalid-max-depth-error
   #:invalid-max-depth-error-value
   #:prolog-depth-limit-exceeded
   #:prolog-depth-limit-exceeded-goal
   #:invalid-goal-error
   #:invalid-goal-error-goal
   #:prolog-exception
   #:prolog-exception-term
   #:prolog-runtime-error
   #:prolog-instantiation-error
   #:prolog-type-error
   #:prolog-domain-error
   #:prolog-permission-error
   #:prolog-existence-error
   #:prolog-evaluation-error
   #:prolog-resource-error
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
   ;; text parser
   #:read-prolog-term
   #:read-prolog-clause
   #:parse-prolog
   #:consult-prolog
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
   #:setup-call-cleanup
   #:call-cleanup
   #:forall
   #:if-then-else
   #:soft-if-then-else
   #:catch
   #:throw
   #:repeat
   #:findall
   #:bagof
   #:setof
   #:true
   #:fail
   #:false
   #:|\+|
   #:asserta
   #:assertz
   #:retract
   #:retractall
   #:current-predicate
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
   #:atomic
   #:number
   #:integer
   #:float
   #:==
   #:|\==|
   #:@<
   #:@=<
   #:@>
   #:@>=
   #:compare
   #:term-variables
   #:compound
   #:callable
   #:ground
   #:functor
   #:arg
   #:copy-term
   #:numbervars
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
