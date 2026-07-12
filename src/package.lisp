(defpackage #:cl-prolog.user-atoms
  (:use)
  (:documentation
   "Interned Prolog atoms whose names would otherwise resolve to inherited Common Lisp symbols."))

(defpackage #:cl-prolog
  (:use #:cl)
  (:shadow #:! #:assert #:catch #:throw)
  (:documentation
   "A small, dependency-free Prolog engine.

Rulebases are plain data (see PROLOG, DEFINE-RULEBASE), proof search is
continuation-passing (see MAP-PROLOG-SOLUTIONS), and the builtin goal set
is extensible (see DEFINE-FOREIGN-PREDICATE).")
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
   #:copy-rulebase
   #:rulebase-extend
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
   #:prolog-halt
   #:prolog-halt-code
   #:arithmetic-evaluation-error
   #:arithmetic-error-expression
   #:arithmetic-error-reason
   #:define-foreign-predicate
   ;; queries
   #:map-prolog-solutions
   #:query-prolog
   #:query-prolog-first
   #:prolog-succeeds-p
   #:solution-binding
   ;; text parser
   #:read-prolog-term
   #:read-prolog-clause
   #:write-prolog-term
   #:prolog-term-string
   #:parse-prolog
   #:consult-prolog
   #:ensure-prolog-loaded
   #:consult
   #:ensure_loaded
   #:load_files
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
   #:call_nth
   #:call_with_depth_limit
   #:once
   #:setup_call_cleanup
   #:call_cleanup
   #:forall
   #:if-then-else
   #:soft-if-then-else
   #:catch
   #:throw
   #:unify_with_occurs_check
   #:repeat
   #:findall
   #:bagof
   #:setof
   #:sort
   #:msort
   #:keysort
   #:true
   #:fail
   #:false
   #:|\\+|
   #:asserta
   #:assert
   #:assertz
   #:retract
   #:retractall
   #:current_predicate
   #:predicate_property
   #:abolish
   #:clause
   #:|\\=|
   #:is
   #:in
   #:ins
   #:|..|
   #:|#=|
   #:|#\\=|
   #:|#<|
   #:|#=<|
   #:|#>|
   #:|#>=|
   #:all_different
   #:labeling
   #:indomain
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
   #:|\\==|
   #:@<
   #:@=<
   #:@>
   #:@>=
   #:compare
   #:unifiable
   #:term_variables
   #:compound
   #:callable
   #:ground
   #:acyclic_term
   #:cyclic_term
   #:functor
   #:arg
   #:copy_term
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
