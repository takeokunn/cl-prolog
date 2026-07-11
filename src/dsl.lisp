;;;; Rule definition DSL.
;;;;
;;;; Clauses are written as data and compiled by macros at the boundary:
;;;; a one-element clause is a fact, anything longer is HEAD followed by
;;;; body goals.  (:when EXPR) guard goals are compiled into closures over
;;;; their logic variables at macroexpansion time, so no evaluation of
;;;; user expressions ever happens inside the engine.

(in-package #:fx.prolog)

(defmacro prolog (&body clauses)
  "Build a rulebase from CLAUSES.

A one-element clause ((PRED . ARGS)) is a fact; a longer clause
(HEAD GOAL...) is a rule.  (:when EXPR) goals are compiled to closures
over their logic variables."
  `(make-rulebase :clauses (list ,@(%clause-forms clauses))))

(defmacro define-rulebase (name &body clauses)
  "Define NAME as a special variable holding the rulebase built from CLAUSES."
  `(defparameter ,name (prolog ,@clauses)))

(defmacro extend-rulebase (base &body clauses)
  "Return a new rulebase whose clauses shadow-extend BASE."
  (let ((extension (gensym "EXTENSION")))
    `(let ((,extension (prolog ,@clauses)))
       (make-rulebase :clauses (append (rulebase-clauses ,extension)
                                       (rulebase-clauses ,base))))))

(defun %register-global-rule (head body)
  "Insert the rule HEAD :- BODY into *GLOBAL-RULEBASE* and return HEAD."
  (rulebase-insert-clause! *global-rulebase* (make-clause head body))
  head)

(defmacro def-rule (head &body body)
  "Register the rule HEAD :- BODY in *GLOBAL-RULEBASE* at load time."
  `(eval-when (:load-toplevel :execute)
     (%register-global-rule ',head ,(%rule-body-form body))))

(defmacro with-prolog-query (binding-vars (rulebase query &key (max-depth '*max-prolog-depth*))
                             &body body)
  "Bind BINDING-VARS from the first solution of QUERY and run BODY.

BODY is skipped entirely when QUERY has no proof."
  (let ((solution (gensym "SOLUTION")))
    `(let ((,solution (query-prolog-first ,rulebase ,query :max-depth ,max-depth)))
       (when ,solution
         (let ,(mapcar (lambda (variable)
                         `(,variable (solution-binding ',variable ,solution)))
                       binding-vars)
           (declare (ignorable ,@binding-vars))
           ,@body)))))

(defmacro prolog-match (rulebase &body clauses)
  "Evaluate the body of the first clause whose query succeeds in RULEBASE."
  `(cond
     ,@(mapcar (lambda (clause)
                 `((prolog-succeeds-p ,rulebase ',(first clause))
                   ,@(rest clause)))
               clauses)))
