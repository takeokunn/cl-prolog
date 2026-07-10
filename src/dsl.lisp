;;;; Rule definition DSL.
;;;;
;;;; Clauses are written as data and compiled by macros at the boundary:
;;;; a one-element clause is a fact, anything longer is HEAD followed by
;;;; body goals.  (:when EXPR) guard goals are compiled into closures over
;;;; their logic variables at macroexpansion time, so no evaluation of
;;;; user expressions ever happens inside the engine.

(in-package #:fx.prolog)

(defun %when-guard-p (goal)
  "True for a DSL-level (:when EXPR) guard goal."
  (and (consp goal)
       (eq (first goal) :when)
       (consp (rest goal))
       (null (cddr goal))))

(defun %when-guard-form (goal)
  "Compile (:when EXPR) into a form building (:when FUNCTION . VARIABLES)."
  (let* ((test (second goal))
         (variables (%collect-variables test)))
    `(list :when
           (lambda ,variables
             (declare (ignorable ,@variables))
             ,test)
           ,@(mapcar (lambda (variable) `(quote ,variable)) variables))))

(defun %goal-form (goal)
  "Return a form that builds GOAL, compiling nested (:when EXPR) guards."
  (cond
    ((%when-guard-p goal) (%when-guard-form goal))
    ((and (consp goal) (member (first goal) '(and or not) :test #'eq))
     `(list ',(first goal) ,@(mapcar #'%goal-form (rest goal))))
    ((%conjunction-p goal)
     `(list ,@(mapcar #'%goal-form goal)))
    (t `(quote ,goal))))

(defun %clause-form (clause)
  "Return a form constructing CLAUSE as a fact or rule; validate its shape."
  (unless (and (consp clause) (consp (first clause)))
    (error "Invalid PROLOG clause: ~S" clause))
  (if (null (rest clause))
      `(make-fact :predicate ',(first (first clause))
                  :args ',(rest (first clause)))
      `(make-rule :head ',(first clause)
                  :body (list ,@(mapcar #'%goal-form (rest clause))))))

(defun %partition-clause-forms (clauses)
  "Split CLAUSES into (VALUES FACT-FORMS RULE-FORMS) preserving order."
  (let ((fact-forms '())
        (rule-forms '()))
    (dolist (clause clauses)
      (let ((form (%clause-form clause)))
        (if (null (rest clause))
            (push form fact-forms)
            (push form rule-forms))))
    (values (nreverse fact-forms) (nreverse rule-forms))))

(defmacro prolog (&body clauses)
  "Build a rulebase from CLAUSES.

A one-element clause ((PRED . ARGS)) is a fact; a longer clause
(HEAD GOAL...) is a rule.  (:when EXPR) goals are compiled to closures
over their logic variables."
  (multiple-value-bind (fact-forms rule-forms)
      (%partition-clause-forms clauses)
    `(make-rulebase :facts (list ,@fact-forms)
                    :rules (list ,@rule-forms))))

(defmacro define-rulebase (name &body clauses)
  "Define NAME as a special variable holding the rulebase built from CLAUSES."
  `(defparameter ,name (prolog ,@clauses)))

(defmacro extend-rulebase (base &body clauses)
  "Return a new rulebase whose clauses shadow-extend BASE."
  (let ((extension (gensym "EXTENSION")))
    `(let ((,extension (prolog ,@clauses)))
       (make-rulebase
        :facts (append (rulebase-facts ,extension) (rulebase-facts ,base))
        :rules (append (rulebase-rules ,extension) (rulebase-rules ,base))))))

(defmacro def-rule (head &body body)
  "Register the rule HEAD :- BODY in *GLOBAL-RULEBASE* at load time."
  `(eval-when (:load-toplevel :execute)
     (assert-rule! *global-rulebase*
                   (make-rule :head ',head
                              :body (list ,@(mapcar #'%goal-form body))))
     ',head))

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
