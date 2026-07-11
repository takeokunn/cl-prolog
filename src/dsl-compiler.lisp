;;;; Macro-time clause and goal compilation helpers.
;;;;
;;;; The public DSL macros stay thin by delegating all data shaping here.
;;;; This keeps runtime proof search free from authoring concerns.

(in-package #:fx.prolog)

(declaim (ftype function %goal-forms))

(defun %control-goal-p (goal)
  "True when GOAL is a control form that nests other goals."
  (and (consp goal)
       (member (first goal) '(and or not) :test #'eq)))

(defun %when-guard-p (goal)
  "True for a DSL-level (:when EXPR) guard goal."
  (and (consp goal)
       (eq (first goal) :when)
       (consp (rest goal))
       (null (cddr goal))))

(defun %when-guard-variables (goal)
  "Return the logic variables captured by the DSL-level WHEN guard GOAL."
  (%collect-variables (second goal)))

(defun %when-guard-form (goal)
  "Compile (:when EXPR) into a form building (:when FUNCTION . VARIABLES)."
  (let* ((test (second goal))
         (variables (%when-guard-variables goal)))
    `(list :when
           (lambda ,variables
             (declare (ignorable ,@variables))
             ,test)
           ,@(mapcar (lambda (variable) `(quote ,variable)) variables))))

(defun %goal-form (goal)
  "Return a form that builds GOAL, compiling nested (:when EXPR) guards."
  (cond
    ((%when-guard-p goal) (%when-guard-form goal))
    ((%control-goal-p goal)
     `(list ',(first goal) ,@(%goal-forms (rest goal))))
    ((%conjunction-p goal)
     `(list ,@(%goal-forms goal)))
    (t `(quote ,goal))))

(defun %goal-forms (goals)
  "Compile GOALS into forms constructing engine-level goal data."
  (mapcar #'%goal-form goals))

(defun %fact-form (head)
  "Return a form constructing a fact for HEAD."
  `(make-fact :predicate ',(first head)
              :args ',(rest head)))

(defun %rule-body-form (goals)
  "Return a form constructing the body list for GOALS."
  `(list ,@(%goal-forms goals)))

(defun %rule-form (head goals)
  "Return a form constructing a rule with HEAD and GOALS."
  `(make-rule :head ',head
              :body ,(%rule-body-form goals)))

(defun %clause-form (clause)
  "Return a form constructing CLAUSE as a fact or rule; validate its shape."
  (unless (and (consp clause) (consp (first clause)))
    (error "Invalid PROLOG clause: ~S" clause))
  (if (null (rest clause))
      (%fact-form (first clause))
      (%rule-form (first clause) (rest clause))))

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
