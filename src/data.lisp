;;;; Ordered clause data model and operations.

(in-package #:fx.prolog)

(defstruct (clause (:copier nil)
                   (:constructor make-clause (head &optional (body '()))))
  "A Horn clause. An empty BODY denotes a fact."
  (head '() :type list :read-only t)
  (body '() :type list :read-only t))

(defstruct (rulebase (:copier nil)
                     (:constructor %make-rulebase (clauses)))
  "A mutable ordered collection of clauses."
  (clauses '() :type list))

(defun make-rulebase (&key (clauses '()))
  "Return a rulebase containing CLAUSES in resolution order."
  (%make-rulebase (copy-list clauses)))

(defun rulebase-insert-clause! (rulebase clause &key (position :last))
  "Insert CLAUSE at POSITION (:FIRST or :LAST) and return RULEBASE."
  (ecase position
    (:first (push clause (rulebase-clauses rulebase)))
    (:last (setf (rulebase-clauses rulebase)
                 (append (rulebase-clauses rulebase) (list clause)))))
  rulebase)

(defun rulebase-remove-clause! (rulebase clause)
  "Remove one identity-equal CLAUSE and return RULEBASE."
  (setf (rulebase-clauses rulebase)
        (delete clause (rulebase-clauses rulebase) :test #'eq :count 1))
  rulebase)
