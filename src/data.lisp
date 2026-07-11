;;;; Rulebase data model.
;;;;
;;;; Facts, rules, and rulebases are plain immutable-by-convention data.
;;;; All proof-search logic lives in engine.lisp; this file only defines
;;;; the shapes the engine walks.

(in-package #:fx.prolog)

(defstruct (fact (:copier nil) (:predicate nil)
                 (:constructor %make-fact (predicate args)))
  "A ground clause: PREDICATE applied to ARGS."
  (predicate nil :type symbol :read-only t)
  (args '() :type list :read-only t))

(defun make-fact (&key (predicate nil) (args '()))
  "Return a fact value with explicit defaults."
  (%make-fact predicate args))

(defstruct (rule (:copier nil) (:predicate nil)
                 (:constructor %make-rule (head body)))
  "A Horn clause: HEAD holds when every goal in BODY holds."
  (head '() :type list :read-only t)
  (body '() :type list :read-only t))

(defun make-rule (&key (head '()) (body '()))
  "Return a rule value with explicit defaults."
  (%make-rule head body))

(defstruct (clause-entry (:copier nil)
                         (:constructor %make-clause-entry (kind clause)))
  "An identity-bearing clause stored in a rulebase's resolution order."
  (kind nil :type (member :fact :rule) :read-only t)
  (clause nil :read-only t))

(defstruct (rulebase (:copier nil)
                     (:constructor %make-rulebase (facts rules clauses)))
  "A queryable, ordered collection of facts and rules."
  (facts '() :type list)
  (rules '() :type list)
  (clauses '() :type list))

(defun %clause-entry-for-fact (fact)
  (%make-clause-entry :fact fact))

(defun %clause-entry-for-rule (rule)
  (%make-clause-entry :rule rule))

(defun make-rulebase (&key (facts '()) (rules '()) (clauses nil clauses-p))
  "Return a rulebase value with explicit defaults."
  (let ((facts (copy-list facts))
        (rules (copy-list rules)))
    (%make-rulebase
     facts rules
     (if clauses-p
         (copy-list clauses)
         (append (mapcar #'%clause-entry-for-fact facts)
                 (mapcar #'%clause-entry-for-rule rules))))))

(defparameter *global-rulebase* (make-rulebase)
  "Mutable top-level rulebase used by DEF-RULE and the DCG helpers.")

(defun clear-global-rulebase! ()
  "Reset the mutable top-level rulebase and return it."
  (setf *global-rulebase* (make-rulebase))
  *global-rulebase*)

(defun assert-fact! (rulebase fact)
  "Push FACT into RULEBASE and return RULEBASE."
  (push fact (rulebase-facts rulebase))
  (push (%clause-entry-for-fact fact) (rulebase-clauses rulebase))
  rulebase)

(defun assert-rule! (rulebase rule)
  "Push RULE into RULEBASE and return RULEBASE."
  (push rule (rulebase-rules rulebase))
  ;; Preserve the historical resolution order: every fact precedes every rule,
  ;; while a newly asserted rule precedes the existing rules.
  (let* ((entry (%clause-entry-for-rule rule))
         (clauses (rulebase-clauses rulebase))
         (first-rule (position :rule clauses :key #'clause-entry-kind)))
    (setf (rulebase-clauses rulebase)
          (if first-rule
              (append (subseq clauses 0 first-rule)
                      (list entry)
                      (nthcdr first-rule clauses))
              (append clauses (list entry)))))
  rulebase)

(defun %assert-clause-entry! (rulebase entry position)
  "Insert ENTRY at POSITION while keeping the compatibility projections synchronized."
  (let ((clause (clause-entry-clause entry)))
    (ecase position
      (:first
       (push entry (rulebase-clauses rulebase))
       (ecase (clause-entry-kind entry)
         (:fact (push clause (rulebase-facts rulebase)))
         (:rule (push clause (rulebase-rules rulebase)))))
      (:last
       (setf (rulebase-clauses rulebase)
             (append (rulebase-clauses rulebase) (list entry)))
       (ecase (clause-entry-kind entry)
         (:fact (setf (rulebase-facts rulebase)
                      (append (rulebase-facts rulebase) (list clause))))
         (:rule (setf (rulebase-rules rulebase)
                      (append (rulebase-rules rulebase) (list clause)))))))
    rulebase))

(defun %remove-clause-entry! (rulebase entry)
  "Delete ENTRY by identity and synchronize the compatibility projections."
  (setf (rulebase-clauses rulebase)
        (delete entry (rulebase-clauses rulebase) :test #'eq :count 1))
  (let ((clause (clause-entry-clause entry)))
    (ecase (clause-entry-kind entry)
      (:fact
       (setf (rulebase-facts rulebase)
             (delete clause (rulebase-facts rulebase) :test #'eq :count 1)))
      (:rule
       (setf (rulebase-rules rulebase)
             (delete clause (rulebase-rules rulebase) :test #'eq :count 1)))))
  rulebase)
