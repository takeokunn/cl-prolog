;;;; Rulebase data model.
;;;;
;;;; Facts, rules, and rulebases are plain immutable-by-convention data.
;;;; All proof-search logic lives in engine.lisp; this file only defines
;;;; the shapes the engine walks.

(in-package #:fx.prolog)

(defstruct (fact (:copier nil) (:predicate nil))
  "A ground clause: PREDICATE applied to ARGS."
  (predicate nil :type symbol :read-only t)
  (args '() :type list :read-only t))

(defstruct (rule (:copier nil) (:predicate nil))
  "A Horn clause: HEAD holds when every goal in BODY holds."
  (head '() :type list :read-only t)
  (body '() :type list :read-only t))

(defstruct (rulebase (:copier nil))
  "A queryable collection of facts and rules.

Facts are always tried before rules during proof search."
  (facts '() :type list)
  (rules '() :type list))

(defparameter *global-rulebase* (make-rulebase)
  "Mutable top-level rulebase used by DEF-RULE and the DCG helpers.")

(defun make-empty-rulebase ()
  "Return a fresh empty rulebase."
  (make-rulebase))

(defun clear-global-rulebase! ()
  "Reset the mutable top-level rulebase and return it."
  (setf (rulebase-facts *global-rulebase*) '()
        (rulebase-rules *global-rulebase*) '())
  *global-rulebase*)

(defun assert-fact! (rulebase fact)
  "Push FACT into RULEBASE and return RULEBASE."
  (push fact (rulebase-facts rulebase))
  rulebase)

(defun assert-rule! (rulebase rule)
  "Push RULE into RULEBASE and return RULEBASE."
  (push rule (rulebase-rules rulebase))
  rulebase)
