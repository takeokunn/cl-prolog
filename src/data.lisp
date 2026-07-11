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

(defstruct (rulebase (:copier nil)
                     (:constructor %make-rulebase (facts rules)))
  "A queryable collection of facts and rules.

Facts are always tried before rules during proof search."
  (facts '() :type list)
  (rules '() :type list))

(defun make-rulebase (&key (facts '()) (rules '()))
  "Return a rulebase value with explicit defaults."
  (%make-rulebase facts rules))

(defparameter *global-rulebase* (make-rulebase)
  "Mutable top-level rulebase used by DEF-RULE and the DCG helpers.")

(defun clear-global-rulebase! ()
  "Reset the mutable top-level rulebase and return it."
  (setf *global-rulebase* (make-rulebase))
  *global-rulebase*)

(defun assert-fact! (rulebase fact)
  "Push FACT into RULEBASE and return RULEBASE."
  (push fact (rulebase-facts rulebase))
  rulebase)

(defun assert-rule! (rulebase rule)
  "Push RULE into RULEBASE and return RULEBASE."
  (push rule (rulebase-rules rulebase))
  rulebase)
