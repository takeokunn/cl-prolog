(in-package #:cl-prolog)

(defun %record-source-operator-effect! (effect)
  (when *current-prolog-source-record*
    (push effect (%source-record-operators *current-prolog-source-record*))))

(defun %record-source-predicate-property!
    (module predicate arity property)
  (when *current-prolog-source-record*
    (push (list module predicate arity property)
          (%source-record-predicate-properties
           *current-prolog-source-record*))))

(defun %record-source-table-declaration! (module predicate arity owner)
  (when *current-prolog-source-record*
    (push (list module predicate arity owner)
          (%source-record-table-declarations
           *current-prolog-source-record*))))

