;;;; Tabling data: the %table-entry/%table-session structs recording
;;;; memoized answers, and variant canonicalization (%canonicalize-variant,
;;;; %variant-graph-key, %instantiate-variant) used to key and replay them.

(in-package #:cl-prolog)

(defstruct (%table-entry (:copier nil)
                         (:constructor %make-table-entry ()))
  "Variant-call answers accumulated during one tabled proof."
  (answers '() :type list)
  (answers-tail '() :type list)
  (answer-count 0 :type (integer 0 *))
  (answer-index (make-hash-table :test #'equal)
                :type hash-table :read-only t)
  (cyclic-answer-index (make-hash-table :test #'equal)
                       :type hash-table :read-only t))

(defstruct (%table-session
            (:copier nil)
            (:constructor %make-table-session
                (entries module-entries predicate-entries left-recursion)))
  "Tables shared by every proof nested within one public query."
  (entries (make-hash-table :test #'equal) :type hash-table :read-only t)
  (module-entries (make-hash-table :test #'equal)
                  :type hash-table :read-only t)
  (predicate-entries (make-hash-table :test #'equal)
                     :type hash-table :read-only t)
  (left-recursion (make-hash-table :test #'equal)
                  :type hash-table :read-only t))

(defparameter +variant-variable-marker+ (gensym "VARIANT-VARIABLE-")
  "Unforgeable marker used in canonical table keys and answers.")

(defun %make-rulebase-table-session (rulebase)
  (declare (cl:ignore rulebase))
  (%make-table-session (make-hash-table :test #'equal)
                       (make-hash-table :test #'equal)
                       (make-hash-table :test #'equal)
                       (make-hash-table :test #'equal)))

(defun %canonicalize-variant (term)
  "Rename TERM's variables by first occurrence, preserving sharing.
The second value reports whether TERM contains a cons cycle."
  (let ((variables (make-hash-table :test #'eq))
        (copies (make-hash-table :test #'eq))
        (active (make-hash-table :test #'eq))
        (next-index 0)
        (cyclic-p nil))
    (labels ((canonicalize (node)
               (cond
                 ((logic-var-p node)
                  (or (gethash node variables)
                      (setf (gethash node variables)
                            (list +variant-variable-marker+
                                  (prog1 next-index (incf next-index))))))
                 ((consp node)
                  (multiple-value-bind (copy present-p)
                      (gethash node copies)
                    (if present-p
                        (progn
                          (when (gethash node active)
                            (setf cyclic-p t))
                          copy)
                        (let ((copy (cons nil nil)))
                          (setf (gethash node copies) copy
                                (gethash node active) t
                                (car copy) (canonicalize (car node))
                                (cdr copy) (canonicalize (cdr node)))
                          (remhash node active)
                          copy))))
                 (t node))))
      (values (canonicalize term) cyclic-p))))

(defun %variant-graph-key (term)
  "Return an EQUAL-safe encoding of TERM's cons graph."
  (let ((identities (make-hash-table :test #'eq))
        (next-index 0))
    (labels ((encode (node)
               (if (consp node)
                   (multiple-value-bind (index present-p)
                       (gethash node identities)
                     (if present-p
                         (list :reference index)
                         (let ((index (prog1 next-index
                                        (incf next-index))))
                           (setf (gethash node identities) index)
                           (list :cons index
                                 (encode (car node))
                                 (encode (cdr node))))))
                   (list :atom node))))
      (encode term))))

(defun %instantiate-variant (term)
  "Replace canonical variable markers in TERM with fresh logic variables.
Ground subtrees are shared because unification never mutates terms."
  (let ((variables nil)
        (copies (make-hash-table :test #'eq)))
    (labels ((instantiate (node)
               (cond
                 ((and (consp node)
                       (eq (first node) +variant-variable-marker+)
                       (consp (rest node))
                       (null (cddr node)))
                  (let ((table (or variables
                                   (setf variables
                                         (make-hash-table :test #'equal)))))
                    (or (gethash node table)
                        (setf (gethash node table)
                              (fresh-logic-variable "?TABLE")))))
                 ((consp node)
                  (multiple-value-bind (copy present-p)
                      (gethash node copies)
                    (if present-p
                        copy
                        (let ((copy (cons nil nil)))
                          (setf (gethash node copies) copy)
                          (let ((new-car (instantiate (car node)))
                                (new-cdr (instantiate (cdr node))))
                            (setf (car copy) new-car
                                  (cdr copy) new-cdr)
                            (if (and (eq new-car (car node))
                                     (eq new-cdr (cdr node)))
                                (progn
                                  (setf (gethash node copies) node)
                                  node)
                                copy))))))
                 (t node))))
      (instantiate term))))
