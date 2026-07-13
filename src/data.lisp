;;;; Ordered clause data model and operations.

(in-package #:cl-prolog)

(defun %make-rulebase-table-session (rulebase)
  (declare (cl:ignore rulebase))
  (%make-table-session (make-hash-table :test #'equal)
                       (make-hash-table :test #'equal)
                       (make-hash-table :test #'equal)
                       (make-hash-table :test #'equal)))

(defun %make-source-registry ()
  "Return an empty registry of canonical source pathnames and load states."
  (make-hash-table :test #'equal))

(defun %copy-source-registry (registry)
  "Return a detached copy of REGISTRY for a rulebase transaction."
  (let ((copy (%make-source-registry)))
    (maphash (lambda (pathname record)
               (setf (gethash pathname copy)
                     (let ((clone (%make-source-record
                                   (%source-record-state record))))
                       (setf (%source-record-operators clone)
                             (copy-tree (%source-record-operators record))
                             (%source-record-predicate-properties clone)
                             (copy-tree
                              (%source-record-predicate-properties record))
                             (%source-record-table-declarations clone)
                             (copy-tree
                              (%source-record-table-declarations record)))
                       clone)))
             registry)
    copy))

(defstruct (rulebase (:copier nil)
                     (:constructor %make-rulebase
                         (entries predicate-index revision operator-table
                          predicate-properties io-context module-registry source-registry
                          prolog-flag-values char-conversions table-declarations)))
  "An ordered logical-update database of clauses."
  (entries '() :type list)
  (predicate-index (make-hash-table :test #'equal) :type hash-table)
  (revision 0 :type (integer 0 *))
  (operator-table *standard-operator-table* :type operator-table)
  (predicate-properties (make-hash-table :test #'equal) :type hash-table)
  (io-context (make-prolog-io-context) :type prolog-io-context)
  (module-registry (make-module-registry) :type module-registry)
  (source-registry (%make-source-registry) :type hash-table)
  (prolog-flag-values (make-hash-table :test #'equal) :type hash-table)
  (char-conversions (make-hash-table :test #'eql) :type hash-table)
  (table-declarations (make-hash-table :test #'equal) :type hash-table))

(defun %stored-clause-predicate-key (entry)
  "Return ENTRY's (module predicate arity) key, or NIL for a malformed head."
  (let ((head (clause-head (%stored-clause-clause entry))))
    (when (and (consp head) (symbolp (first head)))
      (list (%stored-clause-module entry)
            (first head)
            (length (rest head))))))

(defun %make-rulebase-predicate-index (entries)
  "Index ENTRIES by predicate while retaining their resolution order."
  (let ((index (make-hash-table :test #'equal)))
    (dolist (entry (reverse entries))
      (let ((key (%stored-clause-predicate-key entry)))
        (when key
          (push entry (gethash key index)))))
    index))

(defun %rulebase-source-state (rulebase canonical-pathname)
  "Return CANONICAL-PATHNAME's load state and whether it is registered."
  (let ((record (gethash canonical-pathname
                         (rulebase-source-registry rulebase))))
    (and record (%source-record-state record))))

(defun %rulebase-source-record (rulebase canonical-pathname)
  (gethash canonical-pathname (rulebase-source-registry rulebase)))

(defun %set-rulebase-source-state! (rulebase canonical-pathname state)
  "Record STATE for CANONICAL-PATHNAME and return STATE."
  (check-type state (member :loading :loaded))
  (let ((record (or (%rulebase-source-record rulebase canonical-pathname)
                    (%make-source-record state))))
    (setf (%source-record-state record) state
          (gethash canonical-pathname (rulebase-source-registry rulebase)) record)
    state))

(defun make-rulebase (&key (clauses '()) (io-context (make-prolog-io-context)))
  "Return a rulebase containing CLAUSES in resolution order."
  (let ((entries (mapcar (lambda (clause)
                           (%make-stored-clause
                            clause +default-prolog-module+ 0))
                         clauses)))
    (%make-rulebase
     entries
     (%make-rulebase-predicate-index entries)
     0
     *standard-operator-table*
     (make-hash-table :test #'equal)
     io-context
     (make-module-registry)
     (%make-source-registry)
     (make-hash-table :test #'equal)
     (make-hash-table :test #'eql)
     (make-hash-table :test #'equal))))

(defun %copy-clause (clause)
  "Copy CLAUSE while preserving repeated logic-variable identities."
  (make-clause (copy-tree (clause-head clause))
               (copy-tree (clause-body clause))))

(defun %copy-rulebase (rulebase &optional (copy-clause #'identity))
  "Return a detached mutable copy suitable for transactional updates.

COPY-CLAUSE maps each stored clause into the copy; the default shares clause
terms because transactional updates never mutate them in place."
  (let ((entries
          (mapcar (lambda (entry)
                    (let ((copy (%make-stored-clause
                                 (funcall copy-clause
                                          (%stored-clause-clause entry))
                                 (%stored-clause-module entry)
                                 (%stored-clause-born-revision entry)
                                 (%stored-clause-source entry))))
                      (setf (%stored-clause-died-revision copy)
                            (%stored-clause-died-revision entry))
                      copy))
                  (rulebase-entries rulebase))))
    (%make-rulebase
     entries
     (%make-rulebase-predicate-index entries)
     (rulebase-revision rulebase)
     (rulebase-operator-table rulebase)
     (let ((copy (make-hash-table :test #'equal)))
       (maphash (lambda (key value) (setf (gethash key copy) value))
                (rulebase-predicate-properties rulebase))
       copy)
     (%copy-prolog-io-context (rulebase-io-context rulebase))
     (module-registry-copy (rulebase-module-registry rulebase))
     (%copy-source-registry (rulebase-source-registry rulebase))
     (let ((copy (make-hash-table :test #'equal)))
       (maphash (lambda (name value) (setf (gethash name copy) value))
                (rulebase-prolog-flag-values rulebase))
       copy)
     (let ((copy (make-hash-table :test #'eql)))
       (maphash (lambda (from to) (setf (gethash from copy) to))
                (rulebase-char-conversions rulebase))
       copy)
     (let ((copy (make-hash-table :test #'equal)))
       (maphash (lambda (key owners)
                  (setf (gethash key copy) (copy-list owners)))
                (rulebase-table-declarations rulebase))
       copy))))

(defun copy-rulebase (rulebase)
  "Return a detached copy of RULEBASE, including its complete runtime state.

Stored clauses and their cons-based terms are copied, so mutating terms
reachable from one rulebase never affects the other. Immutable atoms and
persistent metadata such as operator tables may be shared."
  (check-type rulebase rulebase)
  (%copy-rulebase rulebase #'%copy-clause))

(defun %replace-rulebase! (target source)
  "Replace TARGET's complete state with SOURCE after a successful transaction."
  (setf (rulebase-entries target) (rulebase-entries source)
        (rulebase-predicate-index target) (rulebase-predicate-index source)
        (rulebase-revision target) (rulebase-revision source)
        (rulebase-operator-table target) (rulebase-operator-table source)
        (rulebase-predicate-properties target)
        (rulebase-predicate-properties source)
        (rulebase-io-context target) (rulebase-io-context source)
        (rulebase-module-registry target)
        (rulebase-module-registry source)
        (rulebase-source-registry target)
        (rulebase-source-registry source)
        (rulebase-prolog-flag-values target)
        (rulebase-prolog-flag-values source)
        (rulebase-char-conversions target)
        (rulebase-char-conversions source)
        (rulebase-table-declarations target)
        (rulebase-table-declarations source))
  target)

(defun %next-rulebase-revision! (rulebase)
  (incf (rulebase-revision rulebase)))
