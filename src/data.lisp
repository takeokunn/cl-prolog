;;;; Ordered clause data model and operations.

(in-package #:cl-prolog)

(defun %make-rulebase-table-session (rulebase)
  (declare (cl:ignore rulebase))
  (%make-table-session (make-hash-table :test #'equal)
                       (make-hash-table :test #'equal)
                       (make-hash-table :test #'equal)
                       (make-hash-table :test #'equal)))

(defun %canonicalize-variant (term)
  "Rename TERM's variables by first occurrence, preserving sharing."
  (let ((variables (make-hash-table :test #'eq))
        (copies (make-hash-table :test #'eq))
        (next-index 0))
    (labels ((canonicalize (node)
               (cond
                 ((logic-var-p node)
                  (or (gethash node variables)
                      (setf (gethash node variables)
                            (list +variant-variable-marker+
                                  (prog1 next-index (incf next-index))))))
                 ((consp node)
                  (or (gethash node copies)
                      (let ((copy (cons nil nil)))
                        (setf (gethash node copies) copy
                              (car copy) (canonicalize (car node))
                              (cdr copy) (canonicalize (cdr node)))
                        copy)))
                 (t node))))
      (canonicalize term))))

(defun %instantiate-variant (term)
  "Replace canonical variable markers in TERM with fresh logic variables."
  (let ((variables (make-hash-table :test #'equal)))
    (labels ((instantiate (node)
               (cond
                 ((and (consp node)
                       (eq (first node) +variant-variable-marker+)
                       (consp (rest node))
                       (null (cddr node)))
                  (or (gethash node variables)
                      (setf (gethash node variables)
                            (fresh-logic-variable "?TABLE"))))
                 ((consp node)
                  (cons (instantiate (car node))
                        (instantiate (cdr node))))
                 (t node))))
      (instantiate term))))

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

(defun rulebase-extend (rulebase clauses)
  "Return a detached copy of RULEBASE shadow-extended by CLAUSES.

CLAUSES retain their order and precede the clauses already visible in
RULEBASE.  Operator declarations, predicate properties, I/O state, modules,
source registrations, flags, and character conversions are copied as well."
  (let ((extended (copy-rulebase rulebase)))
    (dolist (clause (reverse clauses) extended)
      (rulebase-insert-clause! extended clause :position :first))))

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

(defun %rulebase-tabled-p (rulebase predicate arity
                           &optional (module +default-prolog-module+))
  (not (null (gethash (list module predicate arity)
                      (rulebase-table-declarations rulebase)))))

(defun %add-rulebase-table-declaration! (rulebase predicate arity owner
                                          &optional (module +default-prolog-module+))
  "Add OWNER's table declaration, advancing the revision on first ownership."
  (let* ((key (list module predicate arity))
         (owners (gethash key (rulebase-table-declarations rulebase))))
    (unless (member owner owners :test #'equal)
      (unless owners
        (%next-rulebase-revision! rulebase))
      (push owner (gethash key (rulebase-table-declarations rulebase))))
    rulebase))

(defun %remove-rulebase-table-declaration! (rulebase predicate arity owner
                                             &optional (module +default-prolog-module+))
  "Remove OWNER's declaration, advancing the revision when no owner remains."
  (let* ((key (list module predicate arity))
         (owners (gethash key (rulebase-table-declarations rulebase)))
         (remaining (remove owner owners :test #'equal)))
    (unless (= (length owners) (length remaining))
      (if remaining
          (setf (gethash key (rulebase-table-declarations rulebase)) remaining)
          (progn
            (remhash key (rulebase-table-declarations rulebase))
            (%next-rulebase-revision! rulebase))))
    rulebase))

(defun %remove-rulebase-table-declarations! (rulebase predicate arity
                                              &optional (module +default-prolog-module+))
  "Remove every table declaration for PREDICATE/ARITY in MODULE."
  (let ((key (list module predicate arity)))
    (when (remhash key (rulebase-table-declarations rulebase))
      (%next-rulebase-revision! rulebase))
    rulebase))

(defun %rulebase-predicate-property (rulebase predicate arity
                                     &optional (module +default-prolog-module+))
  (gethash (list module predicate arity) (rulebase-predicate-properties rulebase)))

(defun %set-rulebase-predicate-property! (rulebase predicate arity property
                                          &optional (module +default-prolog-module+))
  (setf (gethash (list module predicate arity)
                 (rulebase-predicate-properties rulebase))
        property))

(defun %remove-rulebase-predicate-property! (rulebase predicate arity
                                              &optional (module +default-prolog-module+))
  "Remove PREDICATE/ARITY's declaration and return whether one existed."
  (remhash (list module predicate arity)
           (rulebase-predicate-properties rulebase)))

(defun %rulebase-declared-predicate-indicators
    (rulebase &optional (module +default-prolog-module+))
  "Return declared predicate indicators in parser AST form (/ NAME ARITY)."
  (let ((indicators '()))
    (maphash (lambda (key property)
               (declare (cl:ignore property))
               (when (eq (first key) module)
                 (push (list '/ (second key) (third key)) indicators)))
             (rulebase-predicate-properties rulebase))
    indicators))

(defun %next-rulebase-revision! (rulebase)
  (incf (rulebase-revision rulebase)))

(defun %stored-clause-visible-p (entry revision)
  (and (<= (%stored-clause-born-revision entry) revision)
       (let ((died (%stored-clause-died-revision entry)))
         (or (null died) (< revision died)))))

(defun %rulebase-snapshot (rulebase)
  "Return the current revision and a detached list of visible internal entries."
  (let ((revision (rulebase-revision rulebase)))
    (values revision
            (loop for entry in (rulebase-entries rulebase)
                  when (%stored-clause-visible-p entry revision)
                    collect entry))))

(defun rulebase-visible-clauses (rulebase)
  "Return clauses visible at one current logical-update snapshot."
  (multiple-value-bind (revision entries) (%rulebase-snapshot rulebase)
    (declare (cl:ignore revision))
    (mapcar #'%stored-clause-clause entries)))

(defun %rulebase-module-entries (rulebase module)
  "Return the visible stored clauses defined by MODULE."
  (multiple-value-bind (revision entries) (%rulebase-snapshot rulebase)
    (declare (cl:ignore revision))
    (remove module entries :test-not #'eq :key #'%stored-clause-module)))

(defun %rulebase-predicate-entries-at-revision
    (rulebase module predicate arity revision)
  "Return PREDICATE/ARITY clauses visible in MODULE at REVISION."
  (loop for entry in (gethash (list module predicate arity)
                              (rulebase-predicate-index rulebase))
        when (%stored-clause-visible-p entry revision)
          collect entry))

(defun %rulebase-predicate-entries (rulebase module predicate arity)
  "Return the current revision and visible entries for one predicate."
  (let ((revision (rulebase-revision rulebase)))
    (values revision
            (%rulebase-predicate-entries-at-revision
             rulebase module predicate arity revision))))

(defun rulebase-insert-clause! (rulebase clause
                                &key (position :last)
                                  (module +default-prolog-module+)
                                  source)
  "Insert CLAUSE at POSITION (:FIRST or :LAST) and return RULEBASE."
  (let ((entry (%make-stored-clause clause module
                                    (%next-rulebase-revision! rulebase)
                                    source)))
    (ecase position
      (:first (push entry (rulebase-entries rulebase)))
      (:last (setf (rulebase-entries rulebase)
                   (append (rulebase-entries rulebase) (list entry)))))
    (let ((key (%stored-clause-predicate-key entry)))
      (when key
        (ecase position
          (:first (push entry (gethash key (rulebase-predicate-index rulebase))))
          (:last (setf (gethash key (rulebase-predicate-index rulebase))
                       (append (gethash key (rulebase-predicate-index rulebase))
                               (list entry))))))))
  rulebase)

(defun %rulebase-retract-entry! (rulebase entry)
  "Mark ENTRY dead and return true; return NIL when another update won."
  (when (null (%stored-clause-died-revision entry))
    (setf (%stored-clause-died-revision entry)
          (%next-rulebase-revision! rulebase))
    t))

(defun %rulebase-retract-entries! (rulebase entries)
  "Atomically assign one death revision to every live member of ENTRIES."
  (let ((live (remove-if #'%stored-clause-died-revision entries)))
    (when live
      (let ((revision (%next-rulebase-revision! rulebase)))
        (dolist (entry live)
          (setf (%stored-clause-died-revision entry) revision))))
    (not (null live))))
