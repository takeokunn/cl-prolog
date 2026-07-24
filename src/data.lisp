;;;; Ordered clause data model and operations: the clause/rulebase
;;;; structs, predicate-index maintenance, and insert/retract/visibility
;;;; queries.  Tabling data lives in table-variant.lisp; source-load
;;;; bookkeeping lives in source-registry.lisp.

(in-package #:cl-prolog)

(defstruct (clause (:copier nil)
                   (:constructor make-clause (head &optional (body '()))))
  "A Horn clause. An empty BODY denotes a fact."
  (head '() :type list :read-only t)
  (body '() :type list :read-only t))

(defstruct (%stored-clause (:copier nil)
                           (:constructor %make-stored-clause
                               (clause module born-revision &optional source)))
  "Internal lifetime metadata for one clause in a rulebase."
  (clause (make-clause '()) :type clause :read-only t)
  (module +default-prolog-module+ :type symbol :read-only t)
  (born-revision 0 :type (integer 0 *) :read-only t)
  (source nil :type (or null pathname) :read-only t)
  (died-revision nil :type (or null (integer 0 *))))

(defstruct (rulebase (:copier nil)
                     (:constructor %make-rulebase
                         (entries entries-tail predicate-index predicate-tails
                          revision operator-table predicate-properties io-context
                          module-registry source-registry prolog-flag-values
                          char-conversions table-declarations)))
  "An ordered logical-update database of clauses."
  (entries '() :type list)
  (entries-tail '() :type list)
  (predicate-index (make-hash-table :test #'equal) :type hash-table)
  (predicate-tails (make-hash-table :test #'equal) :type hash-table)
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

(defun %append-to-predicate-index-tail! (key entry index tails)
  "Append ENTRY as the new tail of predicate KEY's entry list in INDEX,
using TAILS to reach the current tail cell in O(1)."
  (let ((cell (list entry))
        (tail (gethash key tails)))
    (if tail
        (setf (cdr tail) cell)
        (setf (gethash key index) cell))
    (setf (gethash key tails) cell)))

(defun %make-rulebase-predicate-index (entries)
  "Index ENTRIES by predicate and return its tail metadata as a second value."
  (let ((index (make-hash-table :test #'equal))
        (tails (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (let ((key (%stored-clause-predicate-key entry)))
        (when key
          (%append-to-predicate-index-tail! key entry index tails))))
    (values index tails)))

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
    (multiple-value-bind (predicate-index predicate-tails)
        (%make-rulebase-predicate-index entries)
      (%make-rulebase
       entries
       (last entries)
       predicate-index
       predicate-tails
       0
       *standard-operator-table*
       (make-hash-table :test #'equal)
       io-context
       (make-module-registry)
       (%make-source-registry)
       (make-hash-table :test #'equal)
       (make-hash-table :test #'eql)
       (make-hash-table :test #'equal)))))

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
    (multiple-value-bind (predicate-index predicate-tails)
        (%make-rulebase-predicate-index entries)
      (%make-rulebase
       entries
       (last entries)
       predicate-index
       predicate-tails
       (rulebase-revision rulebase)
       (rulebase-operator-table rulebase)
       (%copy-hash-table (rulebase-predicate-properties rulebase))
       (%copy-prolog-io-context (rulebase-io-context rulebase))
       (module-registry-copy (rulebase-module-registry rulebase))
       (%copy-source-registry (rulebase-source-registry rulebase))
       (%copy-hash-table (rulebase-prolog-flag-values rulebase))
       (%copy-hash-table (rulebase-char-conversions rulebase))
       (let ((copy (make-hash-table :test #'equal)))
         (maphash (lambda (key owners)
                    (setf (gethash key copy) (copy-list owners)))
                  (rulebase-table-declarations rulebase))
         copy)))))

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
        (rulebase-entries-tail target) (rulebase-entries-tail source)
        (rulebase-predicate-index target) (rulebase-predicate-index source)
        (rulebase-predicate-tails target) (rulebase-predicate-tails source)
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

(defun %predicate-property-key (predicate arity module)
  (list module predicate arity))

(defun %rulebase-predicate-property (rulebase predicate arity
                                     &optional (module +default-prolog-module+))
  (gethash (%predicate-property-key predicate arity module)
           (rulebase-predicate-properties rulebase)))

(defun %set-rulebase-predicate-property! (rulebase predicate arity property
                                          &optional (module +default-prolog-module+))
  (setf (gethash (%predicate-property-key predicate arity module)
                 (rulebase-predicate-properties rulebase))
        property))

(defun %remove-rulebase-predicate-property! (rulebase predicate arity
                                              &optional (module +default-prolog-module+))
  "Remove PREDICATE/ARITY's declaration and return whether one existed."
  (remhash (%predicate-property-key predicate arity module)
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
      (:first
       (let ((cell (cons entry (rulebase-entries rulebase))))
         (setf (rulebase-entries rulebase) cell)
         (when (null (cdr cell))
           (setf (rulebase-entries-tail rulebase) cell))))
      (:last
       (let ((cell (list entry))
             (tail (rulebase-entries-tail rulebase)))
         (if tail
             (setf (cdr tail) cell)
             (setf (rulebase-entries rulebase) cell))
         (setf (rulebase-entries-tail rulebase) cell))))
    (let ((key (%stored-clause-predicate-key entry)))
      (when key
        (let ((index (rulebase-predicate-index rulebase))
              (tails (rulebase-predicate-tails rulebase)))
          (ecase position
            (:first
             (let* ((entries (gethash key index))
                    (cell (cons entry entries)))
               (setf (gethash key index) cell)
               (unless entries
                 (setf (gethash key tails) cell))))
            (:last
             (%append-to-predicate-index-tail! key entry index tails)))))))
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
