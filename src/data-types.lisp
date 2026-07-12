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

(defstruct (%source-record (:constructor %make-source-record (state)))
  "Artifacts owned by one canonical source file."
  (state :loading :type (member :loading :loaded))
  (operators '() :type list)
  (predicate-properties '() :type list)
  (table-declarations '() :type list))

(defstruct (%table-entry (:copier nil)
                         (:constructor %make-table-entry ()))
  "Variant-call answers accumulated during one tabled proof."
  (answers '() :type list))

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
