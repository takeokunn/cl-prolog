;;;; Per-rulebase registry of loaded source units: the %source-record
;;;; struct tracking one source's load state and rollback effects, and
;;;; the registry table holding one record per canonical source pathname.

(in-package #:cl-prolog)

(defstruct (%source-record (:constructor %make-source-record (state)))
  "Artifacts owned by one canonical source file."
  (state :loading :type (member :loading :loaded))
  (operators '() :type list)
  (predicate-properties '() :type list)
  (table-declarations '() :type list))

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
