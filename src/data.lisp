;;;; Ordered clause data model and operations.

(in-package #:cl-prolog)

(defstruct (clause (:copier nil)
                   (:constructor make-clause (head &optional (body '()))))
  "A Horn clause. An empty BODY denotes a fact."
  (head '() :type list :read-only t)
  (body '() :type list :read-only t))

(defstruct (%stored-clause (:copier nil)
                           (:constructor %make-stored-clause
                               (clause born-revision)))
  "Internal lifetime metadata for one clause in a rulebase."
  (clause (make-clause '()) :type clause :read-only t)
  (born-revision 0 :type (integer 0 *) :read-only t)
  (died-revision nil :type (or null (integer 0 *))))

(defstruct (rulebase (:copier nil)
                     (:constructor %make-rulebase (entries revision)))
  "An ordered logical-update database of clauses."
  (entries '() :type list)
  (revision 0 :type (integer 0 *)))

(defun make-rulebase (&key (clauses '()))
  "Return a rulebase containing CLAUSES in resolution order."
  (%make-rulebase
   (mapcar (lambda (clause) (%make-stored-clause clause 0)) clauses)
   0))

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

(defun rulebase-insert-clause! (rulebase clause &key (position :last))
  "Insert CLAUSE at POSITION (:FIRST or :LAST) and return RULEBASE."
  (let ((entry (%make-stored-clause clause (%next-rulebase-revision! rulebase))))
    (ecase position
      (:first (push entry (rulebase-entries rulebase)))
      (:last (setf (rulebase-entries rulebase)
                   (append (rulebase-entries rulebase) (list entry))))))
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
