(in-package #:cl-prolog)

(defun rulebase-extend (rulebase clauses)
  "Return a detached copy of RULEBASE shadow-extended by CLAUSES.

CLAUSES retain their order and precede the clauses already visible in
RULEBASE.  Operator declarations, predicate properties, I/O state, modules,
source registrations, flags, and character conversions are copied as well."
  (let ((extended (copy-rulebase rulebase)))
    (dolist (clause (reverse clauses) extended)
      (rulebase-insert-clause! extended clause :position :first))))

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
