(in-package #:cl-prolog)

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
