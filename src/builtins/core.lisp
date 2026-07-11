;;;; Builtin goal solvers.
;;;;
;;;; Every builtin is declared with DEFINE-BUILTIN and follows the engine's
;;;; CPS contract: emit each solution environment as it is found, never
;;;; collect intermediate result lists.

(in-package #:cl-prolog)

(defun %unify-emit (left right environment emit)
  "EMIT the extension of ENVIRONMENT that unifies LEFT with RIGHT, if any."
  (multiple-value-bind (extended ok)
      (unify left right environment)
    (when ok
      (funcall emit extended))))

(defun %proper-list-p (value)
  "True when VALUE is a finite proper list."
  (loop with seen = (make-hash-table :test #'eq)
        for tail = value then (cdr tail)
        do (cond
             ((null tail) (return t))
             ((atom tail) (return nil))
             ((gethash tail seen) (return nil))
             (t (setf (gethash tail seen) t)))))

(defun %entry-head (clause)
  (clause-head clause))

(defun %entry-body-term (clause)
  (if (null (clause-body clause))
      'true
      (let ((body (clause-body clause)))
        (cond
          ((null body) 'true)
          ((null (rest body)) (first body))
          (t (cons 'and body))))))

(defun %freshen-dynamic-clause (clause)
  (%freshen-clause clause))

(defun %builtin-predicate-p (predicate arity)
  (and (symbolp predicate) (%goal-solver predicate arity)))

(defun %rulebase-defines-predicate-p (rulebase predicate arity module)
  (some (lambda (stored)
          (multiple-value-bind (entry-predicate entry-arity)
              (%entry-predicate-arity (%stored-clause-clause stored))
            (and (eq predicate entry-predicate) (= arity entry-arity))))
        (%rulebase-module-entries rulebase module)))

(defun %ensure-dynamic-predicate (rulebase predicate arity goal environment
                                  &key (operation "MODIFY")
                                    (permission-type "STATIC_PROCEDURE"))
  "Reject modification or inspection of a static PREDICATE/ARITY."
  (when (or (%builtin-predicate-p predicate arity)
            (eq :static (%rulebase-predicate-property
                         rulebase predicate arity *current-prolog-module*))
            (and (null (%rulebase-predicate-property
                        rulebase predicate arity *current-prolog-module*))
                 (%rulebase-defines-predicate-p
                  rulebase predicate arity *current-prolog-module*)))
    (%raise-permission-error
     operation permission-type (list '/ predicate arity) environment
     (first goal) "static procedures cannot be inspected or modified")))

(defun %ensure-callable (term environment operation)
  "Return TERM when it is callable, otherwise raise its ISO error."
  (when (logic-var-p term)
    (%raise-instantiation-error environment operation
                                "callable term must be instantiated"))
  (unless (or (symbolp term)
              (and (%proper-list-p term) (%goal-form-p term)))
    (%raise-type-error "CALLABLE" term environment operation
                       "expected a callable term"))
  (%ensure-goal-form term))

(defun %dynamic-clause-head (term environment operation)
  "Validate TERM as a fact or rule and return its callable head."
  (when (logic-var-p term)
    (%raise-instantiation-error environment operation
                                "dynamic clause must be instantiated"))
  (unless (or (symbolp term) (%proper-list-p term))
    (%raise-type-error "CALLABLE" term environment operation
                       "dynamic clause must be a proper callable term"))
  (if (and (consp term) (eq (first term) ':-))
      (progn
        (unless (consp (rest term))
          (%raise-type-error "CALLABLE" term environment operation
                             "rule must contain a callable head"))
        (%ensure-callable (second term) environment operation))
      (%ensure-callable term environment operation)))

;;;; ISO Prolog flags

(defstruct (prolog-flag (:constructor %make-prolog-flag (name default-value
                                                         allowed-values)))
  name
  default-value
  allowed-values)

(defmacro define-prolog-flags (&body specifications)
  "Define the implementation's flag data independently from builtin logic."
  `(defparameter *prolog-flag-specifications*
     (list ,@(loop for (name value allowed-values) in specifications
                   collect `(%make-prolog-flag
                              ,(string-upcase (symbol-name name))
                              ,value ',allowed-values)))))

(define-prolog-flags
  (bounded "FALSE" ())
  (max-arity "UNBOUNDED" ())
  (integer-rounding-function "DOWN" ())
  (char-conversion "OFF" ("ON" "OFF"))
  (debug "OFF" ("ON" "OFF"))
  (unknown "ERROR" ("ERROR" "FAIL" "WARNING"))
  (double-quotes "CODES" ("CODES" "CHARS" "ATOM")))

(defun %prolog-flag-name (term environment operation)
  (let ((resolved (logic-substitute term environment)))
    (when (logic-var-p resolved)
      (%raise-instantiation-error environment operation
                                  "flag name must be instantiated"))
    (unless (symbolp resolved)
      (%raise-type-error "ATOM" resolved environment operation
                         "flag name must be an atom"))
    (string-upcase (symbol-name resolved))))

(defun %find-prolog-flag (name)
  (find name *prolog-flag-specifications*
        :key #'prolog-flag-name :test #'string=))

(defun %prolog-flag-value (rulebase flag)
  "Return FLAG's rulebase-local value, installing its immutable default lazily."
  (let* ((name (prolog-flag-name flag))
         (values (rulebase-prolog-flag-values rulebase)))
    (multiple-value-bind (value present-p) (gethash name values)
      (if present-p
          value
          (setf (gethash name values) (prolog-flag-default-value flag))))))

(defun %external-prolog-flag-value (value)
  (if (stringp value) (%iso-atom value) value))

(defun %resolve-prolog-flag-value (term environment operation)
  (let ((resolved (logic-substitute term environment)))
    (when (logic-var-p resolved)
      (%raise-instantiation-error environment operation
                                  "flag value must be instantiated"))
    (if (symbolp resolved)
        (string-upcase (symbol-name resolved))
        resolved)))

(define-builtin (current_prolog_flag name value)
    (rulebase environment depth emit)
  (let ((resolved-name (logic-substitute name environment)))
    (if (logic-var-p resolved-name)
        (dolist (flag *prolog-flag-specifications*)
          (%unify-emit
           name (%iso-atom (prolog-flag-name flag)) environment
           (lambda (named-environment)
             (%unify-emit value
                          (%external-prolog-flag-value
                          (%prolog-flag-value rulebase flag))
                          named-environment emit))))
        (let ((flag (%find-prolog-flag
                     (%prolog-flag-name name environment
                                        (%iso-atom "CURRENT_PROLOG_FLAG")))))
          (when flag
            (%unify-emit value
                         (%external-prolog-flag-value
                          (%prolog-flag-value rulebase flag))
                         environment emit))))))

(define-builtin (set_prolog_flag name value)
    (rulebase environment depth emit)
  (let* ((operation (%iso-atom "SET_PROLOG_FLAG"))
         (flag-name (%prolog-flag-name name environment operation))
         (flag (%find-prolog-flag flag-name)))
    (unless flag
      (%raise-domain-error "PROLOG_FLAG" (logic-substitute name environment)
                           environment operation "unknown Prolog flag"))
    (unless (prolog-flag-allowed-values flag)
      (%raise-permission-error "MODIFY" "FLAG" (%iso-atom flag-name)
                               environment operation
                               "implementation-defined flag is read-only"))
    (let ((new-value (%resolve-prolog-flag-value value environment operation)))
      (unless (member new-value (prolog-flag-allowed-values flag) :test #'equal)
        (%raise-domain-error "FLAG_VALUE" (logic-substitute value environment)
                             environment operation "invalid Prolog flag value"))
      (setf (gethash flag-name (rulebase-prolog-flag-values rulebase)) new-value)
      (funcall emit environment))))

(defun %clause-term-entry (term rulebase goal environment)
  "Convert a substituted dynamic clause TERM to a freshly renamed entry."
  (cond
    ((and (%proper-list-p term) (eq (first term) ':-))
     (unless (and (consp (rest term))
                  (%goal-form-p (second term)))
       (%invalid-goal goal "a rule must have shape (:- (PREDICATE . ARGS) GOAL...)"))
     (%ensure-dynamic-predicate rulebase (first (second term))
                                (length (rest (second term))) goal environment)
     (let ((body (cddr term)))
       (unless (every #'%goal-form-p (mapcar #'%ensure-goal-form body))
         (%invalid-goal goal "every rule body element must be a callable goal"))
       (%freshen-clause (make-clause (second term) body))))
    ((or (symbolp term) (%goal-form-p term))
     (let ((normalized (%ensure-goal-form term)))
       (%ensure-dynamic-predicate rulebase (first normalized)
                                  (length (rest normalized)) goal environment)
       (let ((table (make-hash-table :test #'eq)))
         (make-clause (%freshen-term normalized table)))))
    (t
     (%invalid-goal goal "a dynamic clause must be a fact or (:- HEAD BODY...)"))))

(defun %entry-predicate-arity (entry)
  (let ((head (%entry-head entry)))
    (values (first head) (length (rest head)))))
