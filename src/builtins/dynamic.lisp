(in-package #:cl-prolog)

(define-builtin (asserta clause) (rulebase environment depth emit)
  (let* ((goal (list 'asserta clause))
         (entry (%clause-term-entry (logic-substitute clause environment)
                                    rulebase goal environment)))
    (multiple-value-bind (predicate arity) (%entry-predicate-arity entry)
      (%set-rulebase-predicate-property!
       rulebase predicate arity :dynamic *current-prolog-module*))
    (rulebase-insert-clause! rulebase entry :position :first
                             :module *current-prolog-module*)
    (funcall emit environment)))

(define-builtin (assertz clause) (rulebase environment depth emit)
  (let* ((goal (list 'assertz clause))
         (entry (%clause-term-entry (logic-substitute clause environment)
                                    rulebase goal environment)))
    (multiple-value-bind (predicate arity) (%entry-predicate-arity entry)
      (%set-rulebase-predicate-property!
       rulebase predicate arity :dynamic *current-prolog-module*))
    (rulebase-insert-clause! rulebase entry :position :last
                             :module *current-prolog-module*)
    (funcall emit environment)))

(define-builtin (retract clause) (rulebase environment depth emit)
  (let* ((goal (list 'retract clause))
         (pattern (logic-substitute clause environment))
         (head (%dynamic-clause-head pattern environment 'retract)))
    (%ensure-dynamic-predicate rulebase (first head) (length (rest head)) goal environment)
    (multiple-value-bind (snapshot entries) (%rulebase-snapshot rulebase)
      (declare (cl:ignore snapshot))
      (dolist (entry entries)
        (when (eq (%stored-clause-module entry) *current-prolog-module*)
        (let* ((clause-entry (%stored-clause-clause entry))
               (fresh (%freshen-dynamic-clause clause-entry))
             (stored (if (null (clause-body fresh))
                          (%entry-head fresh)
                          (list* ':- (%entry-head fresh)
                                 (clause-body fresh)))))
          (multiple-value-bind (extended ok)
              (unify clause stored environment)
            (when (and ok (%rulebase-retract-entry! rulebase entry))
              (funcall emit extended)))))))))

(define-builtin (retractall clause) (rulebase environment depth emit)
  (let* ((goal (list 'retractall clause))
         (pattern (logic-substitute clause environment))
         (head (%dynamic-clause-head pattern environment 'retractall)))
    (%ensure-dynamic-predicate rulebase (first head) (length (rest head)) goal environment)
    (multiple-value-bind (snapshot entries) (%rulebase-snapshot rulebase)
      (declare (cl:ignore snapshot))
      (let ((matches '()))
        (dolist (entry entries)
          (when (eq (%stored-clause-module entry) *current-prolog-module*)
          (let* ((fresh (%freshen-dynamic-clause (%stored-clause-clause entry)))
             (stored (if (null (clause-body fresh))
                          (%entry-head fresh)
                          (list* ':- (%entry-head fresh)
                                 (clause-body fresh)))))
            (multiple-value-bind (extended ok)
                (unify pattern stored environment)
              (declare (cl:ignore extended))
              (when ok (push entry matches))))))
        (%rulebase-retract-entries! rulebase matches)))
    (funcall emit environment)))

(define-builtin (current-predicate indicator) (rulebase environment depth emit)
  (let ((resolved (logic-substitute indicator environment))
        (seen (make-hash-table :test #'equal))
        (indicators '()))
    (labels ((remember (candidate)
               (unless (gethash candidate seen)
                 (setf (gethash candidate seen) t)
                 (push candidate indicators))))
      (dolist (stored (%rulebase-module-entries
                       rulebase *current-prolog-module*))
        (multiple-value-bind (predicate arity)
            (%entry-predicate-arity (%stored-clause-clause stored))
          (remember (list predicate '/ arity))))
      (dolist (candidate
               (%rulebase-declared-predicate-indicators
                rulebase *current-prolog-module*))
        (remember candidate))
      (dolist (candidate (%builtin-predicate-indicators))
        (remember candidate))
      (dolist (candidate (%foreign-predicate-indicators))
        (remember candidate)))
    (unless (and (%proper-list-p resolved)
                 (= (length resolved) 3)
                 (or (symbolp (first resolved))
                     (logic-var-p (first resolved)))
                 (eq (second resolved) '/))
      (%raise-type-error "PREDICATE_INDICATOR" resolved
                         environment 'current-predicate
                         "expected a predicate indicator"))
    (unless (or (integerp (third resolved))
                (logic-var-p (third resolved)))
      (%raise-type-error "INTEGER" (third resolved)
                         environment 'current-predicate
                         "predicate arity must be an integer"))
    (when (and (integerp (third resolved))
               (minusp (third resolved)))
      (%raise-domain-error "NOT_LESS_THAN_ZERO" (third resolved)
                           environment 'current-predicate
                           "predicate arity cannot be negative"))
    (if (%term-has-variables-p resolved)
        (dolist (candidate (nreverse indicators))
          (%unify-emit indicator candidate environment emit))
        (progn
          ;; Dispatch also covers valid arities of variadic builtins, which
          ;; cannot all be represented by the finite enumeration above.
          (when (or (gethash resolved seen)
                    (%builtin-predicate-p (first resolved) (third resolved)))
            (funcall emit environment))))))

(define-builtin (abolish indicator) (rulebase environment depth emit)
  (let* ((goal (list 'abolish indicator))
         (resolved (logic-substitute indicator environment)))
    (when (logic-var-p resolved)
      (%raise-instantiation-error environment 'abolish
                                  "predicate indicator must be instantiated"))
    (unless (and (%proper-list-p resolved)
                 (= (length resolved) 3)
                 (symbolp (first resolved))
                 (eq (second resolved) '/))
      (%raise-type-error "PREDICATE_INDICATOR" resolved environment 'abolish
                         "expected a predicate indicator"))
    (unless (integerp (third resolved))
      (%raise-type-error "INTEGER" (third resolved) environment 'abolish
                         "predicate arity must be an integer"))
    (when (minusp (third resolved))
      (%raise-domain-error "NOT_LESS_THAN_ZERO" (third resolved)
                           environment 'abolish
                           "predicate arity cannot be negative"))
    (destructuring-bind (predicate slash arity) resolved
      (declare (cl:ignore slash))
      (%ensure-dynamic-predicate rulebase predicate arity goal environment)
      (multiple-value-bind (snapshot entries) (%rulebase-snapshot rulebase)
        (declare (cl:ignore snapshot))
        (%rulebase-retract-entries!
         rulebase
         (remove-if-not
          (lambda (stored)
            (multiple-value-bind (entry-predicate entry-arity)
                (%entry-predicate-arity (%stored-clause-clause stored))
              (and (eq (%stored-clause-module stored) *current-prolog-module*)
                   (eq predicate entry-predicate) (= arity entry-arity))))
          entries)))
      (%remove-rulebase-predicate-property!
       rulebase predicate arity *current-prolog-module*))
    (funcall emit environment)))

(define-builtin (clause head body) (rulebase environment depth emit)
  (let* ((resolved-head (logic-substitute head environment))
         (callable (%ensure-callable resolved-head environment 'clause)))
    (%ensure-dynamic-predicate rulebase (first callable)
                               (length (rest callable))
                               (list 'clause head body) environment
                               :operation "ACCESS"
                               :permission-type "PRIVATE_PROCEDURE")
    (dolist (entry (%rulebase-module-entries
                    rulebase *current-prolog-module*))
      (let ((fresh (%freshen-dynamic-clause (%stored-clause-clause entry))))
        (multiple-value-bind (head-environment head-ok)
            (unify head (%entry-head fresh) environment)
          (when head-ok
            (%unify-emit body (%entry-body-term fresh) head-environment emit)))))))
