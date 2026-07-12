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
         (head (%dynamic-clause-head pattern environment 'retract))
         (normalized-pattern (if (symbolp pattern) head pattern)))
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
              (unify normalized-pattern stored environment)
            (when (and ok (%rulebase-retract-entry! rulebase entry))
              (funcall emit extended)))))))))

(define-builtin (retractall clause) (rulebase environment depth emit)
  (let* ((goal (list 'retractall clause))
         (pattern (logic-substitute clause environment))
         (head (%dynamic-clause-head pattern environment 'retractall))
         (normalized-pattern (if (symbolp pattern) head pattern)))
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
                (unify normalized-pattern stored environment)
              (declare (cl:ignore extended))
              (when ok (push entry matches))))))
        (%rulebase-retract-entries! rulebase matches)))
    (funcall emit environment)))

(define-builtin (current_predicate indicator) (rulebase environment depth emit)
  (let ((resolved (logic-substitute indicator environment))
        (seen (make-hash-table :test #'equal))
        (indicators '()))
    (labels ((remember (predicate arity)
               (let ((candidate (list '/ predicate arity)))
                 (unless (gethash candidate seen)
                   (setf (gethash candidate seen) t)
                   (push candidate indicators)))))
      (dolist (stored (%rulebase-module-entries
                       rulebase *current-prolog-module*))
        (multiple-value-bind (predicate arity)
            (%entry-predicate-arity (%stored-clause-clause stored))
          (remember predicate arity)))
      (dolist (candidate
               (%rulebase-declared-predicate-indicators
                rulebase *current-prolog-module*))
        (remember (second candidate) (third candidate)))
      (dolist (candidate (%builtin-predicate-indicators))
        (remember (second candidate) (third candidate)))
      (dolist (candidate (%foreign-predicate-indicators))
        (remember (second candidate) (third candidate))))
    (unless (logic-var-p resolved)
      (unless (and (%proper-list-p resolved)
                   (= (length resolved) 3)
                   (eq (first resolved) '/)
                   (or (symbolp (second resolved))
                       (logic-var-p (second resolved))))
        (%raise-type-error "PREDICATE_INDICATOR" resolved
                           environment 'current_predicate
                           "expected a predicate indicator"))
      (unless (or (integerp (third resolved))
                  (logic-var-p (third resolved)))
        (%raise-type-error "INTEGER" (third resolved)
                           environment 'current_predicate
                           "predicate arity must be an integer"))
      (when (and (integerp (third resolved))
                 (minusp (third resolved)))
        (%raise-domain-error "NOT_LESS_THAN_ZERO" (third resolved)
                             environment 'current_predicate
                             "predicate arity cannot be negative")))
    (if (or (logic-var-p resolved)
            (%term-has-variables-p resolved))
        (dolist (candidate (nreverse indicators))
          (%unify-emit indicator candidate environment emit))
        (progn
          ;; Dispatch also covers valid arities of variadic builtins, which
          ;; cannot all be represented by the finite enumeration above.
          (when (or (gethash resolved seen)
                    (%builtin-predicate-p (second resolved) (third resolved)))
            (funcall emit environment))))))

(defun %predicate-properties (rulebase predicate arity module)
  "Return the supported reflection properties for PREDICATE/ARITY."
  (cond
    ((%builtin-predicate-p predicate arity)
     '(built_in))
    (t
     (let ((declared (%rulebase-predicate-property
                      rulebase predicate arity module))
           (defined (%rulebase-defines-predicate-p
                     rulebase predicate arity module)))
       (when (or declared defined)
         (list (if (eq declared :dynamic) 'dynamic 'static)
               'user))))))

(define-builtin (predicate_property head property)
    (rulebase environment depth emit)
  (let* ((resolved-head (logic-substitute head environment))
         (resolved-property (logic-substitute property environment))
         (callable (%ensure-callable resolved-head environment
                                     'predicate_property))
         (predicate (first callable))
         (arity (length (rest callable))))
    (unless (or (logic-var-p resolved-property)
                (symbolp resolved-property))
      (%raise-type-error "ATOM" resolved-property environment
                         'predicate_property
                         "predicate property must be an atom"))
    (dolist (candidate
             (%predicate-properties rulebase predicate arity
                                    *current-prolog-module*))
      (%unify-emit property candidate environment emit))))

(defun %current-module-names (rulebase)
  "Return registered module names in a deterministic reflection order."
  (let ((names '()))
    (maphash (lambda (name module)
               (declare (ignore module))
               (unless (eq name +default-prolog-module+)
                 (push name names)))
             (module-registry-modules
              (rulebase-module-registry rulebase)))
    (cons +default-prolog-module+
          (sort names #'string<
                :key (lambda (name)
                       (format nil "~A/~A"
                               (or (and (symbol-package name)
                                        (package-name (symbol-package name)))
                                   "")
                               (symbol-name name)))))))

(define-builtin (current_module module) (rulebase environment depth emit)
  (let ((resolved (logic-substitute module environment)))
    (unless (or (logic-var-p resolved) (symbolp resolved))
      (%raise-type-error "ATOM" resolved environment 'current_module
                         "module name must be an atom"))
    (if (logic-var-p resolved)
        (dolist (candidate (%current-module-names rulebase))
          (%unify-emit module candidate environment emit))
        (when (gethash resolved
                       (module-registry-modules
                        (rulebase-module-registry rulebase)))
          (funcall emit environment)))))

(define-builtin (abolish indicator) (rulebase environment depth emit)
  (let* ((goal (list 'abolish indicator))
         (resolved (logic-substitute indicator environment)))
    (when (logic-var-p resolved)
      (%raise-instantiation-error environment 'abolish
                                  "predicate indicator must be instantiated"))
    (unless (and (%proper-list-p resolved)
                 (= (length resolved) 3)
                 (eq (first resolved) '/)
                 (symbolp (second resolved)))
      (%raise-type-error "PREDICATE_INDICATOR" resolved environment 'abolish
                         "expected a predicate indicator"))
    (unless (integerp (third resolved))
      (%raise-type-error "INTEGER" (third resolved) environment 'abolish
                         "predicate arity must be an integer"))
    (when (minusp (third resolved))
      (%raise-domain-error "NOT_LESS_THAN_ZERO" (third resolved)
                           environment 'abolish
                           "predicate arity cannot be negative"))
    (destructuring-bind (slash predicate arity) resolved
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
            (unify callable (%entry-head fresh) environment)
          (when head-ok
            (%unify-emit body (%entry-body-term fresh) head-environment emit)))))))
