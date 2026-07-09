(in-package #:fx.prolog)

(defparameter *max-prolog-depth* 64)

(defun %copy-rulebase (rulebase)
  (make-rulebase :facts (copy-list (rulebase-facts rulebase))
                 :rules (copy-list (rulebase-rules rulebase))))

(defun %query-vars (term &optional (seen (make-hash-table :test #'eq)) (order '()))
  (labels ((walk (x)
             (cond
               ((logic-var-p x)
                (unless (gethash x seen)
                  (setf (gethash x seen) t
                        order (nconc order (list x)))))
               ((consp x)
                (walk (car x))
                (walk (cdr x))))))
    (walk term)
    order))

(defun %project-solution (query env)
  (let ((vars (%query-vars query)))
    (mapcar (lambda (var)
              (cons var (logic-substitute var env)))
            vars)))

(defun %conjunction-form-p (query)
  (and (consp query)
       (or (consp (car query))
           (eq (car query) '!))))

(defun %normalize-query (query)
  (cond
    ((null query) '())
    ((%conjunction-form-p query) query)
    (t (list query))))

(defun %quoted-form-p (form)
  (and (consp form)
       (eq (car form) 'quote)
       (consp (cdr form))
       (null (cddr form))))

(defun %self-evaluating-symbol-p (value)
  (or (keywordp value) (member value '(t nil) :test #'eq)))

(defun %subst-for-eval-walk (form env)
  (cond
    ((logic-var-p form)
     (let ((value (logic-substitute form env)))
       (cond
         ((logic-var-p value) value)
         ((and (symbolp value) (not (%self-evaluating-symbol-p value)))
          `(quote ,value))
         (t value))))
    ((%quoted-form-p form) form)
    ((consp form)
     (cons (%subst-for-eval-walk (car form) env)
           (%subst-for-eval-walk (cdr form) env)))
    ((and (symbolp form) (not (%self-evaluating-symbol-p form)))
     `(quote ,form))
    (t form)))

(defun %eval-lisp-condition (condition env)
  (handler-case
      (let ((prepared (%subst-for-eval-walk condition env)))
        (if (consp prepared)
            (eval prepared)
            prepared))
    (error () nil)))

(defgeneric predicate-true-p (predicate args bindings)
  (:documentation "Return true when PREDICATE succeeds in the current environment."))

(defmethod predicate-true-p ((predicate symbol) args bindings)
  (declare (ignore predicate args bindings))
  nil)

(defun %fact->goal (fact)
  (cond
    ((fact-p fact)
     (cons (fact-predicate fact) (fact-args fact)))
    ((consp fact) fact)
    ((symbolp fact) (list fact))
    (t fact)))

(defun %fact-matches-predicate-p (fact-goal predicate)
  (and (consp fact-goal)
       (eql predicate (car fact-goal))))

(defun %dcg-token-kind (token)
  (if (consp token) (car token) token))

(defun %dcg-token-value (token)
  (when (consp token) (cdr token)))

(defun %dcg-token-match (expected input rest env)
  (let ((input (logic-substitute input env)))
    (when (consp input)
      (multiple-value-bind (env1 kind-ok)
          (unify expected (%dcg-token-kind (car input)) env)
        (when kind-ok
          (multiple-value-bind (env2 rest-ok)
              (unify rest (cdr input) env1)
            (when rest-ok
              (list env2))))))))

(defun %dcg-token-match-value (expected-kind expected-value input rest env)
  (let ((input (logic-substitute input env)))
    (when (consp input)
      (multiple-value-bind (env1 kind-ok)
          (unify expected-kind (%dcg-token-kind (car input)) env)
        (when kind-ok
          (multiple-value-bind (env2 value-ok)
              (unify expected-value (%dcg-token-value (car input)) env1)
            (when value-ok
              (multiple-value-bind (env3 rest-ok)
                  (unify rest (cdr input) env2)
                (when rest-ok
                  (list env3))))))))))

(defun %builtin-solve (predicate args kb env depth)
  (case predicate
     (= (multiple-value-bind (next ok)
            (unify (first args) (second args) env)
          (when ok
            (list next))))
    ((!= /=) (multiple-value-bind (next ok)
                 (unify (first args) (second args) env)
               (declare (ignore next))
               (unless ok
                 (list env))))
    (not (multiple-value-bind (solutions cutp)
             (%solve-goals kb (%normalize-query (first args)) env (1- depth))
           (declare (ignore cutp))
           (unless solutions
             (list env))))
    (:when (when (%eval-lisp-condition (first args) env)
             (list env)))
    (and (%solve-goals kb args env depth))
    (or (let ((results '()))
          (dolist (alt args (nreverse results))
            (multiple-value-bind (solutions alt-cut)
                (%solve-goals kb (%normalize-query alt) env depth)
              (setf results (nconc results solutions))
              (when alt-cut
                (return (nreverse results)))))))
    (dcg-token-match
     (%dcg-token-match (first args) (second args) (third args) env))
    (dcg-token-match-value
     (%dcg-token-match-value (first args) (second args) (third args) (fourth args) env))
     (! :cut)
     (t nil)))

(defun %rename-term (term env)
  (cond
    ((logic-var-p term)
     (or (gethash term env)
         (setf (gethash term env)
               (gensym "?R"))))
    ((consp term)
     (cons (%rename-term (car term) env)
           (%rename-term (cdr term) env)))
    (t term)))

(defun %rename-rule (rule)
  (let ((env (make-hash-table :test #'eq)))
    (make-rule :head (%rename-term (rule-head rule) env)
               :body (mapcar (lambda (goal) (%rename-term goal env))
                             (rule-body rule)))))

(defun %prove-body (kb goals env depth)
  (cond
    ((null goals) (values (list env) nil))
    (t
     (multiple-value-bind (first-sols first-cut)
         (%prove-goal kb (first goals) env depth)
       (let ((results '())
             (cutp first-cut))
         (dolist (solution first-sols)
           (multiple-value-bind (rest-sols rest-cut)
               (%prove-body kb (rest goals) solution depth)
             (setf results (nconc results rest-sols)
                   cutp (or cutp rest-cut))
             (when rest-cut
               (return))))
         (values results cutp))))))

(defun %prove-rule (kb rule goal env depth)
  (let* ((renamed (%rename-rule rule))
         (head (rule-head renamed))
         (body (rule-body renamed))
         (next-env nil)
         (ok nil))
    (multiple-value-setq (next-env ok)
      (unify goal head env))
    (if ok
        (%prove-body kb body next-env (1- depth))
        (values nil nil))))

(defun %prove-predicate (kb goal env depth)
  (let ((predicate (car goal))
        (results '())
        (cutp nil))
    (when (predicate-true-p predicate (cdr goal) env)
      (push env results))
    (dolist (fact (rulebase-facts kb))
      (let ((fact-goal (%fact->goal fact)))
        (when (and (symbolp predicate)
                   (%fact-matches-predicate-p fact-goal predicate))
          (multiple-value-bind (next ok)
              (unify goal fact-goal env)
            (when ok
              (push next results))))))
    (when (plusp depth)
      (dolist (rule (rulebase-rules kb))
        (when (and (consp (rule-head rule))
                   (eql predicate (car (rule-head rule))))
          (multiple-value-bind (solutions rule-cut)
              (%prove-rule kb rule goal env depth)
            (setf results (nconc solutions results)
                  cutp (or cutp rule-cut))
            (when rule-cut
              (return))))))
    (values (nreverse results) cutp)))

(defun %prove-goal (kb goal env depth)
  (cond
    ((eq goal '!) (values (list env) t))
    ((and (consp goal)
          (symbolp (car goal))
          (member (car goal) '(= != /= not :when and or ! dcg-token-match dcg-token-match-value) :test #'eq))
     (let ((result (%builtin-solve (car goal) (cdr goal) kb env depth)))
       (if (eq result :cut)
           (values (list env) t)
           (values result nil))))
    ((and (consp goal) (symbolp (car goal)))
     (%prove-predicate kb goal env depth))
    ((symbolp goal)
     (%prove-predicate kb (list goal) env depth))
    (t (values nil nil))))

(defun %solve-goals (kb goals env depth)
  (if (null goals)
      (values (list env) nil)
      (multiple-value-bind (first-sols first-cut)
          (%prove-goal kb (first goals) env depth)
        (let ((results '())
              (cutp first-cut))
          (dolist (solution first-sols)
            (multiple-value-bind (rest-sols rest-cut)
                (%solve-goals kb (rest goals) solution depth)
              (setf results (nconc results rest-sols)
                    cutp (or cutp rest-cut))
              (when rest-cut
                (return))))
          (values results cutp)))))

(defun %search-query (kb query &optional (max-depth *max-prolog-depth*))
  (%solve-goals kb (%normalize-query query) '() max-depth))

(defun prove (kb goal &optional (bindings '()) (max-depth *max-prolog-depth*))
  (declare (ignore bindings))
  (query-prolog kb goal :max-depth max-depth))

(defun prove-all (kb goal &key (max-depth *max-prolog-depth*))
  (query-prolog kb goal :max-depth max-depth))

(defun %query-substitutions (rulebase query max-depth)
  (multiple-value-bind (solutions cutp)
      (%search-query rulebase query max-depth)
    (declare (ignore cutp))
    (mapcar (lambda (solution)
              (logic-substitute query solution))
            solutions)))

(defun query-prolog (rulebase query &key (max-depth *max-prolog-depth*))
  (multiple-value-bind (solutions cutp)
      (%search-query rulebase query max-depth)
    (declare (ignore cutp))
    (mapcar (lambda (solution)
              (%project-solution query solution))
            solutions)))

(defun query-prolog-first (rulebase query &key (max-depth *max-prolog-depth*))
  (first (query-prolog rulebase query :max-depth max-depth)))

(defun prolog-succeeds-p (rulebase query &key (max-depth *max-prolog-depth*))
  (not (null (query-prolog rulebase query :max-depth max-depth))))

(defun merge-rulebase-facts (rulebase facts)
  (make-rulebase :facts (append facts (rulebase-facts rulebase))
                 :rules (copy-list (rulebase-rules rulebase))))

(defun query-prolog-cps (rulebase query continue &rest keyargs)
  (funcall continue (apply #'query-prolog rulebase query keyargs)))

(defun prolog-succeeds-p-cps (rulebase query continue &rest keyargs)
  (funcall continue (apply #'prolog-succeeds-p rulebase query keyargs)))

(defun merge-rulebase-facts-cps (rulebase facts continue)
  (funcall continue (merge-rulebase-facts rulebase facts)))

(defun %fact-spec->form (spec)
  `(make-fact :predicate ',(first spec)
              :args ',(rest spec)))

(defun %rule-spec->form (spec)
  `(make-rule :head ',(first spec)
              :body ',(rest spec)))

(defun %parse-rulebase-sections (sections)
  (let ((facts '())
        (rules '()))
    (dolist (section sections)
      (case (first section)
        (:facts (setf facts (append facts (mapcar #'%fact-spec->form (rest section)))))
        (:rules (setf rules (append rules (mapcar #'%rule-spec->form (rest section)))))
        (otherwise (error "Unknown rulebase section ~S" (first section)))))
    (values facts rules)))

(defmacro define-rulebase (name &body sections)
  (multiple-value-bind (facts rules)
      (%parse-rulebase-sections sections)
    `(defparameter ,name
       (make-rulebase :facts (list ,@facts)
                      :rules (list ,@rules)))))

(defmacro extend-rulebase (base &body sections)
  (multiple-value-bind (facts rules)
      (%parse-rulebase-sections sections)
    `(make-rulebase
      :facts (append (list ,@facts) (rulebase-facts ,base))
      :rules (append (list ,@rules) (rulebase-rules ,base)))))

(defmacro with-prolog-query (binding-vars (rulebase query &key (max-depth '*max-prolog-depth*)) &body body)
  (let ((solutions-var (gensym "SOLUTIONS"))
        (solution-var (gensym "SOLUTION")))
    `(let ((,solutions-var (query-prolog ,rulebase ,query :max-depth ,max-depth)))
       (when ,solutions-var
         (let ((,solution-var (first ,solutions-var)))
           (let ,(mapcar (lambda (var)
                           `(,var (cdr (assoc ',var ,solution-var))))
                         binding-vars)
             ,@body))))))

(defmacro prolog-match (rulebase &body clauses)
  `(cond
     ,@(mapcar (lambda (clause)
                 `((prolog-succeeds-p ,rulebase ,(first clause))
                   ,@(rest clause)))
               clauses)))

(defun %prolog-rule-index-rules ()
  (let ((rules '()))
    (maphash (lambda (predicate predicate-rules)
               (declare (ignore predicate))
               (setf rules (append predicate-rules rules)))
             *prolog-rules*)
    rules))

(defun %global-rulebase-view ()
  (make-rulebase :facts (rulebase-facts *global-rulebase*)
                 :rules (append (%prolog-rule-index-rules)
                                (rulebase-rules *global-rulebase*))))

(defun query-all (goal)
  (%query-substitutions (%global-rulebase-view) goal *max-prolog-depth*))

(defmacro def-rule (head &body body)
  `(eval-when (:load-toplevel :execute)
     (register-prolog-rule ',head ',body)
     ',head))
