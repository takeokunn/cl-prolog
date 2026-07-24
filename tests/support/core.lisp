;;;; Test runner and table-driven assertions.

(in-package #:cl-prolog.tests)

(defparameter *test-timeout-seconds* 10)

(defmacro deftest (name (&key (timeout '*test-timeout-seconds*)) &body body)
  "Register NAME as a cl-weave test while preserving the suite's old DSL."
  `(cl-weave:it ,(string-downcase (symbol-name name))
     (:timeout-ms (* 1000 ,timeout))
     (cl-weave:expect-has-assertions)
     ,@body))

(defmacro is (form &optional message)
  (declare (ignore message))
  `(cl-weave:expect ,form :to-be-truthy))

(defmacro is-equal (expected form &optional message)
  (declare (ignore message))
  (let ((expected-value (gensym "EXPECTED"))
        (actual-value (gensym "ACTUAL")))
    `(let ((,expected-value ,expected)
           (,actual-value ,form))
       (cl-weave:expect ,actual-value :to-equal ,expected-value))))

(defun %proper-list-p (value)
  (loop with slow = value
        with fast = value
        do (cond
             ((null fast) (return t))
             ((atom fast) (return nil))
             ((null (cdr fast)) (return t))
             ((atom (cdr fast)) (return nil))
             (t
              (setf slow (cdr slow)
                    fast (cddr fast))
              (when (eq slow fast)
                (return nil))))))

(defun %canonical-form (value)
  (cond
    ((%proper-list-p value)
     (sort (mapcar #'%canonical-form value) #'string< :key #'prin1-to-string))
    ((consp value)
     (cons (%canonical-form (car value)) (%canonical-form (cdr value))))
    (t value)))

(defun %tree-equal-p (left right &optional (seen (make-hash-table :test #'eq)))
  "Return true when LEFT and RIGHT are structurally equal, tolerating cycles."
  (cond
    ((eq left right) t)
    ((and (consp left) (consp right))
     (let ((right-nodes (gethash left seen)))
       (cond
         ((member right right-nodes :test #'eq) t)
         (t
          (push right (gethash left seen))
          (and (%tree-equal-p (car left) (car right) seen)
               (%tree-equal-p (cdr left) (cdr right) seen))))))
    ((or (consp left) (consp right)) nil)
    (t (equal left right))))

(defun %tree-contains-p (tree target)
  "Return true when TARGET appears anywhere in TREE, tolerating cycles."
  (let ((seen (make-hash-table :test #'eq))
        (stack (list tree)))
    (loop while stack
          for node = (pop stack)
          do (cond
               ((if (consp target)
                    (%tree-equal-p node target)
                    (equal node target))
                (return t))
               ((and (consp node) (not (gethash node seen)))
                (setf (gethash node seen) t)
                (push (cdr node) stack)
                (push (car node) stack)))
          finally (return nil))))

(defun exported-symbol-p (name package-name)
  "Return true when NAME is exported from PACKAGE-NAME."
  (eq :external (nth-value 1 (find-symbol name package-name))))

(defun normalize-error-data (term)
  "Normalize nested Prolog error payloads for structural comparison."
  (typecase term
    (null nil)
    (symbol (symbol-name term))
    (cons (mapcar #'normalize-error-data term))
    (t term)))

(defun %read-prolog-query (rulebase format-control &rest format-arguments)
  "Format FORMAT-CONTROL with FORMAT-ARGUMENTS (if any) and read the result
as a Prolog query term using RULEBASE's operator table."
  (read-prolog-term (apply #'format nil format-control format-arguments)
                     (cl-prolog::rulebase-operator-table rulebase)))

(defun query-error-summary (rulebase goal &key with-data)
  "Run GOAL against RULEBASE, returning NIL on success or the signalled
condition's type on a Prolog runtime error.  WITH-DATA also returns the
condition's normalized ISO error-term payload as a second list element."
  (handler-case
      (progn (query-prolog rulebase goal) nil)
    (prolog-runtime-error (condition)
      (if with-data
          (list (type-of condition)
                (normalize-error-data (second (prolog-exception-term condition))))
          (type-of condition)))))

(defun %symbol-export-assertion (name package-name exported-p)
  "Compile a package export assertion for NAME in PACKAGE-NAME."
  (let ((message (if exported-p
                     (format nil "Expected exported symbol ~A" name)
                     (format nil "Legacy symbol ~A must not be exported" name))))
    `(is ,(if exported-p
              `(exported-symbol-p ,name ,package-name)
              `(not (exported-symbol-p ,name ,package-name)))
         ,message)))

(defmacro is-same-set (expected form &optional (message (format nil "Sets differ for ~S" form)))
  `(is-equal (%canonical-form ,expected) (%canonical-form ,form) ,message))

(defmacro signals-error (form &optional message)
  (declare (ignore message))
  `(cl-weave:expect (lambda () ,form) :to-throw))

(defmacro with-closed-io-context ((context) &body body)
  "Run BODY, then close every stream CONTEXT owns, even if BODY errors."
  `(unwind-protect (progn ,@body)
     (cl-prolog::%close-all-owned-prolog-streams! ,context)))

(defun package-owned-symbol-count (package-designator)
  "Return the number of symbols PACKAGE-DESIGNATOR's package owns (interns
directly, excluding symbols merely inherited via :USE) -- the before/after
invariant interning-avoidance security tests check around a rejected or
speculative identifier, to assert it was never interned as a side effect."
  (let ((package (find-package package-designator)))
    (loop for symbol being each symbol of package
          count (eq package (symbol-package symbol)))))

(defun %table-spec-assertion (spec)
  "Compile one table SPEC into an assertion form."
  (destructuring-bind (kind &rest arguments) spec
    (ecase kind
      (:is
       `(is ,@arguments))
      (:is-not
       (destructuring-bind (form &optional message) arguments
         `(is (not ,form) ,(or message (format nil "Assertion unexpectedly succeeded: ~S" form)))))
      (:equal
       `(is-equal ,@arguments))
      (:same-set
       `(is-same-set ,@arguments))
      (:signals
       `(signals-error ,@arguments))
      (:exported
       (destructuring-bind (name package-name) arguments
         (%symbol-export-assertion name package-name t)))
      (:not-exported
       (destructuring-bind (name package-name) arguments
         (%symbol-export-assertion name package-name nil))))))

(defun %unification-spec-assertion (spec)
  "Compile one unification SPEC into an assertion form."
  (destructuring-bind (kind left right &rest options) spec
    (let* ((initial-env (getf options :initial-env))
           (expected (getf options :expected))
           (unify-form `(unify ',left ',right ,@(when initial-env (list `',initial-env)))))
      (ecase kind
        (:substitute
         `(multiple-value-bind (env ok)
              ,unify-form
            (is ok)
            (is-equal ',expected (logic-substitute ',left env))))
        (:fails
         `(multiple-value-bind (env ok)
              ,unify-form
            (is (not ok))
            (is (null env))))
        (:ok
         `(is (nth-value 1 ,unify-form)))
        (:not-ok
         `(is (not (nth-value 1 ,unify-form))))))))

(defmacro %deftest-table-from-specs (name specs compiler)
  "Define NAME as a cl-weave test with one case per SPEC, compiled via COMPILER."
  `(cl-weave:describe-sequential ,(string-downcase (symbol-name name))
     ,@(loop for spec in specs
             for index from 1
             collect `(cl-weave:it ,(format nil "case ~D: ~S" index spec)
                        (cl-weave:expect-has-assertions)
                        ,(funcall (symbol-function compiler) spec)))))

(defmacro deftest-table (name () &body specs)
  "Define NAME as a test composed entirely from table SPECS.

Supported spec forms:
  (:is FORM [MESSAGE])
  (:is-not FORM [MESSAGE])
  (:equal EXPECTED FORM [MESSAGE])
  (:same-set EXPECTED FORM [MESSAGE])
  (:signals FORM [MESSAGE])
  (:exported NAME PACKAGE-NAME)
  (:not-exported NAME PACKAGE-NAME)"
  `(%deftest-table-from-specs ,name ,specs %table-spec-assertion))

(defmacro deftest-io-variants (name ((rulebase input output) input-text) &body cases)
  "Define NAME as an IO test with one case per current/explicit variant.

Each CASE is (LABEL &body BODY).  The LABEL is descriptive only; BODY is run
inside a fresh WITH-IO-RULEBASE setup."
  `(deftest ,name ()
     ,@(mapcar (lambda (case)
                 (destructuring-bind (label &rest body) case
                   (declare (ignore label))
                   `(with-io-rulebase (,rulebase ,input ,output) ,input-text
                      ,@body)))
               cases)))

(defmacro deftest-unification (name &body specs)
  "Define NAME as a test composed from unification table SPECS.

Supported spec forms:
  (:substitute LEFT RIGHT :expected EXPECTED [:initial-env ENV])
  (:fails LEFT RIGHT [:initial-env ENV])
  (:ok LEFT RIGHT [:initial-env ENV])
  (:not-ok LEFT RIGHT [:initial-env ENV])"
  `(%deftest-table-from-specs ,name ,specs %unification-spec-assertion))

(defun %tree-target-form (target)
  "Compile TARGET syntax into a form producing the fragment to search for."
  (cond
    ((atom target) `',target)
    ((eq (first target) 'quote) target)
    (t `',target)))

(defmacro deftest-tree-contains (name (tree-form) &body targets)
  "Define NAME as a test that asserts TREE-FORM contains each TARGET."
  `(cl-weave:describe-sequential ,(string-downcase (symbol-name name))
     ,@(loop for target in targets
             for index from 1
             collect `(cl-weave:it ,(format nil "target ~D: ~S" index target)
                        (cl-weave:expect-has-assertions)
                        (let ((tree ,tree-form))
                          (is (%tree-contains-p tree ,(%tree-target-form target))))))))

(defmacro with-macroexpansion ((name form) &body body)
  "Bind NAME to the single-step macro expansion of FORM and run BODY."
  `(let ((,name (macroexpand-1 ,form)))
     ,@body))
