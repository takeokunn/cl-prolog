;;;; Test runner and table-driven assertions.

(in-package #:cl-prolog.tests)

(defvar *tests* '())
(defvar *assertion-count* 0)
(defparameter *test-timeout-seconds* 10)

(defmacro deftest (name (&key (timeout '*test-timeout-seconds*)) &body body)
  `(push (list :name ',name
               :timeout ,timeout
               :thunk (lambda () ,@body))
         *tests*))

(defmacro is (form &optional (message (format nil "Assertion failed: ~S" form)))
  `(progn
     (incf *assertion-count*)
     (unless ,form
       (error "~A" ,message))))

(defmacro is-equal (expected form &optional (message (format nil "Values differ for ~S" form)))
  (let ((expected-value (gensym "EXPECTED"))
        (actual-value (gensym "ACTUAL")))
    `(let ((,expected-value ,expected)
           (,actual-value ,form))
       (incf *assertion-count*)
       (unless (equal ,expected-value ,actual-value)
         (error "~A~%expected: ~S~%actual:   ~S"
                ,message ,expected-value ,actual-value)))))

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

(defmacro signals-error (form &optional (message "Expected an error"))
  `(progn
     (incf *assertion-count*)
     (let ((signaled-p nil))
       (handler-case
           ,form
         (error ()
           (setf signaled-p t)))
       (unless signaled-p
         (error "~A: ~S" ,message ',form))
       t)))

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
  `(deftest ,name ()
     ,@(mapcar #'%table-spec-assertion specs)))

(defmacro deftest-unification (name &body specs)
  "Define NAME as a test composed from unification table SPECS.

Supported spec forms:
  (:substitute LEFT RIGHT :expected EXPECTED [:initial-env ENV])
  (:fails LEFT RIGHT [:initial-env ENV])
  (:ok LEFT RIGHT [:initial-env ENV])
  (:not-ok LEFT RIGHT [:initial-env ENV])"
  `(deftest ,name ()
     ,@(mapcar #'%unification-spec-assertion specs)))

(defun %tree-target-form (target)
  "Compile TARGET syntax into a form producing the fragment to search for."
  (cond
    ((atom target) `',target)
    ((eq (first target) 'quote) target)
    (t `',target)))

(defmacro deftest-tree-contains (name (tree-form) &body targets)
  "Define NAME as a test that asserts TREE-FORM contains each TARGET."
  `(deftest ,name ()
     (let ((tree ,tree-form))
       ,@(mapcar (lambda (target)
                   `(is (%tree-contains-p tree ,(%tree-target-form target))))
                 targets))))

(defmacro with-macroexpansion ((name form) &body body)
  "Bind NAME to the single-step macro expansion of FORM and run BODY."
  `(let ((,name (macroexpand-1 ,form)))
     ,@body))

(defun %run-test (name thunk timeout)
  (handler-case
      (progn
        (format t "running ~A~%" name)
        (finish-output)
        #+sbcl
        (sb-ext:with-timeout timeout
          (funcall thunk))
        #-sbcl
        (funcall thunk)
        (format t "ok ~A~%" name)
        (finish-output)
        t)
    (error (condition)
      (format t "not ok ~A~%~A~%" name condition)
      (finish-output)
      nil)))

(defun run-tests ()
  (let ((passed 0)
        (failed 0))
    (dolist (test (reverse *tests*))
      (if (%run-test (getf test :name)
                     (getf test :thunk)
                     (getf test :timeout *test-timeout-seconds*))
          (incf passed)
        (incf failed)))
    (format t "~%~D tests, ~D assertions, ~D failures~%"
            (+ passed failed) *assertion-count* failed)
    (finish-output)
    (when (plusp failed)
      (error "Test suite failed with ~D failing test~:P." failed))))
