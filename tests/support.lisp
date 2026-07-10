;;;; Test harness and shared fixtures.
;;;;
;;;; The harness stays dependency-free like the library itself.  Most
;;;; tests should use DEFTEST-QUERIES, a table of (QUERY KIND EXPECTED)
;;;; specs, and only fall back to raw DEFTEST for behavior that is not a
;;;; query/expectation pair.

(defpackage #:fx.prolog.tests
  (:use #:cl #:fx.prolog)
  (:export #:run-tests
           #:deftest
           #:deftest-queries
           #:is
           #:is-equal
           #:is-same-set
           #:signals-error
           #:make-family-rulebase
           #:with-clean-global-rulebase))

(in-package #:fx.prolog.tests)

;;; Runner

(defvar *tests* '())
(defvar *assertion-count* 0)

(defmacro deftest (name () &body body)
  `(push (list ',name (lambda () ,@body)) *tests*))

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
  (loop for cursor = value then (cdr cursor)
        do (cond
             ((null cursor) (return t))
             ((consp cursor))
             (t (return nil)))))

(defun %canonical-form (value)
  (cond
    ((%proper-list-p value)
     (sort (mapcar #'%canonical-form value) #'string< :key #'prin1-to-string))
    ((consp value)
     (cons (%canonical-form (car value)) (%canonical-form (cdr value))))
    (t value)))

(defmacro is-same-set (expected form &optional (message (format nil "Sets differ for ~S" form)))
  `(is-equal (%canonical-form ,expected) (%canonical-form ,form) ,message))

(defmacro signals-error (form &optional (message "Expected an error"))
  `(progn
     (incf *assertion-count*)
     (handler-case
         (progn ,form (error "~A: ~S" ,message ',form))
       (error () t))))

(defun %run-test (name thunk)
  (handler-case
      (progn
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
      (if (%run-test (first test) (second test))
          (incf passed)
          (incf failed)))
    (format t "~%~D tests, ~D assertions, ~D failures~%"
            (+ passed failed) *assertion-count* failed)
    (when (plusp failed)
      (error "Test suite failed with ~D failing test~:P." failed))))

;;; Query expectation tables

(defun %query-spec-assertion (spec)
  "Compile one (QUERY KIND EXPECTED... OPTIONS...) spec into an assertion form."
  (destructuring-bind (query kind &optional expected &rest options) spec
    (let ((run `(query-prolog %rulebase ',query ,@options)))
      (ecase kind
        (=>        `(is-equal ',expected ,run))
        (:set      `(is-same-set ',expected ,run))
        (:first    `(is-equal ',expected (query-prolog-first %rulebase ',query)))
        (:succeeds `(is (prolog-succeeds-p %rulebase ',query)))
        (:fails    `(is (not (prolog-succeeds-p %rulebase ',query))))
        (:signals  `(signals-error ,run))))))

(defmacro deftest-queries (name (rulebase-form) &body specs)
  "Define a test that checks each query expectation SPEC against RULEBASE-FORM.

SPEC is (QUERY KIND EXPECTED... OPTIONS...):
  (Q => SOLUTIONS)    ordered solution list equality
  (Q :set SOLUTIONS)  order-insensitive solution equality
  (Q :first SOLUTION) first solution equality
  (Q :succeeds)       provable
  (Q :fails)          not provable
  (Q :signals)        signals an error
Trailing OPTIONS (e.g. :limit 2) are passed to QUERY-PROLOG."
  `(deftest ,name ()
     (let ((%rulebase ,rulebase-form))
       (declare (ignorable %rulebase))
       ,@(mapcar #'%query-spec-assertion specs))))

;;; Fixtures

(defvar *recorded-colors* '())

(defmethod predicate-true-p ((predicate (eql 'record-color)) args bindings)
  (push (logic-substitute (first args) bindings) *recorded-colors*)
  t)

(defmethod predicate-true-p ((predicate (eql 'accept-ok)) args bindings)
  (equal (mapcar (lambda (arg) (logic-substitute arg bindings)) args)
         '(:ok)))

(define-rulebase *macro-rulebase*
  ((parent tom bob))
  ((parent bob alice))
  ((ancestor ?x ?y) (parent ?x ?y))
  ((ancestor ?x ?y) (parent ?x ?z) (ancestor ?z ?y)))

(defun make-family-rulebase ()
  (prolog
    ((parent tom bob))
    ((parent bob alice))
    ((parent alice eve))
    ((choice left))
    ((choice right))
    ((colored ?x) (parent ?x bob) (record-color ?x))
    ((accepted ?x) (accept-ok ?x))
    ((adult ?x) (parent ?x ?y) (:when (symbolp ?y)))
    ((ancestor ?x ?y) (parent ?x ?y))
    ((ancestor ?x ?y) (parent ?x ?z) (ancestor ?z ?y))
    ((grandparent ?x ?z) (parent ?x ?y) (parent ?y ?z))))

(defmacro with-clean-global-rulebase (&body body)
  `(let ((*global-rulebase* (make-empty-rulebase))
         (*recorded-colors* '()))
     ,@body))

