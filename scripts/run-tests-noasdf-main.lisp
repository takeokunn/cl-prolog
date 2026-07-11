(defpackage #:cl-prolog.run-tests-noasdf
  (:use #:cl)
  (:export #:exit-script
           #:main))

(in-package #:cl-prolog.run-tests-noasdf)

(defun exit-script (code)
  (finish-output *standard-output*)
  (finish-output *error-output*)
  (sb-ext:exit :code code))

(defun ensure-bootstrap-loaded ()
  (unless (find-package "CL-PROLOG.BOOTSTRAP")
    (load (merge-pathnames "bootstrap.lisp"
                           (or *load-truename*
                               *load-pathname*
                               (error "Cannot determine script path from *LOAD-TRUENAME* or *LOAD-PATHNAME*."))))))

(defun call-bootstrap (symbol-name &rest arguments)
  (ensure-bootstrap-loaded)
  (let ((symbol (find-symbol symbol-name "CL-PROLOG.BOOTSTRAP")))
    (unless symbol
      (error "Bootstrap symbol ~A is missing." symbol-name))
    (apply (symbol-function symbol) arguments)))

(defun run-tests ()
  (call-bootstrap "LOAD-CORE-SOURCES")
  (call-bootstrap "LOAD-TEST-SOURCES")
  (let ((runner (find-symbol "RUN-TESTS" "FX.PROLOG.TESTS")))
    (unless runner
      (error "Cannot resolve FX.PROLOG.TESTS::RUN-TESTS after loading test suites."))
    (funcall (symbol-function runner))))

(defun main ()
  (run-tests)
  0)
