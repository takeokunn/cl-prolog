;;;; Load the system and core test suites without touching ASDF.
;;;;
;;;; Developer entry point that mirrors the component order of
;;;; cl-prolog.asd using plain LOAD.  Useful on machines where ASDF
;;;; operations misbehave and as a fast dependency-free smoke run.
;;;; Scripts-contract tests (tests/scripts.lisp) are excluded because
;;;; they spawn nested SBCL images.
;;;;
;;;; Usage: sbcl --script scripts/run-tests-noasdf.lisp

;; A large nursery keeps this short-lived process out of the collector,
;; which also sidesteps a GC-timing hang seen with some SBCL builds.
#+sbcl (setf (sb-ext:bytes-consed-between-gcs) (* 2048 1024 1024))

(defparameter *repo-root*
  (let ((here (or *load-truename*
                  (error "Cannot determine script path from *LOAD-TRUENAME*."))))
    (make-pathname :name nil :type nil :version nil
                   :directory (butlast (pathname-directory here))
                   :defaults here)))

(dolist (file '("src/package.lisp"
                "src/data.lisp"
                "src/unification.lisp"
                "src/engine.lisp"
                "src/builtins.lisp"
                "src/dcg-runtime.lisp"
                "src/query.lisp"
                "src/dsl.lisp"
                "src/dcg.lisp"
                "tests/support.lisp"
                "tests/core.lisp"
                "tests/engine.lisp"
                "tests/dcg.lisp"))
  (format t "~&;; loading ~A~%" file)
  (finish-output)
  (load (merge-pathnames file *repo-root*)))

(let ((runner (find-symbol "RUN-TESTS" "FX.PROLOG.TESTS")))
  (unless runner
    (error "Cannot resolve FX.PROLOG.TESTS::RUN-TESTS after loading test suites."))
  (funcall (symbol-function runner)))
