;;;; Expression/branch coverage for the core suite via sb-cover.
;;;;
;;;; Compiles src/ with coverage instrumentation, runs the core test
;;;; suites (scripts-contract tests excluded), writes an HTML report to
;;;; coverage/, and prints a per-file summary line to stdout.
;;;;
;;;; Usage: sbcl --script scripts/coverage.lisp

#+sbcl (setf (sb-ext:bytes-consed-between-gcs) (* 2048 1024 1024))

(require :sb-cover)

(defparameter *repo-root*
  (let ((here (or *load-truename*
                  (error "Cannot determine script path from *LOAD-TRUENAME*."))))
    (make-pathname :name nil :type nil :version nil
                   :directory (butlast (pathname-directory here))
                   :defaults here)))

(defparameter *source-files*
  '("src/package.lisp"
    "src/data.lisp"
    "src/unification.lisp"
    "src/engine.lisp"
    "src/builtins.lisp"
    "src/dcg-runtime.lisp"
    "src/query.lisp"
    "src/dsl.lisp"
    "src/dcg.lisp"))

(defparameter *test-files*
  '("tests/support.lisp"
    "tests/core.lisp"
    "tests/engine.lisp"
    "tests/dcg.lisp"))

(declaim (optimize sb-cover:store-coverage-data))

(let ((fasl-directory (merge-pathnames "coverage/fasl/" *repo-root*)))
  (ensure-directories-exist fasl-directory)
  (dolist (file *source-files*)
    (let ((source (merge-pathnames file *repo-root*)))
      (format t "~&;; instrumenting ~A~%" file)
      (finish-output)
      (load (compile-file source
                          :output-file (merge-pathnames
                                        (format nil "~A.fasl" (pathname-name source))
                                        fasl-directory)
                          :verbose nil
                          :print nil)))))

(declaim (optimize (sb-cover:store-coverage-data 0)))

(dolist (file *test-files*)
  (format t "~&;; loading ~A~%" file)
  (finish-output)
  (load (merge-pathnames file *repo-root*)))

(funcall (symbol-function (find-symbol "RUN-TESTS" "FX.PROLOG.TESTS")))

(let ((report-directory (merge-pathnames "coverage/" *repo-root*)))
  (sb-cover:report report-directory)
  (format t "~&;; HTML report: ~Acover-index.html~%" (namestring report-directory)))
