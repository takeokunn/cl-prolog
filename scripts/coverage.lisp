;;;; Expression/branch coverage for the core suite via sb-cover.
;;;;
;;;; Compiles src/ with coverage instrumentation, runs the core test
;;;; suites (scripts-contract tests excluded), writes an HTML report to
;;;; coverage/, and prints a per-file summary line to stdout.
;;;;
;;;; Usage: sbcl --script scripts/coverage.lisp

(require :sb-cover)

(defparameter *command-timeouts*
  '(("compile" . 45)
    ("report" . 120)))

(defun command-timeout (kind)
  (or (cdr (assoc kind *command-timeouts* :test #'string=))
      15))

(defparameter *compile-timeout* (command-timeout "compile"))
(defparameter *report-timeout* (command-timeout "report"))

(defun script-path ()
  (or *load-truename*
      *load-pathname*
      (error "Cannot determine script path from *LOAD-TRUENAME* or *LOAD-PATHNAME*.")))

(defun bootstrap-path ()
  (make-pathname :name nil
                 :type nil
                 :version nil
                 :directory (butlast (pathname-directory (script-path)))
                 :defaults (script-path)))

(load (merge-pathnames "scripts/bootstrap.lisp" (bootstrap-path)))

(defun print-version (&optional (stream *standard-output*))
  (format stream "cl-prolog ~A~%"
          (cl-prolog.bootstrap:project-version)))

(defun usage (&optional (stream *standard-output*))
  (format stream "Usage: sbcl --script scripts/coverage.lisp~%")
  (format stream "       sbcl --script scripts/coverage.lisp --help~%")
  (format stream "       sbcl --script scripts/coverage.lisp --version~%")
  (format stream "~%")
  (format stream "Writes an sb-cover HTML report to coverage/cover-index.html.~%")
  (format stream "Runs only the core suites; script-contract tests stay out of this gate.~%"))

(defun compile-command-arguments (source-file output)
  (list "-MPOSIX=:sys_wait_h"
        "-e"
        (cl-prolog.bootstrap:perl-timeout-wrapper *compile-timeout*)
        (write-to-string *compile-timeout*)
        (cl-prolog.bootstrap:sbcl-program)
        "--disable-debugger"
        "--script"
        (namestring (cl-prolog.bootstrap:repo-file "scripts/coverage-compile-file.lisp"))
        source-file
        (enough-namestring output (cl-prolog.bootstrap:repo-root))))

(defun coverage-output-file (source-file)
  (merge-pathnames
   (format nil "~A.fasl"
           (pathname-name (pathname source-file)))
   (cl-prolog.bootstrap:repo-file "coverage/fasl/")))

(defun compile-covered-file! (source-file dependencies)
  (let ((output (coverage-output-file source-file)))
    (format t "~&;; instrumenting ~A~%" source-file)
    (finish-output)
    (ensure-directories-exist output)
      (let ((exit-code
             (cl-prolog.bootstrap:run-command-stream
              (cl-prolog.bootstrap:perl-program)
              (append (compile-command-arguments source-file output)
                      dependencies)
              :timeout *compile-timeout*)))
      (unless (zerop exit-code)
        (error "Command failed (coverage compile): ~A" source-file)))
    output))

(defun parse-args ()
  (let ((args sb-ext:*posix-argv*))
    (dolist (arg (cdr args))
      (cond
        ((member arg '("--help" "-h") :test #'string=)
         (usage)
         (sb-ext:exit :code 0))
        ((string= arg "--version")
         (print-version)
         (sb-ext:exit :code 0))
        (t
         (format *error-output* "Unknown argument: ~A~%~%" arg)
         (usage *error-output*)
         (sb-ext:exit :code 2))))))

(declaim (optimize sb-cover:store-coverage-data))

(parse-args)

(let ((dependencies '())
      (outputs '()))
  (dolist (file (cl-prolog.bootstrap:core-source-files))
    (push (compile-covered-file! file (reverse dependencies)) outputs)
    (push file dependencies))
  (dolist (output (nreverse outputs))
    (load output)))

(declaim (optimize (sb-cover:store-coverage-data 0)))

(cl-prolog.bootstrap:load-test-sources)

(funcall (symbol-function (find-symbol "RUN-TESTS" "CL-PROLOG.TESTS")))

(defun report-coverage (report-directory)
  #+sbcl
  (handler-case
      (sb-ext:with-timeout *report-timeout*
        (sb-cover:report report-directory))
    (sb-ext:timeout ()
      (format *error-output* "~&;; coverage report timed out after ~A seconds~%"
              *report-timeout*)))
  #-sbcl
  (sb-cover:report report-directory))

(let ((report-directory (cl-prolog.bootstrap:repo-file "coverage/")))
  (report-coverage report-directory)
  (format t "~&;; HTML report: ~Acover-index.html~%" (namestring report-directory))
  (sb-ext:exit :code 0))
