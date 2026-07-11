;;;; Release audit data and shared helpers.

(defpackage #:cl-prolog.release-audit
  (:use #:cl)
  (:export #:exit-script
           #:main))

(in-package #:cl-prolog.release-audit)

(defparameter *results* nil)
(defparameter *requested-checks* nil)
(defparameter *dry-run* nil)
(defparameter *output-format* "text")
(defparameter *include-script-contracts* nil)
(defparameter *default-benchmark-iterations* 100)
(defparameter *git-command-timeout* 10)
(defparameter *source-snapshot-release-artifacts*
  '("README.md"
    "cl-prolog.asd"
    "contracts/public-contract.sexp"
    "docs/api-reference.md"
    "docs/architecture.md"
    "docs/oss-readiness-audit.md"
    "docs/performance.md"
    "docs/public-contract-verifier.md"
    "docs/quality-gates.md"
    "docs/release-audit.md"
    "docs/release-checklist.md"
    "docs/troubleshooting.md"
    "examples/quick-start.lisp"
    "examples/family-tree.lisp"
    "examples/relational-lists.lisp"
    "scripts/benchmark.lisp"
    "scripts/benchmark-main.lisp"
    "scripts/benchmark-support.lisp"
    "scripts/bootstrap.lisp"
    "scripts/coverage.lisp"
    "scripts/release-audit.lisp"
    "scripts/release-audit-main.lisp"
    "scripts/verify-public-contract.lisp"
    "scripts/verify-public-contract-main.lisp"))

(defstruct audit-check
  command
  timeout
  runner)

(defvar *audit-checks* (make-hash-table :test #'equal))

(defmacro define-audit-check (name command-spec timeout runner)
  `(setf (gethash ,name *audit-checks*)
         (make-audit-check :command (lambda ()
                                      (nth-value 0 ,command-spec))
                           :timeout ,timeout
                           :runner ',runner)))

(defun exit-script (code)
  (finish-output *standard-output*)
  (finish-output *error-output*)
  (sb-ext:exit :code code))

(defun script-path ()
  (or *load-truename*
      *load-pathname*
      (error "Cannot determine script path from *LOAD-TRUENAME* or *LOAD-PATHNAME*.")))

(defun repo-root ()
  (uiop:ensure-directory-pathname
   (merge-pathnames "../" (uiop:pathname-directory-pathname (script-path)))))

(defun repo-file (relative-path)
  (merge-pathnames relative-path (repo-root)))

(load (repo-file "scripts/bootstrap.lisp"))

(defun argv ()
  (uiop:command-line-arguments))

(defun project-version ()
  (cl-prolog.bootstrap:project-version))

(defun print-version (&optional (stream *standard-output*))
  (format stream "cl-prolog ~A~%" (project-version)))

(defun usage (&optional (stream *standard-output*))
  (format stream "Usage: sbcl --script scripts/release-audit.lisp [--dry-run] [--json] [--with-nix] [--with-benchmarks] [--with-script-contracts]~%")
  (format stream "       sbcl --script scripts/release-audit.lisp --help~%")
  (format stream "       sbcl --script scripts/release-audit.lisp --version~%")
  (format stream "~%")
  (format stream "Checks:~%")
  (format stream "  core         Public-contract verifier plus tracked release artifacts.~%")
  (format stream "  tests        Regression suite via tests.lisp.~%")
  (format stream "  nix          nix flake check.~%")
  (format stream "  benchmarks   Benchmark smoke in JSON mode.~%")
  (format stream "~%")
  (format stream "Options:~%")
  (format stream "  --dry-run          Print the commands that would run.~%")
  (format stream "  --json             Emit machine-readable output.~%")
  (format stream "  --with-nix         Include nix flake check.~%")
  (format stream "  --with-benchmarks  Include benchmark smoke.~%")
  (format stream "  --with-script-contracts  Run tests.lisp with CL_PROLOG_TEST_SCRIPTS=1.~%")
  (format stream "  --help, -h         Show this help and exit successfully.~%")
  (format stream "  --version          Print the bundled cl-prolog version and exit successfully.~%")
  (format stream "~%")
  (format stream "Examples:~%")
  (format stream "  sbcl --script scripts/release-audit.lisp~%")
  (format stream "  sbcl --script scripts/release-audit.lisp --with-nix --with-benchmarks --with-script-contracts --json~%"))

(defun usage-error (control &rest args)
  (apply #'format *error-output* control args)
  (format *error-output* "~%~%")
  (usage *error-output*)
  (exit-script 2))

(defun parse-args (args)
  (let ((requested-checks (list "core" "tests"))
        (dry-run nil)
        (output-format "text"))
    (loop while args
          for arg = (pop args)
          do (cond
               ((string= arg "--dry-run")
                (setf dry-run t))
               ((string= arg "--json")
                (setf output-format "json"))
               ((string= arg "--with-nix")
                (pushnew "nix" requested-checks :test #'string=))
               ((string= arg "--with-benchmarks")
                (pushnew "benchmarks" requested-checks :test #'string=))
               ((string= arg "--with-script-contracts")
                (setf *include-script-contracts* t))
               ((member arg '("--help" "-h") :test #'string=)
                (usage)
                (exit-script 0))
               ((string= arg "--version")
                (print-version)
                (exit-script 0))
               (t
                (usage-error "Unknown argument: ~A" arg))))
    (list :requested-checks (nreverse requested-checks)
          :dry-run dry-run
          :output-format output-format)))

(defun run-command (program arguments &key timeout)
  (cl-prolog.bootstrap:run-command-capture
   program
   arguments
   :timeout timeout
   :directory (repo-root)))

(defun core-command-spec ()
  (values "sbcl --script scripts/verify-public-contract.lisp --json"
          (cl-prolog.bootstrap:sbcl-program)
          (list "--script"
                "scripts/verify-public-contract.lisp"
                "--json")))

(defun tests-command-spec ()
  (if *include-script-contracts*
      (values "env CL_PROLOG_TEST_SCRIPTS=1 sbcl --script tests.lisp"
              "env"
              (list "CL_PROLOG_TEST_SCRIPTS=1"
                    (cl-prolog.bootstrap:sbcl-program)
                    "--script"
                    "tests.lisp"))
      (values "sbcl --script tests.lisp"
              (cl-prolog.bootstrap:sbcl-program)
              (list "--script" "tests.lisp"))))

(defun nix-command-spec ()
  (values "nix flake check"
          "nix"
          (list "flake" "check")))

(defun benchmark-command-spec ()
  (values (format nil "sbcl --script scripts/benchmark.lisp --json --iterations ~D"
                  *default-benchmark-iterations*)
          (cl-prolog.bootstrap:sbcl-program)
          (list "--script"
                "scripts/benchmark.lisp"
                "--json"
                "--iterations"
                (write-to-string *default-benchmark-iterations*))))

(defun audit-check-for (check)
  (or (gethash check *audit-checks*)
      (error "Unknown check: ~A" check)))

(defun check-command (check)
  (funcall (audit-check-command (audit-check-for check))))

(defun check-timeout (check)
  (audit-check-timeout (audit-check-for check)))

(defmacro with-command-result ((command result command-spec timeout) &body body)
  `(multiple-value-bind (,command program arguments) ,command-spec
     (let* ((,result (run-command program
                                  arguments
                                  :timeout ,timeout))
            (stdout (result-output-string ,result))
            (stderr (result-error-string ,result)))
       (declare (ignorable stdout stderr))
       ,@body)))

(defun result-output-string (process-info)
  (or (getf process-info :output) ""))

(defun result-error-string (process-info)
  (or (getf process-info :error-output) ""))

(defun result-exit-code (process-info)
  (or (getf process-info :exit-code) 1))

(defun check-result (status check command message &optional details)
  (push (list :status status
              :check check
              :command command
              :message message
              :details details)
        *results*)
  (when (string= *output-format* "text")
    (format t "[~A] ~A: ~A~%"
            (string-upcase (symbol-name status))
            check
            message)
    (when (and details (not (string= details "")))
      (format t "~A~%" details))))

(defun verify-command-result (check command result success-message failure-message)
  (let ((stdout (result-output-string result))
        (stderr (result-error-string result)))
    (if (zerop (result-exit-code result))
        (check-result :pass check command success-message stdout)
        (check-result :fail check command failure-message
                      (format nil "stdout:~%~A~%stderr:~%~A" stdout stderr)))))

(defun final-exit-code ()
  (if (some (lambda (result)
              (eq (getf result :status) :fail))
            *results*)
      1
      0))
