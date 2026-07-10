#!/usr/bin/env sbcl --script

(require :asdf)

(defpackage #:cl-prolog.release-audit
  (:use #:cl))

(in-package #:cl-prolog.release-audit)

(defparameter *results* nil)
(defparameter *requested-checks* nil)
(defparameter *dry-run* nil)
(defparameter *output-format* "text")
(defparameter *default-benchmark-iterations* 100)
(defparameter *command-timeouts*
  '(("git" . 10)
    ("public-contract" . 600)
    ("tests" . 900)
    ("benchmark" . 60)
    ("nix" . 1200)))
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
    "scripts/release-audit.lisp"
    "scripts/verify-public-contract.lisp"))

(defun script-path ()
  (or *load-truename*
      (error "Cannot determine script path from *LOAD-TRUENAME*.")))

(defun repo-root ()
  (uiop:ensure-directory-pathname
   (merge-pathnames "../" (uiop:pathname-directory-pathname (script-path)))))

(defun repo-file (relative-path)
  (merge-pathnames relative-path (repo-root)))

(asdf:load-asd (repo-file "cl-prolog.asd"))

(defun argv ()
  (uiop:command-line-arguments))

(defun sbcl-program ()
  (or (uiop:getenv "SBCL")
      "sbcl"))

(defun perl-program ()
  (or (uiop:getenv "PERL")
      "perl"))

(defun command-timeout (kind)
  (or (cdr (assoc kind *command-timeouts* :test #'string=))
      15))

(defun project-version ()
  (asdf:component-version (asdf:find-system :cl-prolog)))

(defun print-version (&optional (stream *standard-output*))
  (format stream "cl-prolog ~A~%" (project-version)))

(defun usage (&optional (stream *standard-output*))
  (format stream "Usage: sbcl --script scripts/release-audit.lisp [--dry-run] [--json] [--with-nix] [--with-benchmarks]~%")
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
  (format stream "  --help, -h         Show this help and exit successfully.~%")
  (format stream "  --version          Print the bundled cl-prolog version and exit successfully.~%")
  (format stream "~%")
  (format stream "Examples:~%")
  (format stream "  sbcl --script scripts/release-audit.lisp~%")
  (format stream "  sbcl --script scripts/release-audit.lisp --with-nix --with-benchmarks --json~%"))

(defun usage-error (control &rest args)
  (apply #'format *error-output* control args)
  (format *error-output* "~%~%")
  (usage *error-output*)
  (sb-ext:exit :code 2))

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
               ((member arg '("--help" "-h") :test #'string=)
                (usage)
                (sb-ext:exit :code 0))
               ((string= arg "--version")
                (print-version)
                (sb-ext:exit :code 0))
               (t
                (usage-error "Unknown argument: ~A" arg))))
    (list :requested-checks (nreverse requested-checks)
          :dry-run dry-run
          :output-format output-format)))

(defun json-escaped-string (string)
  (with-output-to-string (out)
    (loop for ch across string
          do (case ch
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Backspace (write-string "\\b" out))
               (#\FormFeed (write-string "\\f" out))
               (#\Newline (write-string "\\n" out))
               (#\Return (write-string "\\r" out))
               (#\Tab (write-string "\\t" out))
               (t
                (if (< (char-code ch) 32)
                    (format out "\\u~4,'0X" (char-code ch))
                    (write-char ch out)))))))

(defun write-json-string (stream string)
  (format stream "\"~A\"" (json-escaped-string string)))

(defun run-command (program arguments &key timeout)
  (handler-case
      (multiple-value-bind (output error-output exit-code)
          (uiop:run-program
           (append (list (namestring (perl-program))
                         "-e"
                         (format nil "alarm ~D; exec @ARGV" (or timeout 15))
                         program)
                   arguments)
                            :directory (repo-root)
                            :output '(:string :stripped nil)
                            :error-output '(:string :stripped nil)
                            :ignore-error-status t)
        (list :output output
              :error-output error-output
              :exit-code exit-code))
    (error (condition)
      (list :output ""
            :error-output (princ-to-string condition)
            :exit-code 1))))

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

(defun planned-check-command (check)
  (cond
    ((string= check "core")
     "sbcl --script scripts/verify-public-contract.lisp --json")
    ((string= check "tests")
     "sbcl --script tests.lisp")
    ((string= check "nix")
     "nix flake check")
    ((string= check "benchmarks")
     (format nil "sbcl --script scripts/benchmark.lisp --json --iterations ~D"
             *default-benchmark-iterations*))
    (t
     (error "Unknown check: ~A" check))))

(defun verify-tracked-release-artifacts ()
  (dolist (path *source-snapshot-release-artifacts*)
    (let* ((result (run-command "git"
                                (list "ls-files" "--error-unmatch" "--" path)
                                :timeout (command-timeout "git")))
           (stdout (result-output-string result))
           (stderr (result-error-string result)))
      (if (zerop (result-exit-code result))
          (check-result :pass
                        (format nil "artifact/~A" path)
                        "git ls-files --error-unmatch"
                        "artifact is tracked"
                        stdout)
          (check-result :fail
                        (format nil "artifact/~A" path)
                        "git ls-files --error-unmatch"
                        "artifact is missing from git"
                        (format nil "stderr:~%~A~%stdout:~%~A" stderr stdout))))))

(defun verify-core ()
  (let* ((command (planned-check-command "core"))
         (result (run-command (sbcl-program)
                              (list "--script"
                                    "scripts/verify-public-contract.lisp"
                                    "--json")
                              :timeout (command-timeout "public-contract")))
         (stdout (result-output-string result))
         (stderr (result-error-string result)))
    (if (zerop (result-exit-code result))
        (check-result :pass "core" command
                      "public-contract verifier passed"
                      stdout)
        (check-result :fail "core" command
                      "public-contract verifier failed"
                      (format nil "stdout:~%~A~%stderr:~%~A" stdout stderr))))
  (verify-tracked-release-artifacts))

(defun verify-tests ()
  (let* ((command (planned-check-command "tests"))
         (result (run-command (sbcl-program)
                              (list "--script" "tests.lisp")
                              :timeout (command-timeout "tests")))
         (stdout (result-output-string result))
         (stderr (result-error-string result)))
    (if (zerop (result-exit-code result))
        (check-result :pass "tests" command
                      "regression suite passed"
                      stdout)
        (check-result :fail "tests" command
                      "regression suite failed"
                      (format nil "stdout:~%~A~%stderr:~%~A" stdout stderr)))))

(defun verify-nix ()
  (let* ((command (planned-check-command "nix"))
         (result (run-command "nix"
                              (list "flake" "check")
                              :timeout (command-timeout "nix")))
         (stdout (result-output-string result))
         (stderr (result-error-string result)))
    (if (zerop (result-exit-code result))
        (check-result :pass "nix" command
                      "nix flake check passed"
                      stdout)
        (check-result :fail "nix" command
                      "nix flake check failed"
                      (format nil "stdout:~%~A~%stderr:~%~A" stdout stderr)))))

(defun verify-benchmarks ()
  (let* ((command (planned-check-command "benchmarks"))
         (result (run-command (sbcl-program)
                              (list "--script"
                                    "scripts/benchmark.lisp"
                                    "--json"
                                    "--iterations"
                                    (write-to-string *default-benchmark-iterations*))
                              :timeout (command-timeout "benchmark")))
         (stdout (result-output-string result))
         (stderr (result-error-string result)))
    (if (zerop (result-exit-code result))
        (check-result :pass "benchmarks" command
                      "benchmark smoke passed"
                      stdout)
        (check-result :fail "benchmarks" command
                      "benchmark smoke failed"
                      (format nil "stdout:~%~A~%stderr:~%~A" stdout stderr)))))

(defun execute-check (check)
  (cond
    ((string= check "core")
     (verify-core))
    ((string= check "tests")
     (verify-tests))
    ((string= check "nix")
     (verify-nix))
    ((string= check "benchmarks")
     (verify-benchmarks))
    (t
     (error "Unknown check: ~A" check))))

(defun final-exit-code ()
  (if (some (lambda (result)
              (eq (getf result :status) :fail))
            *results*)
      1
      0))

(defun write-json-result (stream result)
  (format stream "{")
  (write-json-string stream "status")
  (format stream ":")
  (write-json-string stream (string-downcase (symbol-name (getf result :status))))
  (format stream ",")
  (write-json-string stream "check")
  (format stream ":")
  (write-json-string stream (getf result :check))
  (format stream ",")
  (write-json-string stream "command")
  (format stream ":")
  (write-json-string stream (getf result :command))
  (format stream ",")
  (write-json-string stream "message")
  (format stream ":")
  (write-json-string stream (getf result :message))
  (format stream ",")
  (write-json-string stream "details")
  (format stream ":")
  (write-json-string stream (or (getf result :details) ""))
  (format stream "}"))

(defun emit-json-report ()
  (format t "{")
  (write-json-string t "report_type")
  (format t ":")
  (write-json-string t "release_audit")
  (format t ",")
  (write-json-string t "project_version")
  (format t ":")
  (write-json-string t (project-version))
  (format t ",")
  (write-json-string t "requested_checks")
  (format t ":[")
  (loop for check in *requested-checks*
        for firstp = t then nil
        do (unless firstp
             (format t ","))
           (write-json-string t check))
  (format t "],")
  (write-json-string t "dry_run")
  (format t ":~A," (if *dry-run* "true" "false"))
  (write-json-string t "ok")
  (format t ":~A," (if (zerop (final-exit-code)) "true" "false"))
  (write-json-string t "exit_code")
  (format t ":~D," (final-exit-code))
  (write-json-string t "results")
  (format t ":[")
  (loop for result in (nreverse *results*)
        for firstp = t then nil
        do (unless firstp
             (format t ","))
           (write-json-result t result))
  (format t "]}~%"))

(defun main ()
  (let ((parsed (parse-args (argv))))
    (setf *requested-checks* (getf parsed :requested-checks)
          *dry-run* (getf parsed :dry-run)
          *output-format* (getf parsed :output-format))
    (if *dry-run*
        (dolist (check *requested-checks*)
          (check-result :pass check (planned-check-command check) "planned check"))
        (dolist (check *requested-checks*)
          (execute-check check)))
    (when (string= *output-format* "json")
      (emit-json-report))
    (sb-ext:exit :code (final-exit-code))))

(main)
