;;;; Release audit check runners.

(in-package #:cl-prolog.release-audit)

(define-audit-check "core" (core-command-spec) 600 verify-core)
(define-audit-check "tests" (tests-command-spec) 900 verify-tests)
(define-audit-check "nix" (nix-command-spec) 1200 verify-nix)
(define-audit-check "benchmarks" (benchmark-command-spec) 60 verify-benchmarks)

(defun verify-tracked-release-artifact (path)
  (let* ((result (run-command "git"
                              (list "ls-files" "--error-unmatch" "--" path)
                              :timeout *git-command-timeout*))
         (stdout (result-output-string result))
         (stderr (result-error-string result))
         (check (format nil "artifact/~A" path)))
    (if (zerop (result-exit-code result))
        (check-result :pass
                      check
                      "git ls-files --error-unmatch"
                      "artifact is tracked"
                      stdout)
        (check-result :fail
                      check
                      "git ls-files --error-unmatch"
                      "artifact is missing from git"
                      (format nil "stderr:~%~A~%stdout:~%~A" stderr stdout)))))

(defun verify-tracked-release-artifacts ()
  (dolist (path *source-snapshot-release-artifacts*)
    (verify-tracked-release-artifact path)))

(defun verify-core ()
  (with-command-result (command result (core-command-spec) (check-timeout "core"))
    (verify-command-result "core" command result
                           "public-contract verifier passed"
                           "public-contract verifier failed"))
  (verify-tracked-release-artifacts))

(defun verify-tests ()
  (with-command-result (command result (tests-command-spec) (check-timeout "tests"))
    (verify-command-result "tests" command result
                           "regression suite passed"
                           "regression suite failed")))

(defun verify-nix ()
  (with-command-result (command result (nix-command-spec) (check-timeout "nix"))
    (verify-command-result "nix" command result
                           "nix flake check passed"
                           "nix flake check failed")))

(defun verify-benchmarks ()
  (with-command-result (command result (benchmark-command-spec) (check-timeout "benchmarks"))
    (verify-command-result "benchmarks" command result
                           "benchmark smoke passed"
                           "benchmark smoke failed")))

(defun execute-check (check)
  (funcall (audit-check-runner (audit-check-for check))))
