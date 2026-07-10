;;;; CLI script JSON-contract tests.
;;;;
;;;; These spawn nested SBCL images and require a working ASDF, so they
;;;; are loaded only by the full suite entry point (tests.lisp), not by
;;;; scripts/run-tests-noasdf.lisp.

(in-package #:fx.prolog.tests)

(defun repo-root ()
  (uiop:ensure-directory-pathname
   (asdf:system-source-directory :cl-prolog)))

(defun sbcl-program ()
  (or (uiop:getenv "SBCL") "sbcl"))

(defun run-script (script &rest arguments)
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program
       (append (list (sbcl-program) "--script" script) arguments)
       :directory (repo-root)
       :output '(:string :stripped nil)
       :error-output '(:string :stripped nil)
       :ignore-error-status t
       :timeout 120)
    (list :output output
          :error-output error-output
          :exit-code exit-code)))

(defun compact-json (string)
  (coerce (remove-if (lambda (ch)
                       (find ch '(#\Space #\Tab #\Newline #\Return)))
                     string)
          'string))

(defun %project-version-fragment ()
  (format nil "\"project_version\":\"~A\""
          (asdf:component-version (asdf:find-system :cl-prolog))))

(defun %script-json (script &rest arguments)
  (let* ((result (apply #'run-script script arguments))
         (stdout (getf result :output))
         (stderr (getf result :error-output)))
    (is-equal 0
              (getf result :exit-code)
              (format nil "Script failed: ~A~%stdout:~%~A~%stderr:~%~A"
                      script
                      stdout
                      stderr))
    (compact-json stdout)))

(defun %json-includes-p (json fragment)
  (not (null (search fragment json :test #'char=))))

(deftest verifier-json-contract ()
  (let ((json (%script-json "scripts/verify-public-contract.lisp" "--json")))
    (is (%json-includes-p json "\"report_type\":\"public_contract\""))
    (is (%json-includes-p json "\"manifest\":\"contracts/public-contract.sexp\""))
    (is (%json-includes-p json (%project-version-fragment)))
    (is (%json-includes-p json "\"manifest_version\":"))
    (is (%json-includes-p json "\"ok\":true"))
    (is (%json-includes-p json "\"summary\":{"))
    (is (%json-includes-p json "\"results\":["))
    (is (%json-includes-p json "\"check\":\"package/FX.PROLOG/exports\""))))

(deftest benchmark-json-contract ()
  (let ((json (%script-json "scripts/benchmark.lisp"
                            "--json"
                            "--iterations"
                            "1"
                            "--scenario"
                            "ancestor-first")))
    (is (%json-includes-p json "\"report_type\":\"benchmark\""))
    (is (%json-includes-p json (%project-version-fragment)))
    (is (%json-includes-p json "\"requested_scenarios\":[\"ancestor-first\"]"))
    (is (%json-includes-p json "\"iterations\":1"))
    (is (%json-includes-p json "\"scenario_count\":1"))
    (is (%json-includes-p json "\"ok\":true"))
    (is (%json-includes-p json "\"results\":[{"))
    (is (%json-includes-p json "\"scenario\":\"ancestor-first\""))
    (is (%json-includes-p json "\"total_ns\":"))
    (is (%json-includes-p json "\"avg_ns\":"))
    (is (%json-includes-p json "\"last_result\":"))))

(deftest release-audit-json-contract ()
  (let ((json (%script-json "scripts/release-audit.lisp"
                            "--dry-run"
                            "--json"
                            "--with-benchmarks")))
    (is (%json-includes-p json "\"report_type\":\"release_audit\""))
    (is (%json-includes-p json (%project-version-fragment)))
    (is (%json-includes-p json "\"requested_checks\":[\"tests\",\"core\",\"benchmarks\"]"))
    (is (%json-includes-p json "\"dry_run\":true"))
    (is (%json-includes-p json "\"ok\":true"))
    (is (%json-includes-p json "\"exit_code\":0"))
    (is (%json-includes-p json "\"check\":\"tests\""))
    (is (%json-includes-p json "\"check\":\"core\""))
    (is (%json-includes-p json "\"check\":\"benchmarks\""))))
