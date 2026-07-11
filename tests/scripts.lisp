;;;; CLI script JSON-contract tests.
;;;;
;;;; These spawn nested SBCL images, so they are loaded only by the full
;;;; suite entry point (tests.lisp), not by scripts/run-tests-noasdf.lisp.

(in-package #:cl-prolog.tests)

(defun sbcl-program ()
  (or (sb-ext:posix-getenv "SBCL") "sbcl"))

(defun run-script (script &rest arguments)
  (cl-prolog.bootstrap:run-command-capture
   (sbcl-program)
   (append (list "--script" script) arguments)
   :timeout 120
   :directory (cl-prolog.bootstrap:repo-root)))

(defun run-script-with-timeout (timeout script &rest arguments)
  (cl-prolog.bootstrap:run-command-capture
   (sbcl-program)
   (append (list "--script" script) arguments)
   :timeout timeout
   :directory (cl-prolog.bootstrap:repo-root)))

(defun compact-json (string)
  (with-output-to-string (out)
    (loop with in-string = nil
          with escaped = nil
          for ch across string
          do (cond
               (escaped
                (write-char ch out)
                (setf escaped nil))
               ((char= ch #\\)
                (write-char ch out)
                (when in-string
                  (setf escaped t)))
               ((char= ch #\")
                (write-char ch out)
                (setf in-string (not in-string)))
               ((and (not in-string)
                     (find ch '(#\Space #\Tab #\Newline #\Return)))
                nil)
               (t
                (write-char ch out))))))

(defun %project-version-fragment ()
  (format nil "\"project_version\":\"~A\""
          (cl-prolog.bootstrap:project-version)))

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

(defun assert-json-fragments (json fragments)
  (dolist (fragment fragments)
    (is (%json-includes-p json fragment)
        (format nil "Missing JSON fragment: ~A~%JSON: ~A" fragment json))))

(defmacro define-json-contract-tests (&body cases)
  `(progn
     ,@(mapcar
        (lambda (case)
          (destructuring-bind (&key name command fragments (timeout 60)) case
            `(deftest ,name (:timeout ,timeout)
               (let ((json (apply #'%script-json ',command)))
                 (assert-json-fragments json (list ,@fragments))))))
        cases)))

(define-json-contract-tests
  (:name verifier-json-contract
   :command ("scripts/verify-public-contract.lisp" "--json")
   :fragments ("\"report_type\":\"public_contract\""
               "\"manifest\":\"contracts/public-contract.sexp\""
               (%project-version-fragment)
               "\"manifest_version\":"
               "\"ok\":true"
               "\"summary\":{"
               "\"results\":["
               "\"check\":\"package/CL-PROLOG/exports\""
               "\"check\":\"workflow-contract/.github/workflows/ci.yml\""
               "\"check\":\"content-contract/scripts/release-audit-main.lisp\""))
  (:name benchmark-json-contract
   :command ("scripts/benchmark.lisp" "--json" "--iterations" "1" "--scenario" "ancestor-first")
   :fragments ("\"report_type\":\"benchmark\""
               (%project-version-fragment)
               "\"requested_scenarios\":[\"ancestor-first\"]"
               "\"iterations\":1"
               "\"scenario_count\":1"
               "\"ok\":true"
               "\"results\":[{"
               "\"scenario\":\"ancestor-first\""
               "\"total_ns\":"
               "\"avg_ns\":"
               "\"last_result\":"))
  (:name release-audit-json-contract
   :command ("scripts/release-audit.lisp" "--dry-run" "--json" "--with-benchmarks" "--with-script-contracts")
   :fragments ("\"report_type\":\"release_audit\""
               (%project-version-fragment)
               "\"requested_checks\":[\"tests\",\"core\",\"benchmarks\"]"
               "\"dry_run\":true"
               "\"ok\":true"
               "\"exit_code\":0"
               "\"command\":\"env CL_PROLOG_TEST_SCRIPTS=1 sbcl --script tests.lisp\""
               "\"check\":\"tests\""
               "\"check\":\"core\""
               "\"check\":\"benchmarks\"")))

(deftest coverage-gate-contract (:timeout 600)
  (let* ((report (cl-prolog.bootstrap:repo-file "coverage/cover-index.html"))
         (result (run-script-with-timeout 540 "scripts/coverage.lisp"))
         (output (concatenate 'string
                              (getf result :output)
                              (getf result :error-output)))
         (passed-p (not (null (search ";; coverage gate: PASS" output
                                      :test #'char=))))
         (failed-p (not (null (search ";; coverage gate: FAIL" output
                                      :test #'char=)))))
    (is (or passed-p failed-p)
        (format nil "Coverage script emitted no gate result:~%~A" output))
    (is-equal (if passed-p 0 1)
              (getf result :exit-code)
              (format nil "Coverage gate and exit code disagree:~%~A" output))
    (is (probe-file report)
        "Coverage gate must preserve the HTML report.")))
