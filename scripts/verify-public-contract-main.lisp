(defpackage #:cl-prolog.verify-public-contract
  (:use #:cl)
  (:export #:exit-script
           #:main))

(in-package #:cl-prolog.verify-public-contract)

(cl-prolog.bootstrap:load-source-file "scripts/verify-public-contract-data.lisp")
(cl-prolog.bootstrap:load-source-file "scripts/verify-public-contract-checks.lisp")

(defun exit-script (code)
  (finish-output *standard-output*)
  (finish-output *error-output*)
  (sb-ext:exit :code code))

(defun argv ()
  (cdr sb-ext:*posix-argv*))

(defun print-version (&optional (stream *standard-output*))
  (format stream "cl-prolog ~A~%" (cl-prolog.bootstrap:project-version)))

(defun usage (&optional (stream *standard-output*))
  (format stream "Usage: sbcl --script scripts/verify-public-contract.lisp [--json]~%")
  (format stream "       sbcl --script scripts/verify-public-contract.lisp --help~%")
  (format stream "       sbcl --script scripts/verify-public-contract.lisp --version~%")
  (format stream "~%")
  (format stream "Checks:~%")
  (format stream "  package surface    Exact package exports and nickname set.~%")
  (format stream "  script targets     Documented scripts run successfully.~%")
  (format stream "  fresh images       Selected targets run in a fresh SBCL image.~%")
  (format stream "  release files      Manifested scripts, docs, examples, and policy files exist and are git-tracked.~%")
  (format stream "  cli scripts        Stable scripts keep --help and --version contracts.~%")
  (format stream "~%")
  (format stream "Exit status:~%")
  (format stream "  0  The observed surface matches the manifest.~%")
  (format stream "  1  At least one check failed.~%")
  (format stream "  2  Invalid CLI usage.~%"))

(defun usage-error (control &rest args)
  (apply #'format *error-output* control args)
  (format *error-output* "~%~%")
  (usage *error-output*)
  (exit-script 2))

(defun parse-args (args)
  (let ((output-format "text"))
    (loop while args
          for arg = (pop args)
          do (cond
               ((string= arg "--json")
                (setf output-format "json"))
               ((member arg '("--help" "-h") :test #'string=)
                (usage)
                (exit-script 0))
               ((string= arg "--version")
                (print-version)
                (exit-script 0))
               (t
                (usage-error "Unknown argument: ~A" arg))))
    output-format))

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

(defun write-json-result (stream result)
  (write-char #\{ stream)
  (write-json-string stream "status")
  (write-char #\: stream)
  (write-json-string stream
                     (ecase (getf result :status)
                       (:pass "pass")
                       (:fail "fail")))
  (write-char #\, stream)
  (write-json-string stream "check")
  (write-char #\: stream)
  (write-json-string stream (getf result :check))
  (write-char #\, stream)
  (write-json-string stream "message")
  (write-char #\: stream)
  (write-json-string stream (getf result :message))
  (write-char #\} stream))

(defun write-json-results (stream results)
  (write-char #\[ stream)
  (loop for result in results
        for firstp = t then nil
        do (unless firstp
             (write-char #\, stream))
           (write-json-result stream result))
  (write-char #\] stream))

(defun emit-json-report (manifest)
  (let ((report-summary (summary)))
    (write-char #\{ *standard-output*)
    (write-json-string *standard-output* "manifest")
    (write-char #\: *standard-output*)
    (write-json-string *standard-output* *manifest-path*)
    (write-char #\, *standard-output*)
    (write-json-string *standard-output* "report_type")
    (write-char #\: *standard-output*)
    (write-json-string *standard-output* "public_contract")
    (write-char #\, *standard-output*)
    (write-json-string *standard-output* "project_version")
    (write-char #\: *standard-output*)
    (write-json-string *standard-output* (cl-prolog.bootstrap:project-version))
    (write-char #\, *standard-output*)
    (write-json-string *standard-output* "ok")
    (format *standard-output* ":~A," (if (zerop (final-exit-code)) "true" "false"))
    (write-json-string *standard-output* "summary")
    (format *standard-output* ":{")
    (write-json-string *standard-output* "total")
    (format *standard-output* ":~D," (getf report-summary :total))
    (write-json-string *standard-output* "passed")
    (format *standard-output* ":~D," (getf report-summary :passed))
    (write-json-string *standard-output* "failed")
    (format *standard-output* ":~D}," (getf report-summary :failed))
    (write-json-string *standard-output* "manifest_version")
    (write-char #\: *standard-output*)
    (write-json-string *standard-output* (plist-value manifest :project-version))
    (write-char #\, *standard-output*)
    (write-json-string *standard-output* "results")
    (write-char #\: *standard-output*)
    (write-json-results *standard-output* (reverse *results*))
    (format *standard-output* "}~%")))

(defparameter *verification-passes*
  '(verify-package-contracts
    verify-script-targets
    verify-fresh-image-targets
    verify-alias-files
    verify-example-scripts
    verify-core-documents
    verify-policy-documents
    verify-ci-workflows
    verify-forbidden-contents
    verify-content-contracts
    verify-stable-scripts))

(defun verify-contract (manifest)
  (cl-prolog.bootstrap:load-core-sources)
  (dolist (verifier *verification-passes*)
    (funcall verifier manifest)))

(defun verifier-report (manifest)
  (setf *results* nil)
  (verify-contract manifest)
  manifest)

(defun emit-report (manifest)
  (when (string= *output-format* "json")
    (emit-json-report manifest)))

(defun main (&optional (args (argv)))
  (setf *output-format* (parse-args args))
  (let ((manifest (cl-prolog.bootstrap:read-manifest)))
    (emit-report (verifier-report manifest))
    (final-exit-code)))
