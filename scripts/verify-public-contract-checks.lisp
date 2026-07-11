(in-package #:cl-prolog.verify-public-contract)

(defparameter *results* nil)
(defparameter *output-format* "text")

(defun record-result (status check message)
  (push (list :status status :check check :message message) *results*)
  (when (string= *output-format* "text")
    (format t "[~A] ~A: ~A~%"
            (string-upcase (symbol-name status))
            check
            message)))

(defun verify-package-contract (package-entry)
  (let* ((package-name (plist-value package-entry :name))
         (expected-exports (plist-value package-entry :exports))
         (expected-nicknames (plist-value package-entry :nicknames))
         (observed-exports (package-external-symbol-names package-name))
         (observed-nicknames (package-nickname-names package-name)))
    (if (string-list= expected-exports observed-exports)
        (record-result :pass
                       (format nil "package/~A/exports" package-name)
                       (format nil "exact export set matches (~D symbols)"
                               (length observed-exports)))
        (record-result :fail
                       (format nil "package/~A/exports" package-name)
                       (format nil "expected [~A] observed [~A]"
                               (string-join (sort-strings expected-exports))
                               (string-join observed-exports))))
    (if (string-list= expected-nicknames observed-nicknames)
        (record-result :pass
                       (format nil "package/~A/nicknames" package-name)
                       "exact nickname set matches")
        (record-result :fail
                       (format nil "package/~A/nicknames" package-name)
                       (format nil "expected [~A] observed [~A]"
                               (string-join (sort-strings expected-nicknames))
                               (string-join observed-nicknames))))))

(defun target-command (target)
  (command-spec target :script))

(defun fresh-image-command (system-name)
  (command-spec system-name :fresh))

(defun verify-target (target check-prefix)
  (handler-case
      (multiple-value-bind (program arguments timeout-kind)
          (if (string= check-prefix "fresh")
              (fresh-image-command target)
              (target-command target))
        (let* ((result (run-command program
                                    arguments
                                    :timeout (command-timeout timeout-kind)))
               (stdout (result-output-string result))
               (stderr (result-error-string result)))
          (if (zerop (result-exit-code result))
              (record-result :pass
                             (format nil "~A/~A" check-prefix target)
                             "execution succeeded")
              (record-result :fail
                             (format nil "~A/~A" check-prefix target)
                             (format nil "execution failed (exit=~D stderr=~S stdout=~S)"
                                     (result-exit-code result)
                                     stderr
                                     stdout)))))
    (error (condition)
      (record-result :fail
                     (format nil "~A/~A" check-prefix target)
                     (princ-to-string condition)))))

(defun verify-existing-file (path check-prefix)
  (if (probe-file (cl-prolog.bootstrap:repo-file path))
      (record-result :pass
                     (format nil "~A/~A" check-prefix path)
                     "file exists")
      (record-result :fail
                     (format nil "~A/~A" check-prefix path)
                     "file is missing")))

(defun verify-git-tracked-file (path check-prefix)
  (let* ((result (run-command "git"
                              (list "ls-files" "--error-unmatch" "--" path)
                              :timeout (command-timeout "git")))
         (stdout (result-output-string result))
         (stderr (result-error-string result)))
    (if (zerop (result-exit-code result))
        (record-result :pass
                       (format nil "~A/~A" check-prefix path)
                       "file is tracked in git")
        (record-result :fail
                       (format nil "~A/~A" check-prefix path)
                       (format nil "git does not track file (exit=~D stderr=~S stdout=~S)"
                               (result-exit-code result)
                               stderr
                               stdout)))))

(defun verify-existing-and-tracked-file (path check-prefix)
  (verify-existing-file path check-prefix)
  (verify-git-tracked-file path (format nil "~A-git" check-prefix)))

(defun verify-example-script-runtime (path)
  (let* ((result (run-command (cl-prolog.bootstrap:sbcl-program)
                              (list "--disable-debugger"
                                    "--script" path)
                              :timeout (command-timeout "sbcl-script")))
         (stdout (result-output-string result))
         (stderr (result-error-string result)))
    (if (zerop (result-exit-code result))
        (record-result :pass
                       (format nil "example-runtime/~A" path)
                       "script exits successfully")
        (record-result :fail
                       (format nil "example-runtime/~A" path)
                       (format nil "script failed (exit=~D stderr=~S stdout=~S)"
                               (result-exit-code result)
                               stderr
                               stdout)))))

(defun verify-cli-script (path)
  (let* ((help-result (run-command (cl-prolog.bootstrap:sbcl-program)
                                   (list "--noinform"
                                         "--disable-debugger"
                                         "--script" path "--help")
                                   :timeout (command-timeout "sbcl-script")))
         (help-stdout (result-output-string help-result))
         (help-stderr (result-error-string help-result))
         (version-result (run-command (cl-prolog.bootstrap:sbcl-program)
                                      (list "--noinform"
                                            "--disable-debugger"
                                            "--script" path "--version")
                                      :timeout (command-timeout "sbcl-script")))
         (version-stdout (result-output-string version-result))
         (version-stderr (result-error-string version-result))
         (version-line (format nil "cl-prolog ~A" (cl-prolog.bootstrap:project-version))))
    (if (and (zerop (result-exit-code help-result))
             (string= "" help-stderr)
             (string-contains-ci-p "Usage:" help-stdout)
             (string-contains-ci-p "--version" help-stdout))
        (record-result :pass
                       (format nil "cli/~A/help" path)
                       "--help contract passed")
        (record-result :fail
                       (format nil "cli/~A/help" path)
                       (format nil "--help contract failed (exit=~D stderr=~S stdout=~S)"
                               (result-exit-code help-result)
                               help-stderr
                               help-stdout)))
    (if (and (zerop (result-exit-code version-result))
             (string= "" version-stderr)
             (string= version-line
                      (string-right-trim '(#\Newline #\Return #\Space #\Tab)
                                         version-stdout)))
        (record-result :pass
                       (format nil "cli/~A/version" path)
                       "--version contract passed")
        (record-result :fail
                       (format nil "cli/~A/version" path)
                       (format nil "--version contract failed (exit=~D stderr=~S stdout=~S)"
                               (result-exit-code version-result)
                               version-stderr
                               version-stdout)))))

(defun verify-forbidden-content-entry (entry)
  (let* ((entry-id (plist-value entry :id))
         (paths (plist-value entry :paths))
         (substrings (plist-value entry :substrings))
         (message (plist-value entry :message)))
    (dolist (path paths)
      (if (not (probe-file (cl-prolog.bootstrap:repo-file path)))
          (record-result :fail
                         (format nil "content/~A/~A" entry-id path)
                         "file is missing, cannot verify content contract")
          (let* ((content (read-file-contents path))
                 (hits (loop for substring in substrings
                             when (string-contains-ci-p substring content)
                               collect substring)))
            (if hits
                (record-result :fail
                               (format nil "content/~A/~A" entry-id path)
                               (format nil "~A; found [~A]"
                                       message
                                       (string-join (sort-strings hits))))
                (record-result :pass
                               (format nil "content/~A/~A" entry-id path)
                               message)))))))

(defun content-contract-failures (content required-substrings minimum-counts)
  (let ((missing-substrings
          (loop for substring in required-substrings
                unless (string-contains-ci-p substring content)
                  collect substring))
        (count-failures
          (loop for (substring . minimum-count) in minimum-counts
                for observed = (count-substring-occurrences substring content)
                unless (>= observed minimum-count)
                  collect (format nil "~S observed ~D < required ~D"
                                  substring
                                  observed
                                  minimum-count))))
    (values missing-substrings count-failures)))

(defun verify-required-content-entry (entry file-check-prefix contract-check-prefix)
  (let* ((path (plist-value entry :path))
         (required-substrings (or (getf entry :required-substrings) '()))
         (minimum-counts (or (getf entry :minimum-counts) '()))
         (require-git-tracked (if (member :require-git-tracked entry)
                                  (getf entry :require-git-tracked)
                                  t))
         (message (plist-value entry :message)))
    (verify-existing-file path file-check-prefix)
    (when require-git-tracked
      (verify-git-tracked-file path (format nil "~A-git" file-check-prefix)))
    (if (not (probe-file (cl-prolog.bootstrap:repo-file path)))
        (record-result :fail
                       (format nil "~A/~A" contract-check-prefix path)
                       "file is missing, cannot verify content contract")
        (let ((content (read-file-contents path)))
          (multiple-value-bind (missing-substrings count-failures)
              (content-contract-failures content
                                         required-substrings
                                         minimum-counts)
            (if (and (null missing-substrings)
                     (null count-failures))
                (record-result :pass
                               (format nil "~A/~A" contract-check-prefix path)
                               message)
                (record-result :fail
                               (format nil "~A/~A" contract-check-prefix path)
                               (format nil "~A; missing [~A]; count failures [~A]"
                                       message
                                       (if missing-substrings
                                           (string-join (sort-strings missing-substrings))
                                           "")
                                       (if count-failures
                                           (string-join (sort-strings count-failures))
                                           "")))))))))

(defun verify-ci-workflow-entry (entry)
  (verify-required-content-entry entry "workflow-file" "workflow-contract"))

(defun verify-content-contract-entry (entry)
  (verify-required-content-entry entry "content-file" "content-contract"))

(defun summary ()
  (let ((total (length *results*))
        (failed (count-if (lambda (entry)
                            (eq (getf entry :status) :fail))
                          *results*)))
    (list :total total
          :failed failed
          :passed (- total failed))))

(defun final-exit-code ()
  (if (some (lambda (result)
              (eq (getf result :status) :fail))
            *results*)
      1
      0))

(defmacro define-path-list-verifier (name accessor check-prefix &key runtime)
  `(defun ,name (manifest)
     (dolist (path (,accessor manifest))
       (verify-existing-and-tracked-file path ,check-prefix)
       ,@(when runtime
           `((,runtime path))))))

(define-path-list-verifier verify-alias-files manifest-alias-files "alias-file")
(define-path-list-verifier verify-example-scripts manifest-example-scripts "example-file"
  :runtime verify-example-script-runtime)
(define-path-list-verifier verify-core-documents manifest-core-docs "doc-file")
(define-path-list-verifier verify-policy-documents manifest-policy-files "policy-file")
(define-path-list-verifier verify-stable-scripts manifest-stable-scripts "script-file"
  :runtime verify-cli-script)

(defun verify-package-contracts (manifest)
  (dolist (package-entry (manifest-packages manifest))
    (verify-package-contract package-entry)))

(defun verify-script-targets (manifest)
  (dolist (system-name (manifest-asdf-systems manifest))
    (verify-target system-name "script")))

(defun verify-fresh-image-targets (manifest)
  (dolist (system-name (manifest-fresh-image-systems manifest))
    (verify-target system-name "fresh")))

(defun verify-ci-workflows (manifest)
  (dolist (entry (manifest-ci-workflows manifest))
    (verify-ci-workflow-entry entry)))

(defun verify-forbidden-contents (manifest)
  (dolist (entry (manifest-forbidden-content manifest))
    (verify-forbidden-content-entry entry)))

(defun verify-content-contracts (manifest)
  (dolist (entry (manifest-content-contracts manifest))
    (verify-content-contract-entry entry)))
