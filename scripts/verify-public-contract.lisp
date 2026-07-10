#!/usr/bin/env sbcl --script

(require :asdf)

(defpackage #:cl-prolog.verify-public-contract
  (:use #:cl))

(in-package #:cl-prolog.verify-public-contract)

(defparameter *results* nil)
(defparameter *output-format* "text")
(defparameter *manifest-path* "contracts/public-contract.sexp")
(defparameter *command-timeouts*
  '(("git" . 10)
    ("sbcl-script" . 20)
    ("sbcl-fresh-load" . 25)))

(defun script-path ()
  (or *load-truename*
      (error "Cannot determine script path from *LOAD-TRUENAME*.")))

(defun repo-root ()
  (uiop:ensure-directory-pathname
   (merge-pathnames "../" (uiop:pathname-directory-pathname (script-path)))))

(defun repo-file (relative-path)
  (merge-pathnames relative-path (repo-root)))

(asdf:load-asd (repo-file "cl-prolog.asd"))
(asdf:load-system :cl-prolog)

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
  (format stream "Usage: sbcl --script scripts/verify-public-contract.lisp [--json]~%")
  (format stream "       sbcl --script scripts/verify-public-contract.lisp --help~%")
  (format stream "       sbcl --script scripts/verify-public-contract.lisp --version~%")
  (format stream "~%")
  (format stream "Checks:~%")
  (format stream "  package surface    Exact package exports and nickname set.~%")
  (format stream "  asdf systems       Documented systems are defined and loadable.~%")
  (format stream "  fresh images       Selected systems load in a fresh SBCL image.~%")
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
  (sb-ext:exit :code 2))

(defun parse-args (args)
  (let ((output-format "text"))
    (loop while args
          for arg = (pop args)
          do (cond
               ((string= arg "--json")
                (setf output-format "json"))
               ((member arg '("--help" "-h") :test #'string=)
                (usage)
                (sb-ext:exit :code 0))
               ((string= arg "--version")
                (print-version)
                (sb-ext:exit :code 0))
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

(defun sort-strings (strings)
  (sort (copy-list strings) #'string<))

(defun string-list= (left right)
  (equal (sort-strings left) (sort-strings right)))

(defun string-join (strings &optional (separator ", "))
  (with-output-to-string (out)
    (loop for string in strings
          for firstp = t then nil
          do (unless firstp
               (write-string separator out))
             (write-string string out))))

(defun plist-value (plist key)
  (let ((marker (gensym "MISSING")))
    (let ((value (getf plist key marker)))
      (if (eq value marker)
          (error "Manifest key ~A is missing." key)
          value))))

(defun read-manifest ()
  (with-open-file (stream (repo-file *manifest-path*) :direction :input)
    (let ((manifest (read stream nil nil)))
      (unless manifest
        (error "Manifest is empty: ~A" *manifest-path*))
      manifest)))

(defun manifest-packages (manifest)
  (plist-value manifest :packages))

(defun manifest-asdf-systems (manifest)
  (plist-value manifest :asdf-systems))

(defun manifest-fresh-image-systems (manifest)
  (or (getf manifest :fresh-image-systems) '()))

(defun manifest-alias-files (manifest)
  (or (getf manifest :alias-files) '()))

(defun manifest-example-scripts (manifest)
  (plist-value manifest :example-scripts))

(defun manifest-core-docs (manifest)
  (plist-value manifest :core-docs))

(defun manifest-policy-files (manifest)
  (plist-value manifest :policy-files))

(defun manifest-stable-scripts (manifest)
  (plist-value manifest :stable-scripts))

(defun manifest-forbidden-content (manifest)
  (or (getf manifest :forbidden-content) '()))

(defun package-external-symbol-names (package-designator)
  (let ((package (find-package package-designator)))
    (unless package
      (error "Package ~A is not available." package-designator))
    (sort-strings
     (loop for symbol being the external-symbols of package
           collect (symbol-name symbol)))))

(defun package-nickname-names (package-designator)
  (let ((package (find-package package-designator)))
    (unless package
      (error "Package ~A is not available." package-designator))
    (sort-strings (mapcar #'string (package-nicknames package)))))

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

(defun string-contains-ci-p (needle haystack)
  (and haystack
       (search needle haystack :test #'char-equal)))

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

(defun load-system-quietly (system-name)
  (let ((*standard-output* (make-broadcast-stream))
        (*error-output* (make-broadcast-stream))
        (*trace-output* (make-broadcast-stream))
        (*compile-verbose* nil)
        (*compile-print* nil)
        (*load-verbose* nil))
    (asdf:load-system system-name)))

(defun verify-asdf-system (system-name)
  (handler-case
      (progn
        (unless (asdf:find-system (intern (string-upcase system-name) "KEYWORD") nil)
          (error "System is not defined: ~A" system-name))
        (load-system-quietly (intern (string-upcase system-name) "KEYWORD"))
        (record-result :pass
                       (format nil "asdf/~A" system-name)
                       "system is defined and loadable"))
    (error (condition)
      (record-result :fail
                     (format nil "asdf/~A" system-name)
                     (princ-to-string condition)))))

(defun verify-fresh-image-asdf-system (system-name)
  (let* ((result (run-command
                  (sbcl-program)
                  (list "--non-interactive"
                        "--eval" "(require :asdf)"
                        "--load" "cl-prolog.asd"
                        "--eval" (format nil "(asdf:load-system ~S)" system-name))
                  :timeout (command-timeout "sbcl-fresh-load")))
         (stdout (result-output-string result))
         (stderr (result-error-string result)))
    (if (zerop (result-exit-code result))
        (record-result :pass
                       (format nil "asdf-fresh/~A" system-name)
                       "system loads successfully in a fresh SBCL image")
        (record-result :fail
                       (format nil "asdf-fresh/~A" system-name)
                       (format nil "fresh-image load failed (exit=~D stderr=~S stdout=~S)"
                               (result-exit-code result)
                               stderr
                               stdout)))))

(defun verify-existing-file (path check-prefix)
  (if (uiop:file-exists-p (repo-file path))
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

(defun verify-example-script-runtime (path)
  (let* ((result (run-command (sbcl-program)
                              (list "--script" path)
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

(defun verify-alias-runtime (path)
  (declare (ignore path))
  nil)

(defun verify-cli-script (path)
  (let* ((help-result (run-command (sbcl-program)
                                   (list "--script" path "--help")
                                   :timeout (command-timeout "sbcl-script")))
         (help-stdout (result-output-string help-result))
         (help-stderr (result-error-string help-result))
         (version-result (run-command (sbcl-program)
                                      (list "--script" path "--version")
                                      :timeout (command-timeout "sbcl-script")))
         (version-stdout (result-output-string version-result))
         (version-stderr (result-error-string version-result))
         (version-line (format nil "cl-prolog ~A" (project-version))))
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

(defun read-file-contents (path)
  (with-open-file (stream (repo-file path) :direction :input)
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      content)))

(defun verify-forbidden-content-entry (entry)
  (let* ((entry-id (plist-value entry :id))
         (paths (plist-value entry :paths))
         (substrings (plist-value entry :substrings))
         (message (plist-value entry :message)))
    (dolist (path paths)
      (if (not (uiop:file-exists-p (repo-file path)))
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
  (write-json-string stream "message")
  (format stream ":")
  (write-json-string stream (getf result :message))
  (format stream "}"))

(defun emit-json-report (manifest)
  (let ((report-summary (summary)))
    (format t "{")
    (write-json-string t "manifest")
    (format t ":")
    (write-json-string t *manifest-path*)
    (format t ",")
    (write-json-string t "report_type")
    (format t ":")
    (write-json-string t "public_contract")
    (format t ",")
    (write-json-string t "project_version")
    (format t ":")
    (write-json-string t (project-version))
    (format t ",")
    (write-json-string t "ok")
    (format t ":~A," (if (zerop (final-exit-code)) "true" "false"))
    (write-json-string t "summary")
    (format t ":{")
    (write-json-string t "total")
    (format t ":~D," (getf report-summary :total))
    (write-json-string t "passed")
    (format t ":~D," (getf report-summary :passed))
    (write-json-string t "failed")
    (format t ":~D}," (getf report-summary :failed))
    (write-json-string t "manifest_version")
    (format t ":")
    (write-json-string t (plist-value manifest :project-version))
    (format t ",")
    (write-json-string t "results")
    (format t ":[")
    (loop for result in (nreverse *results*)
          for firstp = t then nil
          do (unless firstp
               (format t ","))
             (write-json-result t result))
    (format t "]}~%")))

(defun verify-contract (manifest)
  (dolist (package-entry (manifest-packages manifest))
    (verify-package-contract package-entry))
  (dolist (system-name (manifest-asdf-systems manifest))
    (verify-asdf-system system-name))
  (dolist (system-name (manifest-fresh-image-systems manifest))
    (verify-fresh-image-asdf-system system-name))
  (dolist (path (manifest-alias-files manifest))
    (verify-existing-file path "alias-file")
    (verify-git-tracked-file path "alias-git")
    (verify-alias-runtime path))
  (dolist (path (manifest-example-scripts manifest))
    (verify-existing-file path "example-file")
    (verify-git-tracked-file path "example-git")
    (verify-example-script-runtime path))
  (dolist (path (manifest-core-docs manifest))
    (verify-existing-file path "doc-file")
    (verify-git-tracked-file path "doc-git"))
  (dolist (path (manifest-policy-files manifest))
    (verify-existing-file path "policy-file")
    (verify-git-tracked-file path "policy-git"))
  (dolist (entry (manifest-forbidden-content manifest))
    (verify-forbidden-content-entry entry))
  (dolist (path (manifest-stable-scripts manifest))
    (verify-existing-file path "script-file")
    (verify-git-tracked-file path "script-git")
    (verify-cli-script path)))

(defun main ()
  (let* ((manifest (read-manifest)))
    (setf *output-format* (parse-args (argv)))
    (verify-contract manifest)
    (when (string= *output-format* "json")
      (emit-json-report manifest))
    (sb-ext:exit :code (final-exit-code))))

(main)
