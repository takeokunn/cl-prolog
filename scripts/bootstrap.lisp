;;;; Shared bootstrap helpers for direct-load scripts.

(require :asdf)

(defpackage #:cl-prolog.bootstrap
  (:use #:cl)
  (:export #:repo-root
           #:repo-file
           #:read-manifest
           #:project-version
           #:perl-program
           #:sbcl-program
           #:perl-timeout-wrapper
           #:core-source-files
           #:test-source-files
           #:load-source-file
           #:load-core-sources
           #:load-test-sources
           #:load-script-contract-tests
           #:load-benchmark-support
           #:run-command-capture
           #:run-command-stream))

(in-package #:cl-prolog.bootstrap)

(defparameter *core-source-files*
  '("src/package.lisp"
      "src/operator-table.lisp"
      "src/module-system.lisp"
      "src/data.lisp"
      "src/unification.lisp"
      "src/parser.lisp"
      "src/term-writer.lisp"
    "src/engine.lisp"
    "src/io-context.lisp"
    "src/prover.lisp"
    "src/builtins/core.lisp"
    "src/builtins/control.lisp"
    "src/builtins/collection.lisp"
    "src/builtins/dynamic.lisp"
    "src/builtins/arithmetic.lisp"
    "src/builtins/list.lisp"
    "src/builtins/atom.lisp"
    "src/builtins/operator.lisp"
    "src/builtins/io.lisp"
    "src/builtins/io-code.lisp"
    "src/fd-store.lisp"
    "src/builtins/fd.lisp"
    "src/builtin-term.lisp"
    "src/dcg-runtime.lisp"
    "src/query.lisp"
    "src/source-loader.lisp"
    "src/dsl-compiler.lisp"
    "src/dsl.lisp"
    "src/dcg.lisp"))

(defparameter *test-source-files*
  '("tests/support.lisp"
      "tests/unification.lisp"
      "tests/operator-table.lisp"
      "tests/parser.lisp"
      "tests/term-writer.lisp"
      "tests/io-context.lisp"
      "tests/source-loader.lisp"
    "tests/engine-surface.lisp"
    "tests/engine-queries.lisp"
    "tests/engine-runtime.lisp"
    "tests/builtin-term.lisp"
    "tests/builtin-atom.lisp"
    "tests/builtin-operator.lisp"
    "tests/builtin-io.lisp"
    "tests/builtin-io-code.lisp"
    "tests/builtin-fd.lisp"
    "tests/module-system.lisp"
    "tests/dcg.lisp"))

(defparameter *script-contract-test-files*
  '("tests/scripts.lisp"))

(defparameter *benchmark-support-files*
  '("scripts/benchmark-support.lisp"))

(defparameter *load-announcements* t)
(defparameter *temporary-output-counter* 0)

(defun core-source-files ()
  (copy-list *core-source-files*))

(defun test-source-files ()
  (copy-list *test-source-files*))

(defparameter *bootstrap-path*
  (or *load-truename*
      *load-pathname*
      (error "Cannot determine bootstrap path from *LOAD-TRUENAME* or *LOAD-PATHNAME*.")))

(defparameter *repo-root*
  (let* ((directory (pathname-directory *bootstrap-path*)))
    (make-pathname :name nil :type nil :version nil
                   :directory (butlast directory)
                   :defaults *bootstrap-path*)))

(defun repo-root ()
  *repo-root*)

(defun repo-file (relative-path)
  (merge-pathnames relative-path (repo-root)))

(defun source-pathname (path)
  (let ((pathname (pathname path)))
    (if (and (pathname-directory pathname)
             (eq (first (pathname-directory pathname)) :absolute))
        pathname
        (repo-file pathname))))

(defun read-manifest ()
  (with-open-file (stream (repo-file "contracts/public-contract.sexp")
                          :direction :input)
    (let ((manifest (read stream nil nil)))
      (unless manifest
        (error "Manifest is empty: contracts/public-contract.sexp"))
      manifest)))

(defun project-version ()
  (getf (read-manifest) :project-version))

(defun perl-program ()
  (or (sb-ext:posix-getenv "PERL")
      "perl"))

(defun sbcl-program ()
  (or (sb-ext:posix-getenv "SBCL")
      "sbcl"))

(defun perl-timeout-wrapper (timeout)
  (format nil (concatenate 'string
                           "use POSIX qw(:sys_wait_h); "
                           "my $timeout = shift @ARGV; "
                           "my $pid = fork(); "
                           "die qq(fork failed: $!) unless defined $pid; "
                           "if (!$pid) { exec @ARGV; die qq(exec failed: $!); } "
                           "my $deadline = time + $timeout; "
                           "while (1) { "
                           "my $wait = waitpid($pid, WNOHANG); "
                           "if ($wait == $pid) { exit($? >> 8); } "
                           "if (time >= $deadline) { last; } "
                           "select undef, undef, undef, 0.1; "
                           "} "
                           "kill 'TERM', $pid; "
                           "for (1..20) { "
                           "my $wait = waitpid($pid, WNOHANG); "
                           "if ($wait == $pid) { exit 124; } "
                           "select undef, undef, undef, 0.1; "
                           "} "
                           "kill 'KILL', $pid; "
                           "waitpid($pid, 0); "
                           "exit 124;")
          (or timeout 15)))

(defun temporary-directory ()
  (let ((tmpdir
          #+sbcl (sb-ext:posix-getenv "TMPDIR")
          #-sbcl nil))
    (if (and tmpdir (not (string= tmpdir "")))
        (pathname (format nil "~A/" (string-right-trim "/" tmpdir)))
        #p"/tmp/")))

(defun temporary-output-pathname (prefix)
  (merge-pathnames
   (make-pathname :name (format nil "cl-prolog-~A-~A-~A-~D-~D"
                                prefix
                                (sb-unix:unix-getpid)
                                (get-internal-real-time)
                                (incf *temporary-output-counter*)
                                (get-universal-time)
                                )
                  :type "log")
   (temporary-directory)))

(defun read-text-file-if-present (path)
  (if (probe-file path)
      (with-open-file (stream path :direction :input)
        (let ((content (make-string (file-length stream))))
          (read-sequence content stream)
          content))
      ""))

(defun command-with-timeout (program arguments timeout)
  (if timeout
      (append (list (perl-program)
                    "-MPOSIX=:sys_wait_h"
                    "-e"
                    (perl-timeout-wrapper timeout)
                    (write-to-string timeout)
                    program)
              arguments)
      (cons program arguments)))

(defun run-command-capture (program arguments &key timeout (directory (repo-root)))
  (handler-case
      (let ((stdout-path (temporary-output-pathname "stdout"))
            (stderr-path (temporary-output-pathname "stderr")))
        (unwind-protect
             (let* ((command (if timeout
                                 (command-with-timeout program arguments timeout)
                                 (cons program arguments)))
                    (process (sb-ext:run-program (first command)
                                                 (rest command)
                                                 :search t
                                                 :input nil
                                                 :output stdout-path
                                                 :error stderr-path
                                                 :directory directory
                                                 :wait t)))
               (list :output (read-text-file-if-present stdout-path)
                     :error-output (read-text-file-if-present stderr-path)
                     :exit-code (sb-ext:process-exit-code process)))
          (ignore-errors (delete-file stdout-path))
          (ignore-errors (delete-file stderr-path))))
    (error (condition)
      (list :output ""
            :error-output (princ-to-string condition)
            :exit-code 1))))

(defun run-command-stream (program arguments &key timeout (directory (repo-root)))
  (handler-case
      (let* ((command (command-with-timeout program arguments timeout))
             (process (sb-ext:run-program (first command)
                                          (rest command)
                                          :search t
                                          :input nil
                                          :output *standard-output*
                                          :error *error-output*
                                          :directory directory
                                          :wait t)))
        (sb-ext:process-exit-code process))
    (error (condition)
      (format *error-output* "~&;; command failed: ~A ~{~A~^ ~}~%" program arguments)
      (format *error-output* "~&;; reason: ~A~%" condition)
      1)))

(defun load-source-file (relative-path)
  (when *load-announcements*
    (format t "~&;; loading ~A~%" relative-path)
    (finish-output))
  #+sbcl
  (let ((sb-ext:*evaluator-mode* :interpret))
    (load (repo-file relative-path)))
  #-sbcl
  (load (repo-file relative-path)))

(defun load-source-files (relative-paths)
  (dolist (relative-path relative-paths)
    (load-source-file relative-path)
    #+sbcl
    (sb-ext:gc))
  #+sbcl
  (sb-ext:gc :full t))

(defun load-core-sources ()
  (load-source-files *core-source-files*))

(defun load-test-sources ()
  (load-source-files *test-source-files*))

(defun load-script-contract-tests ()
  (load-source-files *script-contract-test-files*))

(defun load-benchmark-support ()
  (load-source-files *benchmark-support-files*))
