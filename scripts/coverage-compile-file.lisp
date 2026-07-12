#!/usr/bin/env sbcl --script

;;;; Isolated sb-cover compilation for a single source file.

;; Same Darwin trap-delivery workaround as scripts/bootstrap.lisp: keep the
;; implicit GC trigger out of reach and collect explicitly between phases.
;; The parent passes --dynamic-space-size 4096 so this threshold fits.
#+(and sbcl darwin)
(setf (sb-ext:bytes-consed-between-gcs) (* 3 1024 1024 1024))

(require :sb-cover)

(declaim (optimize sb-cover:store-coverage-data))

(defun script-path ()
  (or *load-truename*
      (error "Cannot determine script path from *LOAD-TRUENAME*.")))

(defun repo-root ()
  (let ((here (script-path)))
    (make-pathname :name nil :type nil :version nil
                   :directory (butlast (pathname-directory here))
                   :defaults here)))

(defun repo-file (path)
  (merge-pathnames path (repo-root)))

(defun usage (&optional (stream *standard-output*))
  (format stream "Usage: sbcl --script scripts/coverage-compile-file.lisp SOURCE OUTPUT [DEPENDENCY ...]~%")
  (format stream "Compile SOURCE with sb-cover instrumentation into OUTPUT after loading DEPENDENCY files.~%"))

(defun parse-args ()
  (let ((args (cdr sb-ext:*posix-argv*)))
    (when (or (null args)
              (member (first args) '("--help" "-h") :test #'string=))
      (usage)
      (sb-ext:exit :code 0))
    (when (< (length args) 2)
      (usage *error-output*)
      (sb-ext:exit :code 2))
    (values (first args) (second args) (cddr args))))

(multiple-value-bind (source output dependencies)
  (parse-args)
  (dolist (dependency dependencies)
    (load (repo-file dependency))
    #+sbcl (sb-ext:gc))
  (compile-file (repo-file source)
                :output-file (repo-file output)
                :verbose nil
                :print nil))
