#!/usr/bin/env sbcl --script

(load (merge-pathnames "bootstrap.lisp"
                       (or *load-truename*
                           *load-pathname*
                           (error "Cannot determine script path from *LOAD-TRUENAME* or *LOAD-PATHNAME*."))))

(let ((cl-prolog.bootstrap::*load-announcements* nil))
  (cl-prolog.bootstrap:load-source-file "scripts/release-audit-data.lisp")
  (cl-prolog.bootstrap:load-source-file "scripts/release-audit-checks.lisp")
  (cl-prolog.bootstrap:load-source-file "scripts/release-audit-report.lisp")
  (cl-prolog.bootstrap:load-source-file "scripts/release-audit-main.lisp"))

(handler-case
    (cl-prolog.release-audit:exit-script
     (cl-prolog.release-audit:main))
  (error (condition)
    (format *error-output* "~A~%" condition)
    (cl-prolog.release-audit:exit-script 1)))
