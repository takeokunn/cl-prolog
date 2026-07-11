#!/usr/bin/env sbcl --script

(load (merge-pathnames "bootstrap.lisp"
                       (or *load-truename*
                           *load-pathname*
                           (error "Cannot determine script path from *LOAD-TRUENAME* or *LOAD-PATHNAME*."))))

(let ((cl-prolog.bootstrap::*load-announcements* nil))
  (cl-prolog.bootstrap:load-core-sources)
  (cl-prolog.bootstrap:load-benchmark-support)
  (cl-prolog.bootstrap:load-source-file "scripts/benchmark-main.lisp")
  (cl-prolog.bootstrap:load-source-file "scripts/benchmark-report.lisp"))

(handler-case
    (cl-prolog.benchmark.cli:exit-script
     (cl-prolog.benchmark.cli:main))
  (error (condition)
    (format *error-output* "~A~%" condition)
    (cl-prolog.benchmark.cli:exit-script 1)))
