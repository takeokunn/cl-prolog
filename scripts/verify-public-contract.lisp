#!/usr/bin/env sbcl --script

(load (merge-pathnames "bootstrap.lisp"
                       (or *load-truename*
                           *load-pathname*
                           (error "Cannot determine script path from *LOAD-TRUENAME* or *LOAD-PATHNAME*."))))

(let ((cl-prolog.bootstrap::*load-announcements* nil))
  (cl-prolog.bootstrap:load-source-file "scripts/verify-public-contract-main.lisp"))

(handler-case
    (cl-prolog.verify-public-contract:exit-script
     (cl-prolog.verify-public-contract:main))
  (error (condition)
    (format *error-output* "~A~%" condition)
    (cl-prolog.verify-public-contract:exit-script 1)))
