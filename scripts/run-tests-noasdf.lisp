;;;; Run the fast core test suite without ASDF.

(load (merge-pathnames "bootstrap.lisp"
                       (or *load-truename*
                           *load-pathname*
                           (error "Cannot determine script path from *LOAD-TRUENAME* or *LOAD-PATHNAME*."))))

;; A large nursery keeps this short-lived process out of the collector,
;; which also sidesteps a GC-timing hang seen with some SBCL builds.
#+sbcl (setf (sb-ext:bytes-consed-between-gcs) (* 2048 1024 1024))

(let ((cl-prolog.bootstrap::*load-announcements* nil))
  (cl-prolog.bootstrap:load-source-file "scripts/run-tests-noasdf-main.lisp"))

(handler-case
    (cl-prolog.run-tests-noasdf:exit-script
     (cl-prolog.run-tests-noasdf:main))
  (error (condition)
    (format *error-output* "~A~%" condition)
    (cl-prolog.run-tests-noasdf:exit-script 1)))
