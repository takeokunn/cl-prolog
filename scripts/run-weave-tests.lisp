;;;; Run the cl-weave based test suite for cl-prolog.
;;;;
;;;; This loads cl-weave (the testing library) and the cl-prolog/weave-tests
;;;; system through ASDF, then runs every registered `describe` / `it` suite.
;;;; cl-weave must be discoverable on CL_SOURCE_REGISTRY (the Nix check and CI
;;;; both arrange this); it carries no external dependencies of its own.
;;;;
;;;; Exit code is 0 when every test passes and 1 otherwise, so it can gate CI.

(require :asdf)

(defun %script-directory ()
  (let ((path (or *load-truename* *load-pathname*)))
    (unless path
      (error "Cannot determine run-weave-tests.lisp path from *LOAD-TRUENAME*."))
    (make-pathname :name nil :type nil :version nil :defaults path)))

(defun %repo-root ()
  (merge-pathnames (make-pathname :directory '(:relative :up))
                   (%script-directory)))

;; Register the project's own systems (cl-prolog and cl-prolog/weave-tests).
(asdf:load-asd (truename (merge-pathnames "cl-prolog.asd" (%repo-root))))

;; Load the testing library and the suite that depends on it.  The suite's
;; `describe` / `it` forms register themselves into cl-weave's root suite as a
;; load-time side effect.
(asdf:load-system :cl-weave)
(asdf:load-system :cl-prolog/weave-tests)

(let ((passed (uiop:symbol-call "CL-WEAVE" "RUN-ALL" :reporter :spec)))
  (uiop:quit (if passed 0 1)))
