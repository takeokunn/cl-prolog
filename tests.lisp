;;;; ASDF test-op entry point for cl-prolog/tests.
;;;;
;;;; Loads the test suites in-process and runs them.  Paths are resolved from
;;;; the ASDF system source directory (not *LOAD-TRUENAME*), so this works even
;;;; when tests.lisp is loaded as a compiled fasl from the ASDF output cache.

#.(progn (require :asdf) nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  (unless (find-package "FX.PROLOG")
    (unless (asdf:find-system "cl-prolog/tests" nil)
      (asdf:load-asd
       (merge-pathnames "cl-prolog.asd"
                        (or *load-truename* *load-pathname*
                            (error "Cannot determine the repository path.")))))
    (asdf:load-system :cl-prolog)))

;; The shared bootstrap defines the CL-PROLOG.BOOTSTRAP loader that the test
;; support files use to pull in fixtures and table-driven assertion macros.
(load (asdf:system-relative-pathname :cl-prolog/tests "scripts/bootstrap.lisp")
      :verbose nil :print nil)

;; LOAD-TEST-SOURCES loads tests/support.lisp (which defines FX.PROLOG.TESTS)
;; followed by every suite file listed in the bootstrap manifest.
(funcall (or (find-symbol "LOAD-TEST-SOURCES" "CL-PROLOG.BOOTSTRAP")
             (error "CL-PROLOG.BOOTSTRAP:LOAD-TEST-SOURCES is unavailable.")))

;; The script JSON-contract tests spawn a tree of fresh SBCL images, which is
;; too heavy for sandboxed environments (nix build sandboxes, CI runners).  CI
;; exercises the scripts directly as workflow steps instead; set
;; CL_PROLOG_TEST_SCRIPTS=1 to include the meta-tests here.
(when (uiop:getenvp "CL_PROLOG_TEST_SCRIPTS")
  (funcall (or (find-symbol "LOAD-SCRIPT-CONTRACT-TESTS" "CL-PROLOG.BOOTSTRAP")
               (error "CL-PROLOG.BOOTSTRAP:LOAD-SCRIPT-CONTRACT-TESTS is unavailable."))))

(let ((runner (find-symbol "RUN-TESTS" "FX.PROLOG.TESTS")))
  (unless runner
    (error "Cannot resolve FX.PROLOG.TESTS::RUN-TESTS after loading test suites."))
  (funcall (symbol-function runner)))
