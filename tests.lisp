(eval-when (:compile-toplevel :load-toplevel :execute)
  (labels ((runtime-call (package-name symbol-name &rest args)
             (let ((symbol (find-symbol symbol-name package-name)))
               (unless symbol
                 (error "Cannot resolve ~A::~A while bootstrapping tests.lisp."
                        package-name
                        symbol-name))
               (apply (symbol-function symbol) args))))
    (require :asdf)
    (unless (find-package "FX.PROLOG")
      (let* ((script-path (or *load-truename*
                              (error "Cannot determine tests.lisp path from *LOAD-TRUENAME*.")))
             (repo-root (runtime-call "UIOP" "PATHNAME-DIRECTORY-PATHNAME" script-path)))
        (runtime-call "ASDF" "LOAD-ASD" (merge-pathnames "cl-prolog.asd" repo-root))
        (runtime-call "ASDF" "LOAD-SYSTEM" :cl-prolog)))))

(defun test-file (relative-path)
  (asdf:system-relative-pathname :cl-prolog/tests relative-path))

(load (test-file "tests/support.lisp") :verbose nil :print nil)
(load (test-file "tests/core.lisp") :verbose nil :print nil)
(load (test-file "tests/engine.lisp") :verbose nil :print nil)
(load (test-file "tests/dcg.lisp") :verbose nil :print nil)

;; The script JSON-contract tests spawn a tree of fresh SBCL images (the
;; contract verifier itself spawns more), which is too heavy for sandboxed
;; or memory-constrained environments (nix build sandboxes, CI runners).
;; CI exercises the scripts directly as workflow steps instead; set
;; CL_PROLOG_TEST_SCRIPTS=1 to include the meta-tests here.
(when (uiop:getenvp "CL_PROLOG_TEST_SCRIPTS")
  (load (test-file "tests/scripts.lisp") :verbose nil :print nil))

(let ((runner (find-symbol "RUN-TESTS" "FX.PROLOG.TESTS")))
  (unless runner
    (error "Cannot resolve FX.PROLOG.TESTS::RUN-TESTS after loading test suites."))
  (funcall (symbol-function runner)))
