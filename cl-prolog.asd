(asdf:defsystem #:cl-prolog
  :description "A small, dependency-free Common Lisp Prolog engine."
  :author "takeokunn"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :components ((:file "package")
               (:file "data")
               (:file "unification")
               (:file "engine")
               (:file "dsl")
               (:file "dcg"))
  :in-order-to ((asdf:test-op (asdf:test-op "cl-prolog/tests"))))

(asdf:defsystem #:cl-prolog/tests
  :depends-on (#:cl-prolog)
  :serial t
  :components ((:file "tests"))
  :perform (asdf:test-op (op c)
             (declare (ignore op c))
             (uiop:symbol-call "FX.PROLOG.TESTS" "RUN-TESTS")))
