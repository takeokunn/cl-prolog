(asdf:defsystem #:cl-prolog
  :description "A small, dependency-free Common Lisp Prolog engine."
  :long-description "A macro-first Common Lisp Prolog engine with CPS proof search, an extensible builtin registry, and a compact rule DSL."
  :author "takeokunn"
  :maintainer "takeokunn"
  :license "MIT"
  :homepage "https://github.com/takeokunn/cl-prolog"
  :bug-tracker "https://github.com/takeokunn/cl-prolog/issues"
  :source-control (:git "https://github.com/takeokunn/cl-prolog.git")
  :long-name "cl-prolog"
  :version "0.2.0"
  :pathname "src"
  :serial t
  :components ((:file "package")
                 (:file "data")
                 (:file "unification")
                 (:file "operator-table")
                 (:file "parser")
               (:file "engine")
               (:file "prover")
               (:module "builtins"
                :serial t
                :components ((:file "core")
                             (:file "control")
                             (:file "collection")
                             (:file "dynamic")
                             (:file "arithmetic")
                             (:file "list")))
               (:file "builtin-term")
               (:file "dcg-runtime")
               (:file "query")
               (:file "dsl-compiler")
               (:file "dsl")
               (:file "dcg"))
  :in-order-to ((asdf:test-op (asdf:test-op "cl-prolog/tests"))))

(asdf:defsystem #:cl-prolog/tests
  :depends-on (#:cl-prolog)
  :pathname ""
  :serial t
  :components ((:file "tests")))

(asdf:defsystem #:cl-prolog/weave-tests
  :depends-on (#:cl-prolog #:cl-weave)
  :description "cl-weave (Vitest-shaped) test suite for the public cl-prolog surface."
  :serial t
  :pathname "tests-weave"
  :components ((:file "package")
               (:file "prolog"))
  :perform (asdf:test-op (op c)
             (declare (ignore op c))
             (unless (uiop:symbol-call "CL-WEAVE" "RUN-ALL" :reporter :spec)
               (uiop:quit 1))))

(asdf:defsystem #:cl-prolog/examples
  :depends-on (#:cl-prolog)
  :description "Runnable example scripts for cl-prolog."
  :serial t
  :pathname "examples"
  :components ((:file "quick-start")
               (:file "family-tree")
               (:file "relational-lists")))

(asdf:defsystem #:cl-prolog/benchmark
  :depends-on (#:cl-prolog)
  :description "Support helpers and scenario definitions for cl-prolog benchmarks."
  :serial t
  :pathname "scripts"
  :components ((:file "benchmark-support")))
