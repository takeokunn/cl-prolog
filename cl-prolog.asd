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
  :version "0.7.0"
  :pathname "src"
  :serial t
  :components ((:file "package")
                 (:file "operator-table")
                 (:file "module-system")
                 (:file "source-registry")
                 (:file "data")
                 (:file "table-variant")
                 (:file "unification")
                 (:file "lexer")
                 (:file "lexer-operator-lexemes")
                 (:file "lexer-tokenizer")
                 (:file "grammar")
                 (:file "term-writer")
               (:file "engine")
               (:file "io-context")
               (:file "prover")
               (:file "tabling")
               (:module "builtins"
                :serial t
                :components ((:file "core")
                             (:file "control")
                             (:file "collection")
                             (:file "dynamic")
                             (:file "arithmetic")
                             (:file "list")
                             (:file "text-conversion")
                             (:file "atom-ops")
                             (:file "atom-number-conversion")
                             (:file "operator")
                             (:file "io")
                             (:file "io-streams")
                             (:file "io-code")))
               (:file "fd-store")
               (:file "builtins/fd")
               (:file "term-inspect")
               (:file "term-compare")
               (:file "term-construct")
               (:file "dcg-runtime")
               (:file "query")
               (:file "source-io")
               (:file "source-directives")
               (:file "source-rollback")
               (:file "source-loader")
               (:file "dsl-compiler")
               (:file "dsl")
               (:file "dcg"))
  :in-order-to ((asdf:test-op (asdf:test-op "cl-prolog/tests"))))

(asdf:defsystem #:cl-prolog/weave
  :description "cl-weave helpers for testing cl-prolog queries."
  :depends-on (#:cl-prolog #:cl-weave)
  :pathname "src"
  :components ((:file "weave")))

(asdf:defsystem #:cl-prolog/tests
  :depends-on (#:cl-prolog/weave)
  :pathname "tests"
  :serial t
  :components ((:file "support")
               (:module "support-files"
                :pathname "support"
                :serial t
                :components ((:file "core")
                             (:file "query")
                             (:file "fixtures")))
               (:file "unification")
               (:file "operator-table")
               (:file "parser")
               (:file "term-writer")
               (:file "io-context")
               (:file "source-loader")
               (:file "source-loader-transactions")
               (:file "source-loader-limits")
               (:file "engine-surface")
               (:file "engine-queries")
               (:file "builtin-collections")
               (:file "builtin-list")
               (:file "builtin-dynamic-database")
               (:file "builtin-arithmetic-and-flags")
               (:file "engine-runtime")
               (:file "engine-runtime-index-and-depth")
               (:file "engine-runtime-foreign-and-registration")
               (:file "engine-runtime-error-contract")
               (:file "builtin-term")
               (:file "builtin-atom")
               (:file "builtin-operator")
               (:file "builtin-io-terms")
               (:file "builtin-io-streams-lifecycle")
               (:file "builtin-io-open-errors")
               (:file "builtin-io-code")
               (:file "builtin-fd")
               (:file "module-system")
               (:file "dcg")
               (:file "weave-public")
               (:file "weave-quality"))
  :perform (asdf:test-op (op c)
             (declare (ignore op c))
             (unless (uiop:symbol-call "CL-WEAVE" "RUN-ALL" :reporter :spec)
               (error "cl-prolog cl-weave test suite failed."))))

(asdf:defsystem #:cl-prolog/examples
  :depends-on (#:cl-prolog)
  :description "Runnable examples for cl-prolog."
  :serial t
  :pathname "examples"
  :components ((:file "quick-start")
               (:file "family-tree")
               (:file "relational-lists")))
