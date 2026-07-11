(
 :format-version 1
 :project-version "0.2.0"
 :packages
 ((:name "CL-PROLOG"
   :nicknames ()
   :exports ("!" "#<" "#=" "#=<" "#>" "#>=" "#\\=" "*MAX-PROLOG-DEPTH*" ".."
              "<" "=.." "=:=" "=<" "==" "=\\=" ">" ">=" "@<" "@=<" "@>" "@>="
              "ABOLISH" "ALL_DIFFERENT" "ARG" "ARITHMETIC-ERROR-EXPRESSION"
              "ARITHMETIC-ERROR-REASON" "ARITHMETIC-EVALUATION-ERROR"
              "ASSERTA" "ASSERTZ" "ATOM" "ATOMIC" "BAGOF" "CALL" "CALLABLE"
              "CALL_CLEANUP" "CATCH" "CLAUSE" "CLAUSE-BODY" "CLAUSE-HEAD"
              "CLAUSE-P" "COMPARE" "COMPOUND" "CONSULT" "CONSULT-PROLOG"
              "COPY_TERM" "CURRENT_PREDICATE" "DCG-ALT" "DCG-ERROR-RECOVERY"
              "DCG-OPT" "DCG-PLUS" "DCG-STAR" "DCG-TOKEN-MATCH"
              "DCG-TOKEN-MATCH-VALUE" "DEF-DCG-RULE" "DEF-RULE"
              "DEFINE-FOREIGN-PREDICATE" "DEFINE-RULEBASE"
              "ENSURE-PROLOG-LOADED" "ENSURE_LOADED" "EXTEND-RULEBASE" "FAIL"
              "FALSE" "FINDALL" "FLOAT" "FORALL" "FRESH-LOGIC-VARIABLE"
              "FUNCTOR" "GROUND" "IF-THEN-ELSE" "IN" "INTEGER"
              "INVALID-GOAL-ERROR" "INVALID-GOAL-ERROR-GOAL"
              "INVALID-MAX-DEPTH-ERROR" "INVALID-MAX-DEPTH-ERROR-VALUE" "IS"
              "KEYSORT" "LABELING" "LOAD_FILES" "LOGIC-SUBSTITUTE"
              "LOGIC-VAR-P" "MAKE-CLAUSE" "MAKE-RULEBASE"
              "MAP-PROLOG-SOLUTIONS" "NONVAR" "NUMBER" "NUMBERVARS" "ONCE"
              "PARSE-PROLOG" "PHRASE" "PHRASE-ALL" "PROLOG"
              "PROLOG-DEPTH-LIMIT-EXCEEDED"
              "PROLOG-DEPTH-LIMIT-EXCEEDED-GOAL" "PROLOG-DOMAIN-ERROR"
              "PROLOG-EVALUATION-ERROR" "PROLOG-EXCEPTION"
              "PROLOG-EXCEPTION-TERM" "PROLOG-EXISTENCE-ERROR"
              "PROLOG-INSTANTIATION-ERROR" "PROLOG-MATCH"
              "PROLOG-PERMISSION-ERROR" "PROLOG-RESOURCE-ERROR"
              "PROLOG-RUNTIME-ERROR" "PROLOG-SUCCEEDS-P" "PROLOG-TERM-STRING"
              "PROLOG-TYPE-ERROR" "QUERY-PROLOG" "QUERY-PROLOG-FIRST"
              "READ-PROLOG-CLAUSE" "READ-PROLOG-TERM" "REPEAT" "RETRACT"
              "RETRACTALL" "RULEBASE" "RULEBASE-INSERT-CLAUSE!" "RULEBASE-P"
              "RULEBASE-VISIBLE-CLAUSES" "SETOF" "SETUP_CALL_CLEANUP"
              "SOFT-IF-THEN-ELSE" "SOLUTION-BINDING" "SORT" "TERM_VARIABLES"
              "THROW" "TRUE" "UNIFY" "UNIFY_WITH_OCCURS_CHECK" "VAR"
              "WITH-PROLOG-QUERY" "WRITE-PROLOG-TERM" "\\+" "\\=" "\\==")))
 :asdf-systems
 ("cl-prolog"
  "cl-prolog/examples"
  "cl-prolog/benchmark")
 :fresh-image-systems
 ("cl-prolog/examples"
  "cl-prolog/benchmark")
 :alias-files ()
 :example-scripts
 ("examples/quick-start.lisp"
  "examples/family-tree.lisp"
  "examples/relational-lists.lisp")
 :core-docs
 ("README.md"
  "docs/api-reference.md"
  "docs/architecture.md"
  "docs/oss-readiness-audit.md"
  "docs/performance.md"
  "docs/public-contract-verifier.md"
  "docs/quality-gates.md"
  "docs/release-audit.md"
  "docs/release-checklist.md"
  "docs/troubleshooting.md")
 :policy-files
 ("CHANGELOG.md"
  "CODE_OF_CONDUCT.md"
  "CONTRIBUTING.md"
  "SECURITY.md"
  "SUPPORT.md")
 :ci-workflows
 ((:path ".github/workflows/ci.yml"
   :required-substrings ("timeout-minutes:"
                         "sbcl --script scripts/run-tests-noasdf.lisp"
                         "(asdf:test-system :cl-prolog)"
                         "sbcl --script scripts/verify-public-contract.lisp --json"
                         "sbcl --script scripts/benchmark.lisp --json --iterations 10"
                         "sbcl --script examples/quick-start.lisp"
                         "sbcl --script examples/family-tree.lisp"
                         "sbcl --script examples/relational-lists.lisp"
                         "nix flake check --print-build-logs")
   :minimum-counts (("timeout-minutes:" . 7))
   :message "CI workflow keeps documented release gates and explicit timeout declarations"))
 :content-contracts
 ((:path "scripts/bootstrap.lisp"
   :require-git-tracked nil
   :required-substrings ("#:run-command-capture"
                         "defun perl-timeout-wrapper"
                         "(perl-timeout-wrapper timeout)"
                         "-MPOSIX=:sys_wait_h")
   :message "shared subprocess runner keeps explicit timeout machinery")
  (:path "scripts/coverage.lisp"
   :required-substrings ("*command-timeouts*"
                         "*compile-timeout*"
                         "run-command-stream"
                         ":timeout *compile-timeout*")
   :message "coverage compilation keeps an explicit subprocess timeout")
  (:path "scripts/verify-public-contract-data.lisp"
   :require-git-tracked nil
   :required-substrings ("*command-timeouts*"
                         "sbcl-fresh-load")
   :message "public-contract verifier defines explicit subprocess timeouts")
  (:path "scripts/verify-public-contract-checks.lisp"
   :require-git-tracked nil
   :required-substrings (
                         "(command-timeout \"git\")"
                         "(command-timeout \"sbcl-script\")"
                         "(verify-target system-name \"fresh\")")
   :message "public-contract verifier keeps explicit timeouts for every subprocess class")
  (:path "scripts/release-audit-data.lisp"
   :require-git-tracked nil
   :required-substrings ("*git-command-timeout*"
                         "defun check-timeout")
   :message "release audit defines explicit subprocess timeouts")
  (:path "scripts/release-audit-checks.lisp"
   :require-git-tracked nil
   :required-substrings (
                         "(check-timeout \"core\")"
                         "(check-timeout \"tests\")"
                         "(check-timeout \"benchmarks\")"
                         "(check-timeout \"nix\")")
   :message "release audit keeps explicit timeouts for every subprocess class")
  (:path "tests/scripts.lisp"
   :required-substrings (":timeout 120")
   :message "script-contract meta-tests keep an explicit nested-script timeout"))
 :forbidden-content
  ((:id "retired-public-surface-references"
   ;; CHANGELOG.md is exempt: it legitimately names removed symbols.
   :paths ("README.md"
           "CONTRIBUTING.md"
           "SUPPORT.md"
           "docs/api-reference.md"
           "docs/architecture.md"
           "docs/oss-readiness-audit.md"
           "docs/performance.md"
           "docs/public-contract-verifier.md"
           "docs/quality-gates.md"
           "docs/release-audit.md"
           "docs/release-checklist.md"
           "docs/troubleshooting.md")
   :substrings ("cl-cc/prolog"
                "cl-prolog2.asd"
                "cl-cc-prolog.asd"
                "substitute-term"
                "*max-proof-depth*"
                "query-prolog-cps"
                "prolog-succeeds-p-cps"
                "unify-failed-p"
                "scripts/verify-replacements.lisp"
                "docs/stability-policy.md"
                "docs/migration-guide.md"
                "docs/replacement-verifier.md")
   :message "retired public surface and deleted artifact references must not appear in shipped documentation"))
 :stable-scripts
 ("scripts/benchmark.lisp"
  "scripts/coverage.lisp"
  "scripts/release-audit.lisp"
  "scripts/verify-public-contract.lisp"))
