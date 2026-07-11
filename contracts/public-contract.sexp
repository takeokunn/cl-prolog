(
 :format-version 1
 :project-version "0.2.0"
 :packages
 ((:name "FX.PROLOG"
   :nicknames ()
   :exports ("!"
             "ABOLISH"
             "!="
             "<"
             "/="
             "*GLOBAL-RULEBASE*"
             "*MAX-PROLOG-DEPTH*"
             "=:="
             "=<"
             "=\\="
             ">"
             ">="
             "ARITHMETIC-ERROR-EXPRESSION"
             "ARITHMETIC-ERROR-REASON"
             "ARITHMETIC-EVALUATION-ERROR"
             "ASSERTA"
             "ASSERTZ"
             "ASSERT-FACT!"
             "ASSERT-RULE!"
             "CALL"
             "CLAUSE"
             "CLEAR-GLOBAL-RULEBASE!"
             "DCG-ALT"
             "DCG-ERROR-RECOVERY"
             "DCG-OPT"
             "DCG-PLUS"
             "DCG-STAR"
             "DCG-TOKEN-MATCH"
             "DCG-TOKEN-MATCH-VALUE"
             "DEF-DCG-RULE"
             "DEF-RULE"
             "DEFINE-BUILTIN"
             "DEFINE-RULEBASE"
             "EXTEND-RULEBASE"
             "FACT"
             "FACT-ARGS"
             "FACT-PREDICATE"
             "FRESH-LOGIC-VARIABLE"
             "INVALID-GOAL-ERROR"
             "INVALID-GOAL-ERROR-GOAL"
             "IS"
             "LOGIC-SUBSTITUTE"
             "LOGIC-VAR-P"
             "MAKE-FACT"
             "MAKE-RULE"
             "MAKE-RULEBASE"
             "MAP-PROLOG-SOLUTIONS"
             "ONCE"
             "PHRASE"
             "PHRASE-ALL"
             "PREDICATE-TRUE-P"
             "PROLOG"
             "PROLOG-MATCH"
             "PROLOG-SUCCEEDS-P"
             "QUERY-PROLOG"
             "QUERY-PROLOG-FIRST"
             "REPEAT"
             "RETRACT"
             "RULE"
             "RULE-BODY"
             "RULE-HEAD"
             "RULEBASE"
             "RULEBASE-FACTS"
             "RULEBASE-P"
             "RULEBASE-RULES"
             "SOLUTION-BINDING"
             "TRUE"
             "UNIFY"
             "WITH-PROLOG-QUERY")))
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
                         "perl-timeout-wrapper")
   :message "coverage compilation keeps an explicit subprocess timeout")
  (:path "scripts/verify-public-contract-main.lisp"
   :require-git-tracked nil
   :required-substrings ("*command-timeouts*"
                         "(command-timeout \"git\")"
                          "(command-timeout \"sbcl-script\")"
                         "\"sbcl-fresh-load\""
                         "(verify-target system-name \"fresh\")")
   :message "public-contract verifier keeps explicit timeouts for every subprocess class")
  (:path "scripts/release-audit-main.lisp"
   :require-git-tracked nil
   :required-substrings ("*git-command-timeout*"
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
