;;;; Query-level contract for ISO character-code I/O.

(in-package #:cl-prolog.tests)

(deftest io-code-builtins-use-current-streams ()
  (with-io-rulebase (rulebase input output) "A"
    (declare (ignore input))
    (assert-query rulebase (cl-prolog::get_code ?code) => (((?code . 65))))
    (assert-query rulebase (cl-prolog::get_code ?eof) => (((?eof . -1))))
    (assert-query rulebase (cl-prolog::put_code 66) :succeeds)
    (is-equal "B" (get-output-stream-string output))))

(deftest io-code-builtins-use-explicit-streams ()
  (with-io-rulebase (rulebase input output) "C"
    (declare (ignore input))
    (assert-query rulebase
                  (cl-prolog::get_code cl-prolog::user_input ?code)
                  => (((?code . 67))))
    (assert-query rulebase
                  (cl-prolog::put_code cl-prolog::user_output 68)
                  :succeeds)
    (is-equal "D" (get-output-stream-string output))))

(deftest-table io-code-builtins-report-iso-errors ()
  (:signals (query-prolog (make-rulebase) '(cl-prolog::put_code ?code)))
  (:signals (query-prolog (make-rulebase) '(cl-prolog::put_code atom)))
  (:signals (query-prolog (make-rulebase) '(cl-prolog::put_code -1))))
