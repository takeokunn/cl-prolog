;;;; Query-level contract for ISO character-code I/O.

(in-package #:cl-prolog.tests)

(deftest io-code-builtins-use-current-streams ()
  (with-io-rulebase (rulebase input output) "A"
    (assert-query rulebase (cl-prolog::peek_code ?code) :ordered (((?code . 65))))
    (assert-query rulebase (cl-prolog::get_code ?code) :ordered (((?code . 65))))
    (assert-query rulebase (cl-prolog::peek_code ?eof) :ordered (((?eof . -1))))
    (assert-query rulebase (cl-prolog::get_code ?eof) :ordered (((?eof . -1))))
    (assert-query rulebase (cl-prolog::put_code 66) :succeeds)
    (is-equal "B" (get-output-stream-string output))))

(deftest io-code-builtins-use-explicit-streams ()
  (with-io-rulebase (rulebase input output) "C"
    (assert-query rulebase
                  (cl-prolog::peek_code cl-prolog::user_input ?code)
                  :ordered (((?code . 67))))
    (assert-query rulebase
                  (cl-prolog::get_code cl-prolog::user_input ?code)
                  :ordered (((?code . 67))))
    (assert-query rulebase
                  (cl-prolog::put_code cl-prolog::user_output 68)
                  :succeeds)
    (is-equal "D" (get-output-stream-string output))))

(deftest io-code-end-of-stream-state-progresses-from-at-to-past ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query
     rulebase
     (cl-prolog::stream_property
      cl-prolog::user_input (cl-prolog::end_of_stream ?state))
     :ordered (((?state . cl-prolog::at))))
    (assert-query rulebase (cl-prolog::get_code ?code)
                  :ordered (((?code . -1))))
    (assert-query
     rulebase
     (cl-prolog::stream_property
      cl-prolog::user_input (cl-prolog::end_of_stream ?state))
     :ordered (((?state . cl-prolog::past))))
    (assert-query rulebase (cl-prolog::get_code ?code)
                  :ordered (((?code . -1))))))

(deftest-table io-code-builtins-report-iso-errors ()
  (:signals (query-prolog (make-rulebase) '(cl-prolog::put_code ?code)))
  (:signals (query-prolog (make-rulebase) '(cl-prolog::put_code atom)))
  (:signals (query-prolog (make-rulebase) '(cl-prolog::put_code -1))))
