(in-package #:cl-prolog.tests)

(deftest-io-variants io-read-write-facades-share-term-semantics
    ((rulebase input output) "pair(X, X).")
  ("current stream"
   (assert-query rulebase
                 (cl-prolog::read ?term)
                 => (((?term . (cl-prolog::pair
                                cl-prolog::?x cl-prolog::?x)))))
   (assert-query rulebase
                 (cl-prolog::write (cl-prolog::$var 0))
                 :succeeds)
   (assert-query rulebase
                 (cl-prolog::writeq cl-prolog::|Mary Jane|)
                 :succeeds)
   (is-equal "A'Mary Jane'" (get-output-stream-string output)))
  ("explicit stream"
   (assert-query rulebase
                 (cl-prolog::read cl-prolog::user_input ?term)
                 => (((?term . (cl-prolog::pair
                                cl-prolog::?x cl-prolog::?x)))))
   (assert-query rulebase
                 (cl-prolog::write cl-prolog::user_output
                                   (cl-prolog::$var 0))
                 :succeeds)
   (assert-query rulebase
                 (cl-prolog::writeq cl-prolog::user_output
                                    cl-prolog::|Mary Jane|)
                 :succeeds)
   (is-equal "A'Mary Jane'" (get-output-stream-string output))))

(deftest-io-variants io-character-lookahead-supports-current-and-explicit-streams
    ((rulebase input output) "ab")
  ("current stream"
   (assert-query rulebase
                 (cl-prolog::peek_char ?value)
                 => (((?value . cl-prolog::|a|))))
   (assert-query rulebase
                 (cl-prolog::get_char ?value)
                 => (((?value . cl-prolog::|a|)))))
  ("explicit stream"
   (assert-query rulebase
                 (cl-prolog::peek_char cl-prolog::user_input ?value)
                 => (((?value . cl-prolog::|a|))))
   (assert-query rulebase
                 (cl-prolog::get_char cl-prolog::user_input ?value)
                 => (((?value . cl-prolog::|a|))))))
