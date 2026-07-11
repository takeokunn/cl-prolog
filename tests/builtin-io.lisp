;;;; Query-level contract for ISO-style stream builtins.

(in-package #:cl-prolog.tests)

(defmacro with-io-rulebase ((rulebase input output) input-text &body body)
  `(let* ((,input (make-string-input-stream ,input-text))
          (,output (make-string-output-stream))
          (context (cl-prolog::make-prolog-io-context
                    :input ,input :output ,output
                    :error-output (make-string-output-stream)))
          (,rulebase (make-rulebase :io-context context)))
     (unwind-protect
          (progn ,@body)
       (cl-prolog::%close-all-owned-prolog-streams! context))))

(deftest io-builtins-use-rulebase-standard-streams ()
  (with-io-rulebase (rulebase input output) "a"
    (assert-query rulebase (cl-prolog::current_input ?stream) :succeeds)
    (assert-query rulebase (cl-prolog::get_char ?character)
                  => (((?character . cl-prolog::|a|))))
    (assert-query rulebase (cl-prolog::at_end_of_stream) :succeeds)
    (assert-query rulebase (cl-prolog::put_char z) :succeeds)
    (assert-query rulebase (cl-prolog::nl) :succeeds)
    (assert-query rulebase (cl-prolog::flush_output) :succeeds)
    (is-equal (format nil "Z~%") (get-output-stream-string output))))

(deftest io-builtins-support-explicit-streams ()
  (with-io-rulebase (rulebase input output) "x"
    (assert-query rulebase
                  (cl-prolog::get_char cl-prolog::user_input ?character)
                  => (((?character . cl-prolog::|x|))))
    (assert-query rulebase
                  (cl-prolog::put_char cl-prolog::user_output q)
                  :succeeds)
    (assert-query rulebase
                  (cl-prolog::at_end_of_stream cl-prolog::user_input)
                  :succeeds)))

(deftest io-builtins-report-eof-and-reject-unsupported-writer-options ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase (cl-prolog::get_char ?character)
                  => (((?character . cl-prolog::end_of_file))))
    (assert-query rulebase
                  (cl-prolog::write_term cl-prolog::user_output hello
                                         ((cl-prolog::quoted maybe)))
                  :signals)))

(deftest io-read-term-current-stream-reports-variables ()
  (with-io-rulebase (rulebase input output) "pair(X, X, Y)."
    (assert-query
     rulebase
     (cl-prolog::read_term
      ?term ((cl-prolog::variables ?variables)
             (cl-prolog::variable_names ?names)))
     => (((?term . (cl-prolog::pair cl-prolog::?x cl-prolog::?x cl-prolog::?y))
          (?variables . (cl-prolog::?x cl-prolog::?y))
          (?names . ((cl-prolog::= cl-prolog::|X| cl-prolog::?x)
                     (cl-prolog::= cl-prolog::|Y| cl-prolog::?y))))))))

(deftest io-read-term-validates-syntax-error-policy ()
  (with-io-rulebase (rulebase input output) "broken( ."
    (assert-query rulebase
                  (cl-prolog::read_term
                   ?term ((cl-prolog::syntax_errors cl-prolog::fail)))
                  :fails))
  (with-io-rulebase (rulebase input output) "ok."
    (assert-query rulebase
                  (cl-prolog::read_term
                   ?term ((cl-prolog::syntax_errors unsupported)))
                  :signals)))

(deftest io-write-term-current-stream-honors-ignore-ops ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase
                  (cl-prolog::write_term
                   (cl-prolog::+ 1 2)
                   ((cl-prolog::quoted cl-prolog::true)
                    (cl-prolog::ignore_ops cl-prolog::true)
                    (cl-prolog::numbervars cl-prolog::false)))
                  :succeeds)
    (is-equal "'+'(1,2)" (get-output-stream-string output))))

(deftest io-write-term-current-stream-honors-quoted ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase
                  (cl-prolog::write_term
                   cl-prolog::|Mary Jane|
                   ((cl-prolog::quoted cl-prolog::true)))
                  :succeeds)
    (assert-query rulebase
                  (cl-prolog::write_term
                   cl-prolog::|Mary Jane|
                   ((cl-prolog::quoted cl-prolog::false)))
                  :succeeds)
    (is-equal "'Mary Jane'Mary Jane" (get-output-stream-string output))))

(deftest io-write-term-explicit-stream-honors-numbervars ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase
                  (cl-prolog::write_term
                   cl-prolog::user_output
                   (cl-prolog::$var 25)
                   ((cl-prolog::numbervars cl-prolog::true)))
                  :succeeds)
    (assert-query rulebase
                  (cl-prolog::write_term
                   cl-prolog::user_output
                   (cl-prolog::$var 26)
                   ((cl-prolog::numbervars cl-prolog::true)))
                  :succeeds)
    (assert-query rulebase
                  (cl-prolog::write_term
                   cl-prolog::user_output
                   (cl-prolog::$var 0)
                   ((cl-prolog::numbervars cl-prolog::false)
                    (cl-prolog::quoted cl-prolog::true)))
                  :succeeds)
    (is-equal "ZA1'$VAR'(0)" (get-output-stream-string output))))

(deftest io-write-term-combines-ignore-ops-and-quoting ()
  (with-io-rulebase (rulebase input output) ""
    (assert-query rulebase
                  (cl-prolog::write_term
                   (cl-prolog::+ (cl-prolog::$var 0) cl-prolog::|Mary Jane|)
                   ((cl-prolog::quoted cl-prolog::false)
                    (cl-prolog::ignore_ops cl-prolog::true)
                    (cl-prolog::numbervars cl-prolog::true)))
                  :succeeds)
    (is-equal "+(A,Mary Jane)" (get-output-stream-string output))))

(deftest io-write-term-rejects-invalid-boolean-options ()
  (dolist (option '(cl-prolog::quoted cl-prolog::ignore_ops
                    cl-prolog::numbervars))
    (with-io-rulebase (rulebase input output) ""
      (signals-error
       (query-prolog rulebase
                     `(cl-prolog::write_term hello ((,option maybe))))))))

(deftest rulebase-copies-isolate-io-context ()
  (with-io-rulebase (rulebase input output) ""
    (let ((copy (cl-prolog::%copy-rulebase rulebase)))
      (is (not (eq (cl-prolog::rulebase-io-context rulebase)
                   (cl-prolog::rulebase-io-context copy)))))))
