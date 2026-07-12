;;;; cl-weave-specific regression coverage for relational behavior.

(in-package #:cl-prolog.tests)

(defvar *weave-family-rulebase* nil)

(cl-weave:describe-sequential "cl-weave relational regression"
  (cl-weave:before-each
    (setf *weave-family-rulebase* (make-family-rulebase)))
  (cl-weave:after-each
    (setf *weave-family-rulebase* nil))

  (cl-weave:it-each
      (((a b) (c) (a b c))
       (() (a) (a))
       ((left) () (left)))
      "append returns ~S for ~S and ~S" (left right expected)
    (cl-weave:expect-has-assertions)
    (cl-weave:expect
     (query-prolog *weave-family-rulebase* `(append ,left ,right ?result))
     :to-equal `(((?result . ,expected)))))

  (cl-weave:it-property
      "append preserves every generated pair of finite lists"
      ((left (cl-weave:gen-list (cl-weave:gen-member '(a b c)) :max-length 5))
       (right (cl-weave:gen-list (cl-weave:gen-member '(a b c)) :max-length 5)))
    (cl-weave:expect-has-assertions)
    (let ((joined (append left right)))
      (cl-weave:expect
     (query-prolog *weave-family-rulebase* `(append ,left ,right ?result))
     :to-equal `(((?result . ,joined))))))

  (cl-weave:it "repeated ancestor queries complete within the latency budget"
      (:timeout-ms 5000)
    (cl-weave:expect-has-assertions)
    (let ((result (cl-weave:benchmark (:warmup 1 :samples 5 :iterations 40)
                    (cl-weave:expect
                     (query-prolog-first *weave-family-rulebase*
                                         '(ancestor tom ?who))
                     :to-equal '((?who . bob))))))
      (cl-weave:expect (length (cl-weave:benchmark-result-samples result))
                       :to-be 5)
      (cl-weave:expect (cl-weave:median-ms result)
                       :to-be-greater-than-or-equal 0))))
