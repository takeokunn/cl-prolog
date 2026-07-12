;;;; cl-weave helpers for testing cl-prolog queries.

(defpackage #:cl-prolog/weave
  (:use #:cl)
  (:export
   #:assert-query
   #:deftest-queries))

(in-package #:cl-prolog/weave)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %split-query-assertion (kind arguments)
    (case kind
      ((:ordered :set :first)
       (unless arguments
         (error "~S query assertions require an expected value." kind))
       (values (first arguments) (rest arguments)))
      ((:succeeds :fails)
       (values nil arguments))
      (:signals
       (if (and arguments (not (keywordp (first arguments))))
           (values (first arguments) (rest arguments))
           (values nil arguments)))
      (otherwise
       (error "Unknown query assertion kind ~S." kind))))

  (defun %query-run-form (rulebase query kind options)
    (case kind
      (:first
       `(cl-prolog:query-prolog-first ,rulebase ',query ,@options))
      (:succeeds
       `(cl-prolog:prolog-succeeds-p ,rulebase ',query ,@options))
      (otherwise
       `(cl-prolog:query-prolog ,rulebase ',query ,@options))))

  (defun %query-assertion-form (rulebase query kind arguments)
    (multiple-value-bind (expected options)
        (%split-query-assertion kind arguments)
      (let ((run-form (%query-run-form rulebase query kind options)))
        (case kind
          (:ordered
           `(cl-weave:expect ,run-form :to-equal ',expected))
          (:set
           `(cl-weave:expect
             (%solution-multiset-equal-p ,run-form ',expected)
             :to-be-truthy))
          (:first
           `(cl-weave:expect ,run-form :to-equal ',expected))
          (:succeeds
           `(cl-weave:expect ,run-form :to-be-truthy))
          (:fails
           `(cl-weave:expect ,run-form :to-be-null))
          (:signals
           (if expected
               `(cl-weave:expect (lambda () ,run-form) :to-throw ',expected)
               `(cl-weave:expect (lambda () ,run-form) :to-throw)))))))

  (defun %parse-query-spec (spec)
    (unless (consp spec)
      (error "Query specification must be a list, got ~S." spec))
    (let* ((labelledp (stringp (first spec)))
           (label (and labelledp (first spec)))
           (body (if labelledp (rest spec) spec)))
      (unless (and (consp body) (consp (rest body)))
        (error "Query specification requires a query and assertion kind: ~S." spec))
      (values (or label (prin1-to-string (first body)))
              (first body)
              (second body)
              (cddr body)))))

(defun %solution-multiset-equal-p (actual expected)
  "Return true when ACTUAL and EXPECTED contain equal solutions in any order."
  (and (= (length actual) (length expected))
       (labels ((match (remaining wanted)
                  (if (endp wanted)
                      (endp remaining)
                      (let ((position (position (first wanted) remaining
                                                :test #'equal)))
                        (and position
                             (match (append (subseq remaining 0 position)
                                            (subseq remaining (1+ position)))
                                    (rest wanted)))))))
         (match actual expected))))

(defmacro assert-query (rulebase query kind &rest arguments)
  "Assert one literal QUERY against RULEBASE using a query assertion KIND."
  (%query-assertion-form rulebase query kind arguments))

(defmacro deftest-queries (name (rulebase-form) &body specs)
  "Define independent cl-weave cases for literal query SPECS.

Each case evaluates RULEBASE-FORM afresh. A spec may start with a string label;
otherwise the printed query is used as its label."
  `(cl-weave:describe-sequential ,(string name)
     ,@(mapcar
        (lambda (spec)
          (multiple-value-bind (label query kind arguments)
              (%parse-query-spec spec)
            `(cl-weave:it ,label
               (cl-weave:expect-has-assertions)
               (let ((rulebase ,rulebase-form))
                 ,(%query-assertion-form 'rulebase query kind arguments)))))
        specs)))
