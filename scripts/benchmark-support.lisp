(defpackage #:cl-prolog.benchmark
  (:use #:cl)
  (:export #:available-scenarios
           #:run-benchmark-scenario
           #:run-benchmark-suite))

(in-package #:cl-prolog.benchmark)

(defun available-scenarios ()
  '("ancestor-first" "append-first" "dcg-phrase"))

(defun nanoseconds-from-internal-time (ticks)
  (round (* ticks 1000000000) internal-time-units-per-second))

(defun warmup-runs ()
  1)

(defun scenario-ancestor-first ()
  (let* ((rulebase
           (fx.prolog:make-rulebase
            :facts (list (fx.prolog:make-fact :predicate 'parent :args '(tom bob))
                         (fx.prolog:make-fact :predicate 'parent :args '(bob alice))
                         (fx.prolog:make-fact :predicate 'parent :args '(alice eve)))
            :rules (list (fx.prolog:make-rule :head '(ancestor ?x ?y)
                                              :body '((parent ?x ?y)))
                         (fx.prolog:make-rule :head '(ancestor ?x ?y)
                                              :body '((parent ?x ?z)
                                                     (ancestor ?z ?y)))))))
    (lambda ()
      (let ((result (fx.prolog:query-prolog-first rulebase '(ancestor tom ?who))))
        (unless (equal '((?who . bob)) result)
          (error "Unexpected ancestor-first benchmark result: ~S" result))
        result))))

(defun scenario-append-first ()
  (let ((rulebase
          (fx.prolog:prolog
            ((append () ?ys ?ys))
            ((append (?x . ?xs) ?ys (?x . ?zs))
             (append ?xs ?ys ?zs)))))
    (lambda ()
      (let ((result (fx.prolog:query-prolog-first rulebase '(append ?left ?right (a b c)))))
        (unless (equal '((?left) (?right a b c)) result)
          (error "Unexpected append-first benchmark result: ~S" result))
        result))))

(defun scenario-dcg-phrase ()
  (fx.prolog:clear-global-rulebase!)
  (fx.prolog:def-dcg-rule benchmark-int
    (terminal :t-int))
  (lambda ()
    (let ((result (fx.prolog:phrase 'benchmark-int '((:t-int . 1)))))
      (unless (null result)
        (error "Unexpected dcg-phrase benchmark result: ~S" result))
      result)))

(defun make-scenario-runner (scenario)
  (cond
    ((string= scenario "ancestor-first")
     (scenario-ancestor-first))
    ((string= scenario "append-first")
     (scenario-append-first))
    ((string= scenario "dcg-phrase")
     (scenario-dcg-phrase))
    (t
     (error "Unknown benchmark scenario: ~A" scenario))))

(defun run-measured-iterations (thunk iterations)
  (let ((total-ns 0)
        (last-result nil))
    (dotimes (index iterations)
      (declare (ignore index))
      (let ((start (get-internal-real-time)))
        (setf last-result (funcall thunk))
        (incf total-ns
              (nanoseconds-from-internal-time
               (- (get-internal-real-time) start)))))
    (list :iterations iterations
          :total-ns total-ns
          :avg-ns (if (plusp iterations)
                      (round total-ns iterations)
                      0)
          :last-result (prin1-to-string last-result))))

(defun run-benchmark-scenario (scenario iterations)
  (unless (member scenario (available-scenarios) :test #'string=)
    (error "Unknown benchmark scenario: ~A" scenario))
  (unless (and (integerp iterations) (plusp iterations))
    (error "Iterations must be a positive integer, got: ~S" iterations))
  (let ((thunk (make-scenario-runner scenario)))
    (dotimes (index (warmup-runs))
      (declare (ignore index))
      (funcall thunk))
    (append (list :scenario scenario :ok t)
            (run-measured-iterations thunk iterations))))

(defun run-benchmark-suite (scenarios iterations)
  (mapcar (lambda (scenario)
            (run-benchmark-scenario scenario iterations))
          scenarios))
