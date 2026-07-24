;;;; DCG surface and runtime tests.

(in-package #:cl-prolog.tests)

(defun assert-phrase (expected-remainder expected-matched-p
                      rulebase rule-name input)
  "Assert both parts of the PHRASE result contract."
  (multiple-value-bind (remainder matched-p)
      (phrase rulebase rule-name input)
    (is-equal expected-remainder remainder)
    (is (eq expected-matched-p matched-p))))

(deftest dcg-phrase-surface ()
  (let ((rulebase
          (make-rulebase
           :clauses (list (def-dcg-rule greeting
                            (terminal :hello)
                            (terminal :world))))))
    ;; NIL remainder is ambiguous without MATCHED-P: it denotes either full
    ;; consumption or failure depending on the second value.
    (assert-phrase nil t rulebase 'greeting '(:hello :world))
    (assert-phrase nil nil rulebase 'greeting '(:hello))
    (assert-phrase '(:extra) t
                   rulebase 'greeting '(:hello :world :extra))
    (is-equal '(nil) (phrase-all rulebase 'greeting '(:hello :world)))
    (is-equal '() (phrase-all rulebase 'greeting '(:goodbye)))))

(deftest dcg-combinators ()
  (let ((rulebase
          (make-rulebase
           :clauses (list (def-dcg-rule noun (terminal :noun))
                          (def-dcg-rule verb (terminal :verb))
                          (def-dcg-rule optional-noun (dcg-opt noun))
                          (def-dcg-rule noun-sequence (dcg-plus noun))
                          (def-dcg-rule noun-run (dcg-star noun))
                          (def-dcg-rule maybe-word (dcg-alt noun verb))
                          (def-dcg-rule epsilon)
                          (def-dcg-rule epsilon-run (dcg-star epsilon))))))
    (assert-phrase nil t rulebase 'optional-noun '())
    (assert-phrase '(:noun) t rulebase 'noun-sequence '(:noun :noun))
    (assert-phrase nil nil rulebase 'noun-sequence '(:verb))
    (assert-phrase nil t rulebase 'noun-run '())
    (is-same-set '(((?rest . (:noun :noun :verb)))
                   ((?rest . (:noun :verb)))
                   ((?rest . (:verb))))
                 (query-prolog rulebase
                               '(noun-run (:noun :noun :verb) ?rest)))
    (assert-phrase nil t rulebase 'maybe-word '(:verb))
    (assert-phrase nil t rulebase 'maybe-word '(:noun))
    ;; a nullable rule under dcg-star must not loop forever
    (is-equal '(((?rest . (:noun))))
              (query-prolog rulebase '(epsilon-run (:noun) ?rest)))))

(deftest-queries dcg-token-builtins ((make-rulebase))
  ((dcg-token-match :noun (:noun :verb) ?rest)  :ordered (((?rest . (:verb)))))
  ((dcg-token-match :noun ((:noun . cat) :verb) ?rest) :ordered (((?rest . (:verb)))))
  ((dcg-token-match :noun (:verb) ?rest)        :fails)
  ((dcg-token-match :noun () ?rest)             :fails)
  ((dcg-token-match ?kind (:noun) ?rest)        :ordered (((?kind . :noun) (?rest))))
  ((dcg-token-match-value :name "alice" ((:name . "alice")) ?rest) :ordered (((?rest))))
  ((dcg-token-match-value :name "bob" ((:name . "alice")) ?rest)   :fails)
  ((dcg-token-match-value :age 30 (:age) ?rest) :fails)
  ((dcg-token-match-value :name ?v () ?rest)    :fails))

(deftest dcg-error-recovery ()
  (let ((rulebase (make-rulebase)))
    (assert-query rulebase
                  (dcg-error-recovery (:noise (:t-rparen . ")") :tail) ?rest)
                  :ordered (((?rest . ((:t-rparen . ")") :tail)))))
    (assert-query rulebase
                  (dcg-error-recovery (:noise :t-rparen :tail) ?rest)
                  :ordered (((?rest . (:t-rparen :tail)))))
    (assert-query rulebase
                  (dcg-error-recovery (:noise :t-semi :tail) ?rest)
                  :ordered (((?rest . (:t-semi :tail)))))
    (assert-query rulebase
                  (dcg-error-recovery (:noise :t-eof :tail) ?rest)
                  :ordered (((?rest . (:t-eof :tail)))))
    (assert-query rulebase
                  (dcg-error-recovery (:noise :more-noise) ?rest)
                  :ordered (((?rest))))
    (assert-query rulebase (dcg-error-recovery () ?rest) :ordered (((?rest))))
    (dolist (goal (quote ((dcg-error-recovery ?input ?rest)
                         (dcg-error-recovery (:noise . ?tail) ?rest)
                         (dcg-error-recovery (:t-rparen . ?tail) ?rest))))
      (signals-condition prolog-instantiation-error
        (query-prolog rulebase goal)))
    (dolist (goal (quote ((dcg-error-recovery atom ?rest)
                         (dcg-error-recovery (:noise . tail) ?rest))))
      (signals-condition prolog-type-error
        (query-prolog rulebase goal)))
    (let ((cyclic (list :noise :t-rparen)))
      (setf (cddr cyclic) cyclic)
      (signals-condition prolog-type-error
        (query-prolog rulebase
                      (list (quote dcg-error-recovery) cyclic (quote ?rest)))))))

(deftest dcg-brace-guards ()
  (let ((rulebase
          (make-rulebase
           :clauses (list (def-dcg-rule guarded-noun
                            (terminal :noun)
                            (brace (= 1 1)))
                          (def-dcg-rule blocked-noun
                            (terminal :noun)
                            (brace (= 1 2)))))))
    (assert-phrase nil t rulebase 'guarded-noun '(:noun))
    (assert-phrase nil nil rulebase 'blocked-noun '(:noun))))

(deftest dcg-expansion-internals ()
  (is-equal '((:when t) (= ?in ?out))
            (cl-prolog::%dcg-element-goals '(brace t) '?in '?out))
  (is-equal '((node ?in ?out))
            (cl-prolog::%dcg-element-goals 'node '?in '?out))
  (is-equal '((node ?x ?in ?out))
            (cl-prolog::%dcg-element-goals '(node ?x) '?in '?out))
  (is (signals-error (with-macroexpansion (expansion '(def-dcg-rule broken 42))
                       expansion))
      "Unknown DCG body elements must fail at expansion time")
  (let ((rulebase (make-rulebase :clauses (list (def-dcg-rule empty)))))
    (assert-phrase nil t rulebase 'empty '())
    (assert-phrase '(:token) t rulebase 'empty '(:token))))

(deftest prolog-dcg-expansion-is-a-difference-list-clause ()
  (let* ((clause (cl-prolog::%expand-prolog-dcg-clause
                  '(pair ?x)
                  '(and (dcg-terminals (?x))
                        (dcg-terminals (?x)))))
         (rulebase (make-rulebase :clauses (list clause))))
    (is (prolog-succeeds-p rulebase '(phrase (pair a) (a a))))
    (is (prolog-succeeds-p rulebase '(phrase (pair b) (b b tail) (tail))))
    (is (not (prolog-succeeds-p rulebase '(phrase (pair a) (a b)))))))

(deftest prolog-dcg-if-then-else-threads-condition-stream ()
  (let* ((clause
           (cl-prolog::%expand-prolog-dcg-clause
            'p
            '(if-then-else
              (dcg-terminals (a))
              (dcg-terminals (b))
              (dcg-terminals (c)))))
         (rulebase (make-rulebase :clauses (list clause))))
    (is (prolog-succeeds-p rulebase '(phrase p (a b))))
    (is (prolog-succeeds-p rulebase '(phrase p (c))))
    (is (not (prolog-succeeds-p rulebase '(phrase p (a c)))))
    (is (not (prolog-succeeds-p rulebase '(phrase p (b)))))))

(deftest prolog-dcg-goal-handles-empty-and-flat-conjunction-bodies ()
  (let* ((empty-clause (cl-prolog::%expand-prolog-dcg-clause 'empty-rule nil))
         (flat-clause
           (cl-prolog::%expand-prolog-dcg-clause
            'flat-rule
            '(and (dcg-terminals (a)) (dcg-terminals (b)) (dcg-terminals (c)))))
         (rulebase (make-rulebase :clauses (list empty-clause flat-clause))))
    (is (prolog-succeeds-p rulebase '(phrase empty-rule ())))
    (is (not (prolog-succeeds-p rulebase '(phrase empty-rule (a)))))
    (is (prolog-succeeds-p rulebase '(phrase flat-rule (a b c))))
    (is (not (prolog-succeeds-p rulebase '(phrase flat-rule (a b)))))))
