;;;; DCG surface and runtime tests.

(in-package #:fx.prolog.tests)

(deftest dcg-phrase-surface ()
  (let ((rulebase
          (make-rulebase
           :clauses (list (def-dcg-rule greeting
                            (terminal :hello)
                            (terminal :world))))))
    (multiple-value-bind (rest matched-p)
        (phrase rulebase 'greeting '(:hello :world))
      (is matched-p)
      (is (null rest)))
    (multiple-value-bind (rest matched-p)
        (phrase rulebase 'greeting '(:hello))
      (is (not matched-p))
      (is (null rest)))
    (is-equal '(:extra) (phrase rulebase 'greeting '(:hello :world :extra)))
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
    (is-equal nil (phrase rulebase 'optional-noun '()))
    (is-equal '(:noun) (phrase rulebase 'noun-sequence '(:noun :noun)))
    (is (not (nth-value 1 (phrase rulebase 'noun-sequence '(:verb)))))
    (is-equal nil (phrase rulebase 'noun-run '()))
    (is-same-set '(((?rest . (:noun :noun :verb)))
                   ((?rest . (:noun :verb)))
                   ((?rest . (:verb))))
                 (query-prolog rulebase
                               '(noun-run (:noun :noun :verb) ?rest)))
    (is-equal nil (phrase rulebase 'maybe-word '(:verb)))
    (is-equal nil (phrase rulebase 'maybe-word '(:noun)))
    ;; a nullable rule under dcg-star must not loop forever
    (is-equal '(((?rest . (:noun))))
              (query-prolog rulebase '(epsilon-run (:noun) ?rest)))))

(deftest-queries dcg-token-builtins ((make-rulebase))
  ((dcg-token-match :noun (:noun :verb) ?rest)  => (((?rest . (:verb)))))
  ((dcg-token-match :noun ((:noun . cat) :verb) ?rest) => (((?rest . (:verb)))))
  ((dcg-token-match :noun (:verb) ?rest)        :fails)
  ((dcg-token-match :noun () ?rest)             :fails)
  ((dcg-token-match ?kind (:noun) ?rest)        => (((?kind . :noun) (?rest))))
  ((dcg-token-match-value :name "alice" ((:name . "alice")) ?rest) => (((?rest))))
  ((dcg-token-match-value :name "bob" ((:name . "alice")) ?rest)   :fails)
  ((dcg-token-match-value :age 30 (:age) ?rest) :fails)
  ((dcg-token-match-value :name ?v () ?rest)    :fails))

(deftest-queries dcg-error-recovery ((make-rulebase))
  ((dcg-error-recovery (:noise (:t-rparen . ")") :tail) ?rest)
   => (((?rest . ((:t-rparen . ")") :tail)))))
  ((dcg-error-recovery (:noise :more-noise) ?rest) => (((?rest))))
  ((dcg-error-recovery () ?rest)                   => (((?rest)))))

(deftest dcg-brace-guards ()
  (let ((rulebase
          (make-rulebase
           :clauses (list (def-dcg-rule guarded-noun
                            (terminal :noun)
                            (brace (= 1 1)))
                          (def-dcg-rule blocked-noun
                            (terminal :noun)
                            (brace (= 1 2)))))))
    (is (nth-value 1 (phrase rulebase 'guarded-noun '(:noun))))
    (is (not (nth-value 1 (phrase rulebase 'blocked-noun '(:noun)))))))

(deftest dcg-expansion-internals ()
  (is-equal '((:when t) (= ?in ?out))
            (fx.prolog::%dcg-element-goals '(brace t) '?in '?out))
  (is-equal '((node ?in ?out))
            (fx.prolog::%dcg-element-goals 'node '?in '?out))
  (is-equal '((node ?x ?in ?out))
            (fx.prolog::%dcg-element-goals '(node ?x) '?in '?out))
  (is (signals-error (with-macroexpansion (expansion '(def-dcg-rule broken 42))
                       expansion))
      "Unknown DCG body elements must fail at expansion time")
  (let ((rulebase (make-rulebase :clauses (list (def-dcg-rule empty)))))
    (is-equal nil (phrase rulebase 'empty '()))
    (is-equal '(:token) (phrase rulebase 'empty '(:token))
              "An empty rule consumes nothing and leaves the input intact")))
