;;;; DCG runtime builtins.
;;;;
;;;; These builtins operate on token streams threaded through goals as
;;;; (RULE STREAM-IN STREAM-OUT).  A token is either a bare kind symbol or
;;;; a (KIND . VALUE) cons.  Like every builtin they stream solutions via
;;;; EMIT; combinator sub-proofs run inside their own cut barrier so a cut
;;;; in a grammar rule stays local to that rule.

(in-package #:fx.prolog)

(defparameter *dcg-sync-tokens* '(:t-rparen :t-semi :t-eof)
  "Token kinds DCG-ERROR-RECOVERY skips ahead to.")

(defun %dcg-token-kind (token)
  (if (consp token) (car token) token))

(defun %dcg-token-value (token)
  (when (consp token) (cdr token)))

(defun %dcg-sync-token-p (token)
  (and (consp token)
       (member (car token) *dcg-sync-tokens* :test #'eq)))

(defun %skip-to-sync-token (tokens)
  "Drop TOKENS until one is a synchronization token."
  (cond
    ((null tokens) nil)
    ((%dcg-sync-token-p (first tokens)) tokens)
    (t (%skip-to-sync-token (rest tokens)))))

(defun %solve-dcg-star (rule stream-in stream-out rulebase environment depth emit)
  "Match zero or more repetitions of RULE between STREAM-IN and STREAM-OUT."
  (%unify-emit stream-out stream-in environment emit)
  (let ((midpoint (fresh-logic-variable "?MID")))
    (%with-cut-barrier
      (%prove-goal (list rule stream-in midpoint) rulebase environment depth
                   (lambda (extended)
                     ;; Only recurse when RULE consumed input, so nullable
                     ;; rules cannot loop forever.
                     (unless (equal (logic-substitute midpoint extended)
                                    (logic-substitute stream-in extended))
                       (%solve-dcg-star rule midpoint stream-out
                                        rulebase extended depth emit)))))))

(define-builtin (dcg-token-match expected input rest) (rulebase environment depth emit)
  (let ((tokens (logic-substitute input environment)))
    (when (consp tokens)
      (multiple-value-bind (extended ok)
          (unify expected (%dcg-token-kind (first tokens)) environment)
        (when ok
          (%unify-emit rest (rest tokens) extended emit))))))

(define-builtin (dcg-token-match-value expected-kind expected-value input rest)
                (rulebase environment depth emit)
  (let ((tokens (logic-substitute input environment)))
    (when (consp tokens)
      (multiple-value-bind (kind-env kind-ok)
          (unify expected-kind (%dcg-token-kind (first tokens)) environment)
        (when kind-ok
          (multiple-value-bind (value-env value-ok)
              (unify expected-value (%dcg-token-value (first tokens)) kind-env)
            (when value-ok
              (%unify-emit rest (rest tokens) value-env emit))))))))

(define-builtin (dcg-error-recovery input rest) (rulebase environment depth emit)
  (%unify-emit rest
               (%skip-to-sync-token (logic-substitute input environment))
               environment emit))

(define-builtin (dcg-opt rule stream-in stream-out) (rulebase environment depth emit)
  (%with-cut-barrier
    (%prove-goal (list rule stream-in stream-out) rulebase environment depth emit))
  (%unify-emit stream-out stream-in environment emit))

(define-builtin (dcg-star rule stream-in stream-out) (rulebase environment depth emit)
  (%solve-dcg-star rule stream-in stream-out rulebase environment depth emit))

(define-builtin (dcg-plus rule stream-in stream-out) (rulebase environment depth emit)
  (let ((midpoint (fresh-logic-variable "?MID")))
    (%with-cut-barrier
      (%prove-goal (list rule stream-in midpoint) rulebase environment depth
                   (lambda (extended)
                     (%solve-dcg-star rule midpoint stream-out
                                      rulebase extended depth emit))))))

(define-builtin (dcg-alt &rest arguments) (rulebase environment depth emit)
  (let ((alternatives (butlast arguments 2))
        (streams (last arguments 2)))
    (unless (and alternatives (= (length streams) 2))
      (%invalid-goal (cons 'dcg-alt arguments)
                     "DCG-ALT needs at least one alternative and two stream arguments"))
    (dolist (alternative alternatives)
      (%with-cut-barrier
        (%prove-goal (list* alternative streams) rulebase environment depth emit)))))
