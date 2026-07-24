;;;; DCG runtime builtins.
;;;;
;;;; These builtins operate on token streams threaded through goals as
;;;; (RULE STREAM-IN STREAM-OUT).  A token is either a bare kind symbol or
;;;; a (KIND . VALUE) cons.  Like every builtin they stream solutions via
;;;; EMIT; combinator sub-proofs run inside their own cut barrier so a cut
;;;; in a grammar rule stays local to that rule.

(in-package #:cl-prolog)

(defparameter *dcg-sync-tokens* '(:t-rparen :t-semi :t-eof)
  "Token kinds DCG-ERROR-RECOVERY skips ahead to.")

(defun %dcg-token-kind (token)
  (if (consp token) (car token) token))

(defun %dcg-token-value (token)
  (when (consp token) (cdr token)))

(defun %dcg-sync-token-p (token)
  (member (%dcg-token-kind token) *dcg-sync-tokens* :test #'eq))

(defun %skip-to-sync-token (tokens)
  "Drop TOKENS until one is a synchronization token."
  (cond
    ((null tokens) nil)
    ((%dcg-sync-token-p (first tokens)) tokens)
    (t (%skip-to-sync-token (rest tokens)))))

(defun %dcg-progress-p (stream-in stream-out environment)
  "Return true when STREAM-OUT represents actual progress from STREAM-IN."
  (let ((resolved-in (logic-substitute stream-in environment))
        (resolved-out (logic-substitute stream-out environment)))
    (and (not (logic-var-p resolved-out))
         (not (equal resolved-in resolved-out)))))

(defun %solve-dcg-star (rule stream-in stream-out rulebase environment depth emit)
  "Match zero or more repetitions of RULE between STREAM-IN and STREAM-OUT."
  (%unify-emit stream-out stream-in environment emit)
  (let ((midpoint (fresh-logic-variable "?MID")))
    (%prove-bindings/k (list rule stream-in midpoint)
                       rulebase environment depth
                       (lambda (extended)
                         ;; Only recurse when RULE actually advanced the stream.
                         ;; A nullable rule leaves MIDPOINT unresolved, which
                         ;; would otherwise keep generating the same proof forever.
                         (when (%dcg-progress-p stream-in midpoint extended)
                           (%solve-dcg-star rule midpoint stream-out
                                            rulebase extended depth emit))))))

(defun %dcg-match-token (expected-kind expected-value input rest environment emit)
  "Match INPUT's first token against EXPECTED-KIND and EXPECTED-VALUE,
unifying REST with the remaining tokens on success. A fresh, unbound
EXPECTED-VALUE unifies with any token value, so this also implements a
kind-only match."
  (let ((tokens (logic-substitute input environment)))
    (when (consp tokens)
      (multiple-value-bind (kind-env kind-ok)
          (unify expected-kind (%dcg-token-kind (first tokens)) environment)
        (when kind-ok
          (multiple-value-bind (value-env value-ok)
              (unify expected-value (%dcg-token-value (first tokens)) kind-env)
            (when value-ok
              (%unify-emit rest (rest tokens) value-env emit))))))))

(define-builtin (dcg-token-match expected input rest) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (%dcg-match-token expected (fresh-logic-variable) input rest environment emit))

(define-builtin (dcg-token-match-value expected-kind expected-value input rest)
                (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (%dcg-match-token expected-kind expected-value input rest environment emit))

(define-builtin (dcg-error-recovery input rest) (rulebase environment depth emit)
  (let ((tokens (logic-substitute input environment))
        (operation (%iso-atom "DCG_ERROR_RECOVERY")))
    (cond
      ((logic-var-p tokens)
       (%raise-instantiation-error
        environment operation "The input token list must be instantiated"))
      ((not (%proper-list-p tokens))
       (loop with visited = (make-hash-table :test (function eq))
             for tail = tokens then (cdr tail)
             do (cond
                  ((logic-var-p tail)
                   (%raise-instantiation-error
                    environment operation
                    "The input token list must be fully instantiated"))
                  ((not (consp tail))
                   (%raise-type-error
                    "LIST" tokens environment operation
                    "The input must be a proper token list"))
                  ((gethash tail visited)
                   (%raise-type-error
                    "LIST" tokens environment operation
                    "The input must be a finite proper token list"))
                  (t
                   (setf (gethash tail visited) t)))))
      (t
       (%unify-emit rest (%skip-to-sync-token tokens) environment emit)))))

(define-builtin (dcg-opt rule stream-in stream-out) (rulebase environment depth emit)
  (%prove-bindings/k (list rule stream-in stream-out)
                     rulebase environment depth emit)
  (%unify-emit stream-out stream-in environment emit))

(define-builtin (dcg-star rule stream-in stream-out) (rulebase environment depth emit)
  (%solve-dcg-star rule stream-in stream-out rulebase environment depth emit))

(define-builtin (dcg-plus rule stream-in stream-out) (rulebase environment depth emit)
  (let ((midpoint (fresh-logic-variable "?MID")))
    (%prove-bindings/k (list rule stream-in midpoint)
                       rulebase environment depth
                       (lambda (extended)
                         (%solve-dcg-star rule midpoint stream-out
                                          rulebase extended depth emit)))))

(define-builtin (dcg-alt &rest arguments) (rulebase environment depth emit)
  (let ((alternatives (butlast arguments 2))
        (streams (last arguments 2)))
    (unless (and alternatives (= (length streams) 2))
      (%invalid-goal (cons 'dcg-alt arguments)
                     "DCG-ALT needs at least one alternative and two stream arguments"))
    (dolist (alternative alternatives)
      (%prove-bindings/k (list* alternative streams)
                         rulebase environment depth emit))))
