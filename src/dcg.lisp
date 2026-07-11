;;;; DCG surface macros.
;;;;
;;;; DEF-DCG-RULE compiles a grammar body into an ordinary rule whose head
;;;; carries two extra stream arguments; the runtime combinators live in
;;;; dcg-runtime.lisp.  Body elements:
;;;;
;;;;   (terminal KIND...)   match token kinds
;;;;   (brace EXPR)         Lisp guard, compiled like (:when EXPR)
;;;;   NAME or (NAME ARG..) non-terminal call
;;;;   (dcg-* ...)          runtime combinators, threaded like non-terminals

(in-package #:cl-prolog)

(defun %thread-dcg-elements (elements stream-in stream-out element->goals)
  "Chain ELEMENTS with fresh intermediate stream variables."
  (if (null elements)
      (list `(= ,stream-in ,stream-out))
      (labels ((thread (remaining current)
                 (let* ((element (first remaining))
                        (rest (rest remaining))
                        (next (if rest (fresh-logic-variable "?S") stream-out)))
                   (append (funcall element->goals element current next)
                           (when rest
                             (thread rest next))))))
        (thread elements stream-in))))

(defun %dcg-tag-p (element tag)
  (and (consp element)
       (symbolp (first element))
       (string= (symbol-name (first element)) tag)))

(defun %dcg-element-goals (element stream-in stream-out)
  "Expand one DCG body ELEMENT into engine goals."
  (cond
    ((%dcg-tag-p element "TERMINAL")
     (%thread-dcg-elements
      (rest element) stream-in stream-out
      (lambda (terminal current next)
        (list `(dcg-token-match ,terminal ,current ,next)))))
    ((%dcg-tag-p element "BRACE")
     (list `(:when ,(second element))
           `(= ,stream-in ,stream-out)))
    ((symbolp element)
     (list (list element stream-in stream-out)))
    ((consp element)
     (list (append element (list stream-in stream-out))))
    (t
     (error "DCG: unknown body element ~S" element))))

(defun %dcg-body-goals (body stream-in stream-out)
  (%thread-dcg-elements body stream-in stream-out #'%dcg-element-goals))

(defmacro def-dcg-rule (name &body body)
  "Return a grammar clause named NAME.

The clause head is (NAME STREAM-IN STREAM-OUT)."
  (let ((stream-in (fresh-logic-variable "?S-IN"))
        (stream-out (fresh-logic-variable "?S-OUT")))
    `(def-rule (,name ,stream-in ,stream-out)
       ,@(%dcg-body-goals body stream-in stream-out))))

(defun phrase (rulebase rule-name input)
  "Parse INPUT with RULE-NAME against RULEBASE.

Returns (VALUES REMAINDER MATCHED-P): the unconsumed remainder of the
first parse and whether any parse exists."
  (let ((solution (query-prolog-first rulebase
                                      (list rule-name input '?dcg-rest))))
    (values (solution-binding '?dcg-rest solution)
            (not (null solution)))))

(defun phrase-all (rulebase rule-name input)
  "Return the unconsumed remainder of every parse of INPUT with RULE-NAME."
  (mapcar (lambda (solution)
            (solution-binding '?dcg-rest solution))
          (query-prolog rulebase (list rule-name input '?dcg-rest))))
