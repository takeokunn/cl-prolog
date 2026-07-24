;;;; Source-loading interning and resource-error safety: char_conversion,
;;;; include splicing, symbol-interning avoidance on rejected directives, and
;;;; parser resource-error propagation through the direct API and consult/2.

(in-package #:cl-prolog.tests)

(deftest source-loader-char-conversion-directive-affects-later-terms ()
  (let ((rulebase
          (consult-prolog
           (format nil
                   ":- char_conversion('k', 'f').~%~
                    :- set_prolog_flag(char_conversion, on).~%~
                    kact(one).~%~
                    'kwoted'(two)."))))
    ;; kact reads as fact under the conversion; quoted atoms are exempt.
    (%source-queries-succeed-p
     rulebase
     "fact(one)"
     "'kwoted'(two)")))

(progn
(progn
(deftest source-loader-include-splices-terms-into-the-including-unit ()
  (with-temporary-prolog-files ((included "included(fact)."))
      (let ((rulebase
              (consult-prolog
               (format nil ":- include(~A).~%outer(fact)."
                       (%prolog-path-atom included)))))
      (%source-queries-succeed-p
       rulebase
       "included(fact)"
       "outer(fact)"))))
(deftest source-loader-include-rejects-a-query-in-the-included-unit ()
  (with-temporary-prolog-files ((included "?- true."))
    (signals-error
      (consult-prolog
       (format nil ":- include(~A)." (%prolog-path-atom included))))))
(deftest source-loader-include-rejects-a-module-directive-in-the-included-unit ()
  (with-temporary-prolog-files ((included ":- module(spliced, [])."))
    (signals-error
      (consult-prolog
       (format nil ":- include(~A)." (%prolog-path-atom included))))))
(deftest source-loader-include-applies-an-operator-directive-from-the-included-unit ()
  (with-temporary-prolog-files ((included ":- op(700, xfx, spliced_op)."))
    (let ((rulebase
            (consult-prolog
             (format nil ":- include(~A).~%outer(a spliced_op b)."
                     (%prolog-path-atom included)))))
      (is-equal '(cl-prolog::outer (cl-prolog::spliced_op cl-prolog::a cl-prolog::b))
                (cl-prolog::clause-head
                 (first (cl-prolog::rulebase-visible-clauses rulebase)))))))
(deftest source-loader-accepts-a-zero-arity-predicate-with-an-explicit-body ()
  (let ((rulebase (consult-prolog "bar. foo :- bar.")))
    (is (%source-query-succeeds-p rulebase "foo."))))
(deftest source-loader-operator-specifiers-do-not-intern-keywords ()
  (dolist (entry '(("FX" :fx) ("FY" :fy) ("XF" :xf) ("YF" :yf)
                   ("XFX" :xfx) ("XFY" :xfy) ("YFX" :yfx)))
    (is (eq (second entry)
            (cl-prolog::%operator-specifier-keyword
             (make-symbol (first entry))))))
  (let ((before (package-owned-symbol-count '#:keyword)))
    (dotimes (index 128)
      (let* ((name (format nil "INVALID-OPERATOR-~D" index))
             (specifier (make-symbol name)))
        (is (null (nth-value 1 (find-symbol name '#:keyword))))
        (signals-error
         (cl-prolog::%operator-specifier-keyword specifier))
        (is (null (nth-value 1 (find-symbol name '#:keyword))))))
    (is-equal before (package-owned-symbol-count '#:keyword))))

(deftest source-loader-missing-pathnames-do-not-intern-culprits ()
  (let* ((pathname (%temporary-prolog-pathname))
         (resolved (cl-prolog::%resolve-prolog-source-pathname pathname))
         (name (namestring resolved)))
    (is (null (nth-value 1 (find-symbol name '#:cl-prolog))))
    (handler-case
        (progn
          (cl-prolog::with-prolog-source-errors (nil (cl-prolog::%iso-atom "CONSULT")) (consult-prolog pathname))
          (error "Expected a missing source error."))
      (prolog-existence-error (condition)
        (let ((culprit
                (third (second (prolog-exception-term condition)))))
          (is (null (symbol-package culprit)))
          (is-equal name (symbol-name culprit))
          (is-equal (%prolog-path-atom resolved)
                    (prolog-term-string culprit)))))
    (is (null (nth-value 1 (find-symbol name '#:cl-prolog))))))

(deftest source-loader-syntax-descriptions-do-not-intern-culprits ()
  (let ((before (package-owned-symbol-count '#:cl-prolog)))
    (dotimes (index 32)
      (let ((description
              (format nil "Unexpected generated token ~D." index)))
        (handler-case
            (cl-prolog::%raise-syntax-error
             (make-condition 'cl-prolog::prolog-parse-error
                             :description description)
             nil 'cl-prolog::consult)
          (cl-prolog::prolog-syntax-error (condition)
            (let ((culprit
                    (second (second
                             (prolog-exception-term condition)))))
              (is (null (symbol-package culprit)))
              (is-equal description (symbol-name culprit)))))))
    (is-equal before (package-owned-symbol-count '#:cl-prolog)))))

(deftest source-loader-preserves-parser-resource-errors-for-direct-api ()
  (with-temporary-prolog-files ((source "toolong."))
    (let ((*max-prolog-identifier-length* 1))
      (handler-case
          (progn
            (consult-prolog source)
            (error "Expected a parser resource error."))
        (prolog-parser-resource-error (condition)
          (is-equal "IDENTIFIER_LENGTH"
                    (prolog-parser-resource-error-resource condition)))))))

(deftest source-loader-translates-parser-resource-errors-for-consult ()
  (with-temporary-prolog-files ((source "toolong."))
    (let* ((rulebase (make-rulebase))
           (query
             (%read-prolog-query
              rulebase
              "catch(consult(~A), error(resource_error(identifier_length), _), true)."
              (%prolog-path-atom source))))
      (let ((*max-prolog-identifier-length* 1))
        (is (prolog-succeeds-p rulebase query)))))))

(deftest source-loader-rejects-cyclic-source-lists ()
  (let ((sources (list (cl-prolog::%prolog-atom-symbol "cycle.pl"
                                                       :preserve-case t))))
    (setf (cdr sources) sources)
    (signals-error
     (cl-prolog::%source-file-pathnames sources nil 'cl-prolog::consult))))
