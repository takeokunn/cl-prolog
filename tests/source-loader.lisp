;;;; Standalone Prolog source loading contract.

(in-package #:cl-prolog.tests)

(defun %temporary-prolog-pathname ()
  (merge-pathnames
   (make-pathname :name (format nil ".cl-prolog-source-~A" (gensym))
                  :type "pl")
   (truename ".")))

(defun %call-with-source-medium (medium source function)
  (ecase medium
    (:string (funcall function source))
    (:stream
     (with-input-from-string (stream source)
       (let ((result (funcall function stream)))
         (is (open-stream-p stream))
         result)))
    (:pathname
     (let ((pathname (%temporary-prolog-pathname)))
       (unwind-protect
            (progn
              (with-open-file (output pathname
                                      :direction :output
                                      :if-exists :supersede)
                (write-string source output))
              (funcall function pathname))
         (when (probe-file pathname)
           (delete-file pathname)))))))

(defun %consult-through-medium (medium source &optional (rulebase (make-rulebase)))
  (%call-with-source-medium
   medium source
   (lambda (input)
     (consult-prolog input rulebase))))

(defun %source-query-succeeds-p (rulebase source)
  (prolog-succeeds-p
   rulebase
   (read-prolog-term source (cl-prolog::rulebase-operator-table rulebase))))

(defun %source-predicate-defined-p (rulebase name)
  (find name (rulebase-visible-clauses rulebase)
        :key (lambda (clause)
               (car (clause-head clause)))
        :test #'eq))

(deftest source-loader-accepts-each-public-input-medium ()
  (dolist (medium '(:string :stream :pathname))
    (let ((rulebase (%consult-through-medium medium "loaded(ok).")))
      (is (%source-query-succeeds-p rulebase "loaded(ok)")))))

(deftest source-loader-applies-directives-in-source-order ()
  (let ((rulebase
          (consult-prolog
           (format nil
                   ":- op(500, yfx, relates).~%~
                    relation(a relates b).~%~
                    :- dynamic(started/1).~%~
                    :- initialization(assertz(started(ok)))."))))
    (is (%source-query-succeeds-p rulebase "relation(a relates b)"))
    (is (%source-query-succeeds-p rulebase "started(ok)"))))

(deftest source-loader-dynamic-declarations-control-database-updates ()
  (let ((rulebase
          (consult-prolog
           ":- dynamic(mutable/1). mutable(initial). static.")))
    (is (%source-query-succeeds-p rulebase "assertz(mutable(added))"))
    (is (%source-query-succeeds-p rulebase "mutable(added)"))
    (signals-error
     (query-prolog rulebase (read-prolog-term "assertz(static)")))))

(deftest source-loader-expands-standard-dcg-rules ()
  (let ((rulebase
          (consult-prolog
           (format nil
                   "sentence --> noun_phrase, verb_phrase.~%~
                    noun_phrase --> [the], noun.~%~
                    noun --> [cat] ; [dog].~%~
                    verb_phrase --> [sleeps]."))))
    (is (%source-query-succeeds-p
         rulebase "phrase(sentence, [the, cat, sleeps])"))
    (is (%source-query-succeeds-p
         rulebase "phrase(sentence, [the, dog, sleeps, today], [today])"))
    (is (not (%source-query-succeeds-p
              rulebase "phrase(sentence, [the, bird, sleeps])")))))

(deftest source-loader-supports-dcg-arguments-and-braced-goals ()
  (let ((rulebase
          (consult-prolog
           "token(X) --> [X], { X = accepted }.")))
    (is (%source-query-succeeds-p rulebase "phrase(token(accepted), [accepted])"))
    (is (not (%source-query-succeeds-p
              rulebase "phrase(token(rejected), [rejected])")))))

(deftest-table source-loader-rejects-non-consult-forms ()
  (:signals (consult-prolog "?- true.")
            "Queries are not consultable source forms")
  (:signals (consult-prolog ":- unsupported_directive(value).")
            "Unknown directives must not be ignored")
  (:signals (consult-prolog ":- op(1300, xfx, invalid).")
            "Invalid directives fail during validation"))

(deftest source-loader-validation-is-atomic ()
  (dolist (source '("kept. ?- kept."
                    "kept. :- op(1300, xfx, invalid)."
                    "kept. :- unsupported_directive(value)."))
    (let ((rulebase (make-rulebase)))
      (signals-error (consult-prolog source rulebase))
      (is-equal '() (rulebase-visible-clauses rulebase)))))

(deftest source-loader-preserves-an-existing-rulebase-on-failure ()
  (let ((rulebase (consult-prolog "original.")))
    (signals-error (consult-prolog "temporary. ?- true." rulebase))
    (is (%source-query-succeeds-p rulebase "original"))
    (is (not (%source-predicate-defined-p rulebase 'cl-prolog::temporary)))))

(deftest source-loader-rolls-back-failed-initialization ()
  (let ((rulebase (consult-prolog "original.")))
    (signals-error
     (consult-prolog
      "temporary. :- initialization(fail)."
      rulebase))
    (is (%source-query-succeeds-p rulebase "original"))
    (is (not (%source-predicate-defined-p rulebase 'cl-prolog::temporary)))))

(deftest source-loader-rolls-back-io-context-changes ()
  (let* ((original-output (make-string-output-stream))
         (transient-output (make-string-output-stream))
         (context (cl-prolog::make-prolog-io-context
                   :output original-output
                   :error-output (make-string-output-stream)))
         (alias (cl-prolog::%prolog-atom-symbol "transient_output"))
         (rulebase (make-rulebase :io-context context)))
    (unwind-protect
         (progn
           (cl-prolog::%register-prolog-stream!
            context transient-output :output :alias alias)
           (signals-error
            (consult-prolog
             ":- initialization((set_output(transient_output), fail))."
             rulebase))
           (is (eq original-output
                   (cl-prolog::prolog-stream-stream
                    (cl-prolog::prolog-io-context-current-output
                     (cl-prolog::rulebase-io-context rulebase))))))
      (cl-prolog::%close-all-owned-prolog-streams! context))))

(deftest source-loader-rolls-back-operator-and-predicate-properties ()
  (let ((rulebase (make-rulebase)))
    (signals-error
     (consult-prolog
      (format nil
              ":- op(500, yfx, transient).~%~
               :- dynamic(transient/1).~%~
               :- initialization(fail).")
      rulebase))
    (is (null (cl-prolog::%operator-table-find
               (cl-prolog::rulebase-operator-table rulebase)
               'cl-prolog::transient)))
    (is (null (cl-prolog::%rulebase-predicate-property
               rulebase 'cl-prolog::transient 1)))))
