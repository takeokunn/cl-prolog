;;;; Standalone Prolog source loading contract.

(in-package #:cl-prolog.tests)

(deftest source-loader-accepts-each-public-input-medium ()
  (dolist (medium '(:string :stream :pathname))
    (let ((rulebase (%consult-through-medium medium "loaded(ok).")))
      (is (%source-query-succeeds-p rulebase "loaded(ok)")))))

(deftest source-loader-syntax-errors-are-catchable-iso-errors ()
  (with-temporary-prolog-files ((source "broken( ."))
    (let ((rulebase (make-rulebase)))
      (is
       (prolog-succeeds-p
        rulebase
        (read-prolog-term
         (format nil
                 "catch(consult(~A), error(syntax_error(_), context(consult, _)), true)."
                 (%prolog-path-atom source))))))))

(deftest source-loader-applies-directives-in-source-order ()
  (let ((rulebase
          (consult-prolog
           (format nil
                   ":- op(500, yfx, relates).~%~
                    relation(a relates b).~%~
                    :- dynamic(started/1).~%~
                    :- initialization(assertz(started(ok)))."))))
    (%source-queries-succeed-p
     rulebase
     "relation(a relates b)"
     "started(ok)")))

(deftest source-loader-dynamic-declarations-control-database-updates ()
  (let ((rulebase
          (consult-prolog
           ":- dynamic(mutable/1). mutable(initial). static.")))
    (%source-queries-succeed-p
     rulebase
     "assertz(mutable(added))"
     "mutable(added)")
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
    (%source-queries-succeed-p
     rulebase
     "phrase(sentence, [the, cat, sleeps])"
     "phrase(sentence, [the, dog, sleeps, today], [today])")
    (%source-queries-fail-p
     rulebase
     "phrase(sentence, [the, bird, sleeps])")))

(deftest source-loader-supports-dcg-arguments-and-braced-goals ()
  (let ((rulebase
          (consult-prolog
           "token(X) --> [X], { X = accepted }.")))
    (%source-queries-succeed-p
     rulebase
     "phrase(token(accepted), [accepted])")
    (%source-queries-fail-p
     rulebase
     "phrase(token(rejected), [rejected])")))

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

(defmacro deftest-source-loader-rolls-back (name source)
  "Define NAME as a rollback regression test for SOURCE."
  `(deftest ,name ()
     (let ((rulebase (consult-prolog "original.")))
       (signals-error (consult-prolog ,source rulebase))
       (is (%source-query-succeeds-p rulebase "original"))
       (is (not (%source-predicate-defined-p rulebase 'cl-prolog::temporary))))))

(deftest-source-loader-rolls-back
  source-loader-preserves-an-existing-rulebase-on-failure
  "temporary. ?- true.")

(deftest-source-loader-rolls-back
  source-loader-rolls-back-failed-initialization
  "temporary. :- initialization(fail).")

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

(deftest consult-publishes-clauses-before-the-next-conjunct ()
  (with-temporary-prolog-files
      ((source "consulted(immediately)."))
    (let ((rulebase (make-rulebase)))
      (is (%source-query-succeeds-p
           rulebase
           (format nil "consult(~A), consulted(immediately)"
                   (%prolog-path-atom source)))))))

(deftest consult-loads-a-source-list-in-order ()
  (with-temporary-prolog-files
      ((declaration ":- op(500, xfx, precedes).")
       (usage "ordered(first precedes second)."))
    (let ((rulebase (make-rulebase)))
      (is (%source-list-query-succeeds-p
           rulebase
           (list declaration usage)
           "consult(~A), ordered(precedes(first, second))")))))

(deftest load-files-loads-each-source-in-its-list ()
  (with-temporary-prolog-files
      ((first "loaded_from(first).")
       (second "loaded_from(second)."))
    (let ((rulebase (make-rulebase)))
      (is (%source-list-query-succeeds-p
           rulebase
           (list first second)
           "load_files(~A), loaded_from(first), loaded_from(second)")))))

(deftest load-files-rolls-back-the-whole-list-on-late-failure ()
  (with-temporary-prolog-files
      ((valid "transient_clause.")
       (invalid ":- unsupported_directive(value)."))
    (let ((rulebase (consult-prolog "preserved_clause.")))
      (signals-error
       (query-prolog
        rulebase
        (read-prolog-term
         (format nil "load_files(~A)"
                 (%prolog-path-list (list valid invalid))))))
      (is (%source-query-succeeds-p rulebase "preserved_clause"))
      (is (%source-query-succeeds-p
           rulebase
           "catch((transient_clause, fail), error(existence_error(procedure, _), _), true)")))))

(deftest load-files-defers-initialization-until-every-source-is-valid ()
  (with-temporary-prolog-files
      ((initializing ":- initialization(write(should_not_run)).")
       (invalid ":- unsupported_directive(value)."))
    (let* ((output (make-string-output-stream))
           (context (cl-prolog::make-prolog-io-context :output output))
           (rulebase (make-rulebase :io-context context)))
      (signals-error (consult-prolog (list initializing invalid) rulebase))
      (is-equal "" (get-output-stream-string output)))))

(deftest source-loader-resolves-nested-directives-relative-to-their-source ()
  (let* ((base (%temporary-prolog-pathname))
         (directory (make-pathname :directory
                                   (append (pathname-directory base)
                                           (list (pathname-name base)))
                                   :name nil :type nil :defaults base))
         (entry (merge-pathnames "entry.pl" directory))
         (nested-directory (merge-pathnames "nested/" directory))
         (consulted (merge-pathnames "consulted.pl" nested-directory))
         (loaded (merge-pathnames "loaded.pl" nested-directory)))
    (unwind-protect
         (progn
           (ensure-directories-exist consulted)
           (with-open-file (output consulted :direction :output
                                              :if-exists :supersede)
             (write-string "nested_consulted." output))
           (with-open-file (output loaded :direction :output
                                           :if-exists :supersede)
             (write-string "nested_loaded." output))
           (with-open-file (output entry :direction :output
                                          :if-exists :supersede)
             (write-string
              ":- consult('nested/consulted.pl'). :- load_files(['nested/loaded.pl'])."
              output))
           (let ((rulebase (consult-prolog entry)))
             (is (%source-query-succeeds-p rulebase "nested_consulted"))
             (is (%source-query-succeeds-p rulebase "nested_loaded"))))
      (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore))))

(deftest ensure-loaded-is-idempotent-across-pathname-spellings ()
  (with-temporary-prolog-files
      ((source "loaded_once."))
    (let* ((rulebase (make-rulebase))
           (relative (enough-namestring source (truename "."))))
      (ensure-prolog-loaded source rulebase)
      (ensure-prolog-loaded (pathname relative) rulebase)
      (is-equal 1
                (count 'cl-prolog::loaded_once
                       (rulebase-visible-clauses rulebase)
                       :key (lambda (clause) (first (clause-head clause))))))))

(deftest consult-replaces-artifacts-owned-by-the-same-source ()
  (with-temporary-prolog-files
      ((source ":- op(500, xfx, source_operator). old_clause."))
    (let ((rulebase (consult-prolog source)))
      (with-open-file (output source :direction :output :if-exists :supersede)
        (write-string "new_clause." output))
      (consult-prolog source rulebase)
      (is (not (%source-predicate-defined-p rulebase 'cl-prolog::old_clause)))
      (is (%source-query-succeeds-p rulebase "new_clause"))
      (is (null (cl-prolog::%operator-table-find
                 (cl-prolog::rulebase-operator-table rulebase)
                 'cl-prolog::source_operator))))))

(deftest consult-reload-preserves-runtime-clauses-and-operator-overrides ()
  (with-temporary-prolog-files
      ((source ":- op(500, xfx, layered_operator). source_clause."))
    (let ((rulebase (consult-prolog source)))
      (is (%source-query-succeeds-p rulebase "assertz(runtime_clause)"))
      (is (%source-query-succeeds-p
           rulebase "op(600, xfx, 'LAYERED_OPERATOR')"))
      (with-open-file (output source :direction :output :if-exists :supersede)
        (write-string "replacement_clause." output))
      (consult-prolog source rulebase)
      (is (%source-query-succeeds-p rulebase "runtime_clause"))
      (is-equal 600
                (cl-prolog::operator-definition-priority
                 (first (cl-prolog::%operator-table-find
                         (cl-prolog::rulebase-operator-table rulebase)
                         'cl-prolog::layered_operator :xfx)))))))

(deftest consult-reload-failure-restores-owned-artifacts ()
  (with-temporary-prolog-files
      ((source ":- op(500, xfx, rollback_operator). preserved_source_clause."))
    (let ((rulebase (consult-prolog source)))
      (with-open-file (output source :direction :output :if-exists :supersede)
        (write-string "transient_source_clause. ?- invalid." output))
      (signals-error (consult-prolog source rulebase))
      (is (%source-query-succeeds-p rulebase "preserved_source_clause"))
      (is (cl-prolog::%operator-table-find
           (cl-prolog::rulebase-operator-table rulebase)
           'cl-prolog::rollback_operator :xfx)))))

(deftest canonical-source-identity-collapses-symbolic-links ()
  (with-temporary-prolog-files ((source "linked_clause."))
    (let ((link (%temporary-prolog-pathname)))
      (unwind-protect
           (progn
             (uiop:run-program (list "ln" "-s" (namestring source)
                                     (namestring link)))
             (let ((rulebase (ensure-prolog-loaded source)))
               (ensure-prolog-loaded link rulebase)
               (is-equal 1
                         (hash-table-count
                          (cl-prolog::rulebase-source-registry rulebase)))
               (is-equal 1 (length (rulebase-visible-clauses rulebase)))))
        (when (probe-file link) (delete-file link))))))

(deftest ensure-loaded-breaks-circular-source-directives ()
  (with-temporary-prolog-files
      ((first "first_loaded. :- ensure_loaded('__SECOND__').")
       (second "second_loaded. :- ensure_loaded('__FIRST__')."))
    (with-open-file (output first :direction :output :if-exists :supersede)
      (format output "first_loaded. :- ensure_loaded(~A)."
              (%prolog-path-atom second)))
    (with-open-file (output second :direction :output :if-exists :supersede)
      (format output "second_loaded. :- ensure_loaded(~A)."
              (%prolog-path-atom first)))
    (let ((rulebase (ensure-prolog-loaded first)))
      (%source-queries-succeed-p
       rulebase
       "first_loaded"
       "second_loaded")
      (is-equal 2 (hash-table-count
                   (cl-prolog::rulebase-source-registry rulebase))))))

(deftest load-files-if-not-loaded-is-idempotent-and-atomic ()
  (with-temporary-prolog-files
      ((first "first_once.")
       (second "second_once."))
    (let ((rulebase (make-rulebase)))
      (dotimes (_ 2)
        (is (%source-list-query-succeeds-p
             rulebase
             (list first second)
             "load_files(~A, [if(not_loaded)])")))
      (is-equal 2 (length (rulebase-visible-clauses rulebase))))))

(deftest load-files-if-not-loaded-rolls-back-source-registry ()
  (with-temporary-prolog-files
      ((valid "must_rollback.")
       (invalid ":- unsupported_directive(value)."))
    (let* ((rulebase (make-rulebase))
           (query (format nil "load_files(~A, [if(not_loaded)])"
                          (%prolog-path-list (list valid invalid)))))
      (signals-error (%source-query-succeeds-p rulebase query))
      (is (null (rulebase-visible-clauses rulebase)))
      (is-equal 0
                (hash-table-count
                 (cl-prolog::rulebase-source-registry rulebase))))))

(deftest-table load-files-options-are-strict ()
  (:is (%source-query-succeeds-p
        (make-rulebase)
        "catch(load_files([], Options), error(instantiation_error, _), true)"))
  (:is (%source-query-succeeds-p
        (make-rulebase)
        "catch(load_files([], [if(Mode)]), error(instantiation_error, _), true)"))
  (:is (%source-query-succeeds-p
        (make-rulebase)
        "catch(load_files([], [if(not_loaded) | tail]), error(type_error(list, _), _), true)"))
  (:is (%source-query-succeeds-p
        (make-rulebase)
        "catch(load_files([], [unknown]), error(domain_error(load_option, unknown), _), true)"))
  (:is (%source-query-succeeds-p
        (make-rulebase)
        "catch(load_files([], [if(not_loaded), if(not_loaded)]), error(domain_error(load_option, _), _), true)")))

(deftest source-loader-supports-iso-declaration-directives ()
  (let ((rulebase
          (consult-prolog
           (format nil
                   ":- discontiguous(scattered/1).~%~
                    scattered(one).~%~
                    other(fact).~%~
                    scattered(two).~%~
                    :- multifile(shared/0).~%~
                    :- set_prolog_flag(double_quotes, codes)."))))
    (%source-queries-succeed-p
     rulebase
     "scattered(one)"
     "scattered(two)")
    (is (%source-query-succeeds-p rulebase
                                  "current_prolog_flag(double_quotes, codes)")))
  (signals-error (consult-prolog ":- discontiguous.")))

(deftest source-loader-supports-table-directives-without-changing-update-property ()
  (let ((rulebase
          (consult-prolog
           ":- table(reachable/1). :- dynamic(reachable/1). reachable(origin).")))
    (is (cl-prolog::%rulebase-tabled-p
         rulebase 'cl-prolog::reachable 1))
    (is (eq :dynamic
            (cl-prolog::%rulebase-predicate-property
             rulebase 'cl-prolog::reachable 1)))
    (is (%source-query-succeeds-p rulebase "reachable(origin)")))
  (signals-error (consult-prolog ":- table(reachable).")))

(deftest source-loader-table-declarations-are-source-owned ()
  (with-temporary-prolog-files
      ((first ":- table(shared_table/1). shared_table(first).")
       (second ":- table(shared_table/1). shared_table(second)."))
    (let ((rulebase (consult-prolog (list first second))))
      (is (cl-prolog::%rulebase-tabled-p
           rulebase 'cl-prolog::shared_table 1))
      (with-open-file (output first :direction :output :if-exists :supersede)
        (write-string "replacement(first)." output))
      (consult-prolog first rulebase)
      (is (cl-prolog::%rulebase-tabled-p
           rulebase 'cl-prolog::shared_table 1))
      (with-open-file (output second :direction :output :if-exists :supersede)
        (write-string "replacement(second)." output))
      (consult-prolog second rulebase)
      (is (not (cl-prolog::%rulebase-tabled-p
                rulebase 'cl-prolog::shared_table 1))))))

(deftest source-loader-rolls-back-table-declarations ()
  (let ((rulebase (make-rulebase)))
    (signals-error
     (consult-prolog
      ":- table(transient_table/1). :- initialization(fail)."
      rulebase))
    (is (not (cl-prolog::%rulebase-tabled-p
              rulebase 'cl-prolog::transient_table 1)))))

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
