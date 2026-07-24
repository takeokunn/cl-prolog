;;;; consult/load_files/ensure_loaded loading-semantics tests: source lists,
;;;; reload/if-loaded policy, symbolic-link identity, circular ensure_loaded,
;;;; ISO declaration directives, and table-declaration ownership.

(in-package #:cl-prolog.tests)

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

(deftest source-loader-reload-restores-an-operator-removed-by-the-prior-load ()
  (with-temporary-prolog-files
      ((source ":- op(0, xfx, totally_nonexistent_test_op)."))
    (let ((rulebase (make-rulebase)))
      (consult-prolog source rulebase)
      (consult-prolog source rulebase)
      (is (null (cl-prolog::%operator-table-find
                 (cl-prolog::rulebase-operator-table rulebase)
                 'cl-prolog::totally_nonexistent_test_op :xfx))))))

(deftest source-loader-reload-deduplicates-repeated-operator-effects ()
  ;; The operator name is quoted so re-parsing the directive on reload does
  ;; not trip over its own freshly-registered infix status.
  (with-temporary-prolog-files
      ((source ":- op(700, xfx, 'dup_test_op'). :- op(800, xfx, 'dup_test_op')."))
    (let ((rulebase (make-rulebase)))
      (consult-prolog source rulebase)
      (consult-prolog source rulebase)
      (is (= 800
             (cl-prolog::operator-definition-priority
              (first (cl-prolog::%operator-table-find
                      (cl-prolog::rulebase-operator-table rulebase)
                      (cl-prolog::%prolog-atom-symbol "dup_test_op" :preserve-case t)
                      :xfx))))))))

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
           (rewrite-prolog-file! consulted "nested_consulted.")
           (rewrite-prolog-file! loaded "nested_loaded.")
           (rewrite-prolog-file!
            entry
            ":- consult('nested/consulted.pl'). :- load_files(['nested/loaded.pl']).")
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
      (rewrite-prolog-file! source "new_clause.")
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
      (rewrite-prolog-file! source "replacement_clause.")
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
      (rewrite-prolog-file! source "transient_source_clause. ?- invalid.")
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
             (sb-ext:with-timeout 10
               (uiop:run-program (list "ln" "-s" (namestring source)
                                       (namestring link))))
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
    (rewrite-prolog-file!
     first (format nil "first_loaded. :- ensure_loaded(~A)."
                   (%prolog-path-atom second)))
    (rewrite-prolog-file!
     second (format nil "second_loaded. :- ensure_loaded(~A)."
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
        "catch(load_files([], [if(not_loaded), if(not_loaded)]), error(domain_error(load_option, _), _), true)"))
  (:is (%source-query-succeeds-p
        (make-rulebase)
        "catch(load_files([], [if(not_loaded, extra)]), error(domain_error(load_option, _), _), true)")))

(defmacro deftest-source-loading-error (name &body query-form)
  `(deftest ,name ()
     (let ((rulebase (make-rulebase))
           (missing (%temporary-prolog-pathname)))
       (declare (ignorable missing))
       (is (%source-query-succeeds-p rulebase (progn ,@query-form))))))

(deftest-source-loading-error source-loading-instantiation-error
  "catch(consult(Source), error(instantiation_error, _), true)")

(deftest-source-loading-error source-loading-scalar-type-error
  "catch(consult(42), error(type_error(atom, 42), _), true)")

(deftest-source-loading-error source-loading-improper-list-type-error
  (format nil
          "catch(load_files([~A | tail]), ~
           error(type_error(list, [~A | tail]), _), true)"
          (%prolog-path-atom missing)
          (%prolog-path-atom missing)))

(deftest-source-loading-error source-loading-partial-list-instantiation-error
  (format nil
          "catch(load_files([~A | Tail]), error(instantiation_error, _), true)"
          (%prolog-path-atom missing)))

(deftest-source-loading-error source-loading-variable-list-item-instantiation-error
  "catch(load_files([Source]), error(instantiation_error, _), true)")

(deftest-source-loading-error source-loading-missing-source-error
  (format nil
          "catch(consult(~A), ~
           error(existence_error(source_sink, ~A), _), true)"
          (%prolog-path-atom missing)
          (%prolog-path-atom missing)))

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
      (rewrite-prolog-file! first "replacement(first).")
      (consult-prolog first rulebase)
      (is (cl-prolog::%rulebase-tabled-p
           rulebase 'cl-prolog::shared_table 1))
      (rewrite-prolog-file! second "replacement(second).")
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
