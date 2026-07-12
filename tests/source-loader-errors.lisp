(in-package #:cl-prolog.tests)

(defmacro deftest-source-loading-error (name &body query-form)
  `(deftest ,name ()
     (let ((rulebase (make-rulebase))
           (missing (%temporary-prolog-pathname)))
       (is (%source-query-succeeds-p rulebase (progn ,@query-form))))))

(deftest-source-loading-error source-loading-missing-source-error
  (format nil
          "catch(consult(~A), ~
           error(existence_error(source_sink, ~A), _), true)"
          (%prolog-path-atom missing)
          (%prolog-path-atom missing)))

(deftest-source-loading-error source-loading-variable-list-item-instantiation-error
  "catch(load_files([Source]), error(instantiation_error, _), true)")

(deftest-source-loading-error source-loading-partial-list-instantiation-error
  (format nil
          "catch(load_files([~A | Tail]), error(instantiation_error, _), true)"
          (%prolog-path-atom missing)))

(deftest-source-loading-error source-loading-improper-list-type-error
  (format nil
          "catch(load_files([~A | tail]), ~
           error(type_error(list, [~A | tail]), _), true)"
          (%prolog-path-atom missing)
          (%prolog-path-atom missing)))

(deftest-source-loading-error source-loading-scalar-type-error
  "catch(consult(42), error(type_error(atom, 42), _), true)")

(deftest-source-loading-error source-loading-instantiation-error
  "catch(consult(Source), error(instantiation_error, _), true)")

