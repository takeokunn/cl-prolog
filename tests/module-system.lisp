(in-package #:cl-prolog.tests)

(deftest module-registry-declaration-and-resolution ()
  (let ((registry (cl-prolog::make-module-registry))
        (calls (quote ())))
    (cl-prolog::module-registry-declare!
     registry (quote lists) (quote ((/ member 2) (/ append 3))))
    (cl-prolog::module-registry-declare! registry (quote client) (quote ()))
    (cl-prolog::module-registry-import!
     registry (quote client) (quote lists) (quote ((/ member 2))))
    (is (cl-prolog::module-registry-exported-p
         registry (quote lists) (quote member) 2))
    (is (not (cl-prolog::module-registry-exported-p
              registry (quote lists) (quote member) 3)))
    (is-equal (quote lists)
              (cl-prolog::module-registry-resolve
               registry (quote client) (quote member) 2
               (lambda (module predicate arity)
                 (push (list module predicate arity) calls)
                 nil)))
    (is-equal (quote ((client member 2))) calls)
    (setf calls (quote ()))
    (is-equal (quote client)
              (cl-prolog::module-registry-resolve
               registry (quote client) (quote member) 2
               (lambda (module predicate arity)
                 (push (list module predicate arity) calls)
                 (eq module (quote client)))))
    (is-equal (quote ((client member 2))) calls)))

(deftest module-registry-qualified-resolution ()
  (let ((registry (cl-prolog::make-module-registry))
        (calls (quote ())))
    (cl-prolog::module-registry-declare! registry (quote hidden) (quote ()))
    (is-equal (quote hidden)
              (cl-prolog::module-registry-resolve-qualified
               registry (quote hidden) (quote private) 1
               (lambda (module predicate arity)
                 (push (list module predicate arity) calls)
                 (equal (list module predicate arity)
                        (quote (hidden private 1))))))
    (is-equal (quote ((hidden private 1))) calls)
    (setf calls (quote ()))
    (is (not (cl-prolog::module-registry-resolve-qualified
              registry (quote hidden) (quote missing) 0
              (lambda (module predicate arity)
                (push (list module predicate arity) calls)
                nil))))
    (is-equal (quote ((hidden missing 0))) calls)))

(deftest module-registry-declare-rejects-malformed-declarations ()
  (let ((registry (cl-prolog::make-module-registry)))
    (signals-error
      (cl-prolog::module-registry-declare! registry "not-an-atom" '()))
    (cl-prolog::module-registry-declare! registry 'once '())
    (signals-error
      (cl-prolog::module-registry-declare! registry 'once '()))
    (signals-error
      (cl-prolog::module-registry-declare!
       registry 'duplicated '((/ same 1) (/ same 1))))
    (signals-error
      (cl-prolog::module-registry-declare!
       registry 'malformed '((/ same))))
    (signals-error
      (cl-prolog::module-registry-declare!
       registry 'too-long '((/ same 1 extra))))
    (signals-error
      (cl-prolog::module-registry-declare!
       registry 'wrong-functor '((not-a-slash same 1))))
    (signals-error
      (cl-prolog::%find-prolog-module registry "not-an-atom" "test"))))

(deftest module-registry-rejects-invalid-imports ()
  (let ((registry (cl-prolog::make-module-registry)))
    (cl-prolog::module-registry-declare! registry 'left '((/ same 1)))
    (cl-prolog::module-registry-declare! registry 'right '((/ same 1)))
    (cl-prolog::module-registry-declare! registry 'client '())
    (cl-prolog::module-registry-import! registry 'client 'left)
    (signals-error
      (cl-prolog::module-registry-import! registry 'client 'right))
    (signals-error
      (cl-prolog::module-registry-import! registry 'client 'left '((/ hidden 1))))))

(deftest module-registry-rejects-import-redefinition-and-undefined-export ()
  (let ((registry (cl-prolog::make-module-registry)))
    (cl-prolog::module-registry-declare! registry 'library '((/ public 1)))
    (cl-prolog::module-registry-declare! registry 'client '())
    (cl-prolog::module-registry-import! registry 'client 'library)
    (signals-error
      (cl-prolog::module-registry-ensure-definition-allowed
       registry 'client 'public 1))
    (signals-error
      (cl-prolog::module-registry-validate-exports
       registry 'library
       (lambda (module predicate arity)
         (declare (ignore module predicate arity)) nil)))))

(deftest module-registry-copy-is-independent ()
  (let* ((registry (cl-prolog::make-module-registry))
         (copy (cl-prolog::module-registry-copy registry)))
    (cl-prolog::module-registry-declare! copy 'new-module '())
    (signals-error
      (cl-prolog::module-registry-resolve-qualified
       registry 'new-module 'anything 0
       (lambda (module predicate arity)
                 (declare (ignore module predicate arity)) t)))))

(deftest current-module-reflects-module-registry ()
  (let ((rulebase (make-rulebase)))
    (cl-prolog::module-registry-declare!
     (cl-prolog::rulebase-module-registry rulebase) 'zeta '())
    (cl-prolog::module-registry-declare!
     (cl-prolog::rulebase-module-registry rulebase) 'alpha '())
    (assert-query rulebase (cl-prolog::current_module ?module)
      :ordered (((?module . cl-prolog::user))
          ((?module . alpha))
          ((?module . zeta))))
    (assert-query rulebase (cl-prolog::current_module alpha) :succeeds)
    (assert-query rulebase (cl-prolog::current_module missing) :fails)
    (assert-query rulebase (cl-prolog::current_module 42) :signals prolog-type-error)))

(deftest module-consult-isolates-colliding-predicates ()
  (let ((rulebase (make-rulebase)))
    (consult-prolog ":- module(alpha, [value/1]). value(alpha)." rulebase)
    (consult-prolog ":- module(beta, [value/1]). value(beta)." rulebase)
    (is (prolog-succeeds-p rulebase
                           (read-prolog-term "alpha:value(alpha).")))
    (is (not (prolog-succeeds-p rulebase
                                (read-prolog-term "alpha:value(beta)."))))
    (is (prolog-succeeds-p rulebase
                           (read-prolog-term "beta:value(beta).")))
    (is (not (prolog-succeeds-p rulebase
                                (read-prolog-term "beta:value(alpha)."))))))

(deftest module-import-resolves-unqualified-goals ()
  (let ((rulebase (make-rulebase)))
    (consult-prolog ":- module(alpha, [value/1]). value(alpha)." rulebase)
    (consult-prolog
     ":- use_module(alpha). selected(X) :- value(X)."
     rulebase)
    (is (prolog-succeeds-p rulebase
                           (read-prolog-term "selected(alpha).")))
    (is (not (prolog-succeeds-p rulebase
                                (read-prolog-term "selected(beta)."))))))

(deftest qualified-builtins-use-explicit-module ()
  (let ((rulebase (make-rulebase)))
    (consult-prolog ":- module(alpha, [value/1]). value(alpha)." rulebase)
    (is (prolog-succeeds-p rulebase
                           (read-prolog-term "alpha:call(value(alpha)).")))
    (is (prolog-succeeds-p rulebase
                           (read-prolog-term "alpha:assertz(stored(alpha)).")))
    (is (prolog-succeeds-p rulebase
                           (read-prolog-term "alpha:stored(alpha).")))
    (signals-error
      (prolog-succeeds-p rulebase
                         (read-prolog-term "stored(alpha).")))
    (is (not (prolog-succeeds-p rulebase
                                (read-prolog-term "alpha:not(value(alpha))."))))
    (is (prolog-succeeds-p rulebase
                           (read-prolog-term "alpha:not(value(beta)).")))))

(deftest qualified-builtin-rejects-unknown-module ()
  (signals-error
    (query-prolog (make-rulebase)
                  (read-prolog-term "ghost:true."))))

(deftest qualified-module-variable-resolves-through-bindings ()
  (let ((rulebase (make-rulebase)))
    (consult-prolog
     ":- module(alpha, [value/1]). value(alpha)."
     rulebase)
    (is (prolog-succeeds-p
         rulebase
         (read-prolog-term "Module = alpha, Module:value(alpha).")))))

(deftest qualified-module-errors-are-catchable ()
  (let ((rulebase (make-rulebase)))
    (is (prolog-succeeds-p
         rulebase
         (read-prolog-term
          "catch(Module:true, error(instantiation_error, _), true).")))
    (is (prolog-succeeds-p
         rulebase
         (read-prolog-term
          "catch(42:true, error(type_error(atom, 42), _), true).")))
    (is (prolog-succeeds-p
         rulebase
         (read-prolog-term
          "catch(unknown_module:true, error(existence_error(module, unknown_module), _), true).")))))

(deftest current-predicate-includes-imported-predicates ()
  (let ((rulebase (make-rulebase)))
    (consult-prolog
     ":- module(alpha, [value/1]). value(alpha)."
     rulebase)
    (consult-prolog
     ":- module(client, []). :- use_module(alpha)."
     rulebase)
    (is (prolog-succeeds-p
         rulebase
         (read-prolog-term "client:current_predicate(value/1).")))))

(deftest module-consult-rolls-back-import-redefinition ()
  (let ((rulebase (make-rulebase)))
    (consult-prolog ":- module(alpha, [value/1]). value(alpha)." rulebase)
    (signals-error
      (consult-prolog
       ":- use_module(alpha). value(local)."
       rulebase))
    (is (prolog-succeeds-p rulebase
                           (read-prolog-term "alpha:value(alpha).")))
    (signals-error
      (prolog-succeeds-p rulebase
                         (read-prolog-term "value(local).")))))

(deftest module-directive-must-be-the-unique-first-source-term ()
  (signals-error
    (consult-prolog "already_seen. :- module(late, []).")))

(deftest module-consult-rejects-an-undefined-export ()
  (signals-error
    (consult-prolog ":- module(under_defined, [missing/1]).")))

(deftest dynamic-assertion-rejects-import-redefinition ()
  (let ((rulebase (make-rulebase)))
    (consult-prolog ":- module(alpha, [value/1]). value(alpha)." rulebase)
    (consult-prolog ":- use_module(alpha)." rulebase)
    (signals-error
      (query-prolog rulebase (read-prolog-term "assertz(value(local)).")))
    (is (prolog-succeeds-p rulebase (read-prolog-term "value(alpha).")))
    (is (not (prolog-succeeds-p rulebase
                                (read-prolog-term "value(local)."))))))
