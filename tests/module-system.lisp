(in-package #:cl-prolog.tests)

(deftest module-registry-declaration-and-resolution ()
  (let ((registry (cl-prolog::make-module-registry)))
    (cl-prolog::module-registry-declare!
     registry 'lists '((/ member 2) (/ append 3)))
    (cl-prolog::module-registry-declare! registry 'client '())
    (cl-prolog::module-registry-import! registry 'client 'lists '((/ member 2)))
    (is (cl-prolog::module-registry-exported-p registry 'lists 'member 2))
    (is (not (cl-prolog::module-registry-exported-p registry 'lists 'member 3)))
    (is-equal 'lists
              (cl-prolog::module-registry-resolve
               registry 'client 'member 2
               (lambda (module predicate arity)
                 (declare (ignore module predicate arity)) nil)))
    (is-equal 'client
              (cl-prolog::module-registry-resolve
               registry 'client 'member 2
               (lambda (module predicate arity)
                 (declare (ignore predicate arity)) (eq module 'client))))))

(deftest module-registry-qualified-resolution ()
  (let ((registry (cl-prolog::make-module-registry)))
    (cl-prolog::module-registry-declare! registry 'hidden '())
    (is-equal 'hidden
              (cl-prolog::module-registry-resolve-qualified
               registry 'hidden 'private 1
               (lambda (module predicate arity)
                 (equal (list module predicate arity) '(hidden private 1)))))
    (is (not (cl-prolog::module-registry-resolve-qualified
              registry 'hidden 'missing 0
              (lambda (module predicate arity)
                (declare (ignore module predicate arity)) nil))))))

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
      => (((?module . cl-prolog::user))
          ((?module . alpha))
          ((?module . zeta))))
    (assert-query rulebase (cl-prolog::current_module alpha) :succeeds)
    (assert-query rulebase (cl-prolog::current_module missing) :fails)
    (assert-query rulebase (cl-prolog::current_module 42) :signals)))

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
    (is (not (prolog-succeeds-p rulebase
                                (read-prolog-term "stored(alpha)."))))
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

(deftest dynamic-assertion-rejects-import-redefinition ()
  (let ((rulebase (make-rulebase)))
    (consult-prolog ":- module(alpha, [value/1]). value(alpha)." rulebase)
    (consult-prolog ":- use_module(alpha)." rulebase)
    (signals-error
      (query-prolog rulebase (read-prolog-term "assertz(value(local)).")))
    (is (prolog-succeeds-p rulebase (read-prolog-term "value(alpha).")))
    (is (not (prolog-succeeds-p rulebase
                                (read-prolog-term "value(local)."))))))
