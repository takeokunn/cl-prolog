;;;; Collection builtin tests: findall/bagof/setof, sort/msort/keysort, and
;;;; the standard-order-of-terms variable/variant grouping they share.

(in-package #:cl-prolog.tests)

(deftest-queries solution-collection-builtins
    ((make-rulebase
      :clauses (list (make-clause '(edge a 2))
                     (make-clause '(edge a 1))
                     (make-clause '(edge a 2))
                     (make-clause '(edge b 3))
                     (make-clause '(edge3 a left 1))
                     (make-clause '(edge3 a right 2))
                     (make-clause '(edge3 b left 3)))))
  ((findall ?value (edge a ?value) ?bag)
                                   :ordered (((?value . ?value) (?bag 2 1 2))))
  ((findall ?value (edge missing ?value) ?bag)
                                   :ordered (((?value . ?value) (?bag))))
  ((findall ?value (edge missing ?value) ?bag ?tail)
                                   :ordered (((?value . ?value) (?bag . ?tail)
                                        (?tail . ?tail))))
  ((bagof ?value (edge ?key ?value) ?bag)
                                   :ordered (((?value . ?value) (?key . a) (?bag 2 1 2))
                                       ((?value . ?value) (?key . b) (?bag 3))))
  ((bagof ?value (^ ?key (edge ?key ?value)) ?bag)
                                   :ordered (((?value . ?value) (?key . ?key)
                                        (?bag 2 1 2 3))))
  ((bagof ?value (^ (pair ?key ?side) (edge3 ?key ?side ?value)) ?bag)
                                   :ordered (((?value . ?value) (?key . ?key)
                                        (?side . ?side) (?bag 1 2 3))))
  ((bagof ?value (^ ?key (^ ?side (edge3 ?key ?side ?value))) ?bag)
                                   :ordered (((?value . ?value) (?key . ?key)
                                        (?side . ?side) (?bag 1 2 3))))
  ((bagof ?value (edge missing ?value) ?bag) :fails)
  ((setof ?value (edge ?key ?value) ?bag)
                                   :ordered (((?value . ?value) (?key . a) (?bag 1 2))
                                       ((?value . ?value) (?key . b) (?bag 3))))
  ((setof ?value (edge missing ?value) ?bag) :fails))

(deftest findall-difference-list-preserves-ground-tail ()
  (let ((rulebase
          (make-rulebase
           :clauses (list (make-clause '(edge a 2))
                          (make-clause '(edge a 1))
                          (make-clause '(edge a 2))))))
    (is-equal '(2 1 2 . tail)
              (solution-binding
               '?bag
               (query-prolog-first
                rulebase '(findall ?value (edge a ?value) ?bag tail)))
    (is-equal 'tail
              (solution-binding
               '?bag
               (query-prolog-first
                rulebase '(findall ?value (edge missing ?value) ?bag tail)))))))

(deftest-queries sorting-builtins ((make-rulebase))
  ((sort (3 1 2 1) ?sorted) :ordered (((?sorted 1 2 3))))
  ((msort (3 1 2 1) ?sorted) :ordered (((?sorted 1 1 2 3))))
  ((sort () ?sorted) :ordered (((?sorted))))
  ((sort (z 2 a 1) ?sorted) :ordered (((?sorted 1 2 a z))))
  ((sort ((a first second) (z item)) ?sorted)
   :ordered (((?sorted (z item) (a first second)))))
  ((sort (z cl-prolog.user-atoms::car) ?sorted)
   :ordered (((?sorted cl-prolog.user-atoms::car z))))
  ((keysort ((- 2 second) (- 1 first) (- 2 third)) ?sorted)
   :ordered (((?sorted (- 1 first) (- 2 second) (- 2 third))))))

(deftest-queries sorting-builtins-report-iso-errors ((make-rulebase))
  ((sort ?input ?sorted) :signals)
  ((sort improper ?sorted) :signals)
  ((msort ?input ?sorted) :signals)
  ((msort improper ?sorted) :signals)
  ((keysort ((not-a-pair)) ?sorted) :signals))

(deftest collection-builtins-use-standard-variable-order-and-variants ()
  (let ((rulebase
          (make-rulebase
           :clauses (list (make-clause '(variant-key (pair ?left ?left) first))
                          (make-clause '(variant-key (pair ?right ?right) second))
                          (make-clause '(ordered-key z last))
                          (make-clause '(ordered-key a first))))))
    (let* ((solutions
             (query-prolog
              rulebase '(bagof ?value (variant-key ?key ?value) ?bag)))
           (solution (first solutions))
           (key (solution-binding '?key solution)))
      (is (= 1 (length solutions)))
      (is-equal '(first second) (solution-binding '?bag solution))
      (is (eq (second key) (third key))))
    (assert-query rulebase
                  (bagof ?value (ordered-key ?key ?value) ?bag)
                  :ordered
                  (((?value . ?value) (?key . a) (?bag first))
                   ((?value . ?value) (?key . z) (?bag last))))
    (with-single-query-solution
        (solution solutions rulebase
         (list 'sort
               (list (list 'pair '?left '?left)
                     (list 'pair '?right '?right))
               '?sorted))
      (let ((sorted (solution-binding '?sorted solution)))
        (is (= 2 (length sorted)))
        (is (not (eq (second (first sorted))
                     (second (second sorted))))))))
  (let* ((representative (list 'same))
         (equivalent (list 'same))
         (other (list 'z))
         (groups
           (cl-prolog::%partition-solution-groups
            (list (cons other 'last)
                  (cons representative 'first)
                  (cons equivalent 'second))))
         (same-group (find representative groups :key #'car :test #'eq))
         (other-group (find other groups :key #'car :test #'eq)))
    (is (= 2 (length groups)))
    (is (eq representative (car same-group)))
    (is-equal '(first second) (cdr same-group))
    (is-equal '(last) (cdr other-group)))
  (let ((first-cycle (list 'cycle))
        (second-cycle (list 'cycle)))
    (setf (cdr first-cycle) first-cycle
          (cdr second-cycle) second-cycle)
    (let* ((groups
             (cl-prolog::%partition-solution-groups
              (list (cons first-cycle 'first)
                    (cons second-cycle 'second))))
           (group (first groups)))
      (is (= 1 (length groups)))
      (is (eq first-cycle (car group)))
      (is-equal '(first second) (cdr group)))))
