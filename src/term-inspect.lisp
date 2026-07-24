;;;; Term type-inspection builtins: var/nonvar/atom/atomic/number/integer/
;;;; float/compound/callable/ground/acyclic_term/cyclic_term, and the
;;;; %term-resolve/%term-atom-p/%term-compound-p/%term-acyclic-p primitives
;;;; they share.

(in-package #:cl-prolog)

(defun %term-resolve (term environment)
  "Resolve TERM's variables through ENVIRONMENT, preserving structure sharing.

Delegates to LOGIC-SUBSTITUTE, which builds ENVIRONMENT's lookup index once
and reuses it for every node -- unlike a naive per-node %WALK-TERM call,
which would rebuild that index on every recursive step."
  (logic-substitute term environment))

(defun %term-atom-p (term)
  (and (symbolp term) (not (logic-var-p term))))

(defun %term-compound-p (term)
  (and (consp term)
       (%proper-list-p term)
       (%term-atom-p (first term))))

(defun %term-acyclic-p (term &optional (environment '()))
  "Return true when TERM contains no cycle through cons cells."
  (let ((active (make-hash-table :test #'eq))
        (complete (make-hash-table :test #'eq)))
    (labels ((walk (node)
               (let ((resolved (%walk-term node environment)))
                 (cond
                   ((atom resolved) t)
                   ((gethash resolved complete) t)
                   ((gethash resolved active) nil)
                   (t
                    (setf (gethash resolved active) t)
                    (let ((acyclic-p (and (walk (car resolved))
                                          (walk (cdr resolved)))))
                      (remhash resolved active)
                      (when acyclic-p
                        (setf (gethash resolved complete) t))
                      acyclic-p))))))
      (walk term))))

(defmacro define-term-predicate-builtin (&body definitions)
  "Define ISO term type-check builtins that test a single resolved term.
Each of DEFINITIONS is (NAME PREDICATES &key NEGATE); PREDICATES is a single
predicate symbol, or a list of predicate symbols combined with OR. The
builtin succeeds, without bindings, when the (disjunction of the) predicate
holds of the resolved term, or fails to hold when NEGATE is true."
  `(progn
     ,@(loop for (name predicates &key negate) in definitions
             collect
             (let ((predicate-list (if (listp predicates) predicates (list predicates))))
               `(define-builtin (,name term) (rulebase environment depth emit)
                  (declare (cl:ignore rulebase depth))
                  (let ((resolved-term (%term-resolve term environment)))
                    (,(if negate 'unless 'when)
                     (or ,@(mapcar (lambda (predicate) `(,predicate resolved-term))
                                   predicate-list))
                     (funcall emit environment))))))))

(define-term-predicate-builtin
  (var logic-var-p)
  (nonvar logic-var-p :negate t)
  (atom %term-atom-p)
  (number %prolog-number-p)
  (integer integerp)
  (float floatp)
  (compound %term-compound-p)
  (ground %term-has-variables-p :negate t)
  (atomic (%term-atom-p %prolog-number-p))
  (callable (%term-atom-p %term-compound-p)))

(define-builtin (acyclic_term term) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (when (%term-acyclic-p term environment)
    (funcall emit environment)))

(define-builtin (cyclic_term term) (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (unless (%term-acyclic-p term environment)
    (funcall emit environment)))
