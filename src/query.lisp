;;;; Public query API.
;;;;
;;;; MAP-PROLOG-SOLUTIONS is the primitive: it exposes the engine's CPS
;;;; contract directly and streams solutions as they are proven.  The other
;;;; entry points are conveniences layered on top of it.

(in-package #:cl-prolog)

(defun %collect-query-variables (query)
  "Collect public variables, excluding variables scoped by FORALL/2."
  (let ((variables '())
        (seen-conses (make-hash-table :test #'eq)))
    (labels ((walk (term)
               (cond
                 ((logic-var-p term)
                  (pushnew term variables :test #'eq))
                 ((and (consp term) (eq (first term) 'forall))
                  nil)
                 ((consp term)
                  (unless (gethash term seen-conses)
                    (setf (gethash term seen-conses) t)
                    (walk (car term))
                    (walk (cdr term)))))))
      (walk query))
    (nreverse variables)))

(defun %project-bindings (query environment)
  "Return an alist mapping each variable of QUERY to its solved value."
  (mapcar (lambda (variable)
            (cons variable (logic-substitute variable environment)))
          (%collect-query-variables query)))

(defun %query-option (options key default)
  "Return KEY from OPTIONS, or DEFAULT when KEY is absent."
  (let ((missing (gensym "MISSING")))
    (let ((value (getf options key missing)))
      (if (eq value missing) default value))))

(defun %decode-query-options (options)
  "Return query option values as (VALUES MAX-DEPTH ENVIRONMENT PROJECT LIMIT)."
  (unless (evenp (length options))
    (error 'program-error))
  (loop for key in options by (function cddr)
        do (unless (member key (quote (:max-depth :environment :project :limit)))
             (error 'program-error)))
  (let ((limit (%query-option options :limit nil)))
    (unless (typep limit (quote (or null (integer 1 *))))
      (error 'type-error
             :datum limit
             :expected-type (quote (or null (integer 1 *)))))
    (values (%validate-max-depth
             (%query-option options :max-depth *max-prolog-depth*))
            (%query-option options :environment nil)
            (%query-option options :project t)
            limit)))

(defmacro %with-query-options ((options max-depth environment project limit) &body body)
  "Bind public query option names decoded from OPTIONS."
  `(multiple-value-bind (,max-depth ,environment ,project ,limit)
       (%decode-query-options ,options)
     ,@body))

(defun %map-prolog-solutions* (function rulebase query max-depth environment project limit)
  (unless (typep limit '(or null (integer 1)))
    (error "MAP-PROLOG-SOLUTIONS: :LIMIT must be NIL or a positive integer, got ~S."
           limit))
  (%with-logic-variable-order
    (%collect-variables query)
    (let ((remaining limit)
          (cut-tag (%make-cut-tag)))
      (block search
        (cl:catch cut-tag
          (%prove-goals/k
           (%normalize-query query)
           (%make-proof-state rulebase environment max-depth
                              +default-prolog-module+
                              (%make-rulebase-table-session rulebase)
                              cut-tag)
           (lambda (state)
             (let ((bindings (proof-state-bindings state)))
               (funcall function (if project
                                     (%project-bindings query bindings)
                                     bindings)))
             (when (and remaining (zerop (decf remaining)))
               (return-from search))))))
      nil)))

(defun map-prolog-solutions (function rulebase query &rest options)
  "Prove QUERY against RULEBASE, calling FUNCTION once per solution.

Solutions stream to FUNCTION as they are proven.  With PROJECT (the
default) FUNCTION receives an alist of query-variable bindings; otherwise
it receives the raw environment.  LIMIT, when non-NIL, stops the search
after that many solutions.  Returns NIL."
  (%with-query-options (options max-depth environment project limit)
    (%map-prolog-solutions* function rulebase query
                            max-depth environment project limit)))

(defun query-prolog (rulebase query &rest options)
  "Return the list of solutions for QUERY against RULEBASE.

Each solution is an alist of query-variable bindings (or a raw environment
when PROJECT is NIL).  LIMIT bounds the number of solutions returned."
  (%with-query-options (options max-depth environment project limit)
    (let ((solutions '()))
      (%map-prolog-solutions* (lambda (solution) (push solution solutions))
                              rulebase query
                              max-depth environment project limit)
      (nreverse solutions))))

(defun query-prolog-first (rulebase query &rest options)
  "Return the first solution for QUERY and whether a proof was found.

The primary value remains NIL when a ground query succeeds, preserving the
solution representation and existing callers.  The secondary value
distinguishes that case from failure."
  (%with-query-options (options max-depth environment project limit)
    (declare (cl:ignore limit))
    (block first-solution
      (%map-prolog-solutions* (lambda (solution)
                                (return-from first-solution
                                  (values solution t)))
                              rulebase query
                              max-depth environment project 1)
      (values nil nil))))

(defun prolog-succeeds-p (rulebase query &key (max-depth *max-prolog-depth*))
  "Return true when QUERY has at least one proof in RULEBASE."
  (%provable-p query rulebase '() (%validate-max-depth max-depth)))

(defun solution-binding (variable solution)
  "Return VARIABLE's value from a solution alist."
  (cdr (assoc variable solution)))
