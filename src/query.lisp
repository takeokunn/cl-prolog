;;;; Public query API.
;;;;
;;;; MAP-PROLOG-SOLUTIONS is the primitive: it exposes the engine's CPS
;;;; contract directly and streams solutions as they are proven.  The other
;;;; entry points are conveniences layered on top of it.

(in-package #:fx.prolog)

(defun %project-bindings (query environment)
  "Return an alist mapping each variable of QUERY to its solved value."
  (mapcar (lambda (variable)
            (cons variable (logic-substitute variable environment)))
          (%collect-variables query)))

(defun map-prolog-solutions (function rulebase query
                             &key (max-depth *max-prolog-depth*)
                                  environment
                                  (project t)
                                  limit)
  "Prove QUERY against RULEBASE, calling FUNCTION once per solution.

Solutions stream to FUNCTION as they are proven.  With PROJECT (the
default) FUNCTION receives an alist of query-variable bindings; otherwise
it receives the raw environment.  LIMIT, when non-NIL, stops the search
after that many solutions.  Returns NIL."
  (unless (typep limit '(or null (integer 1)))
    (error "MAP-PROLOG-SOLUTIONS: :LIMIT must be NIL or a positive integer, got ~S."
           limit))
  (let ((remaining limit))
    (block search
      (%prove-goal-sequence
       (%normalize-query query) rulebase environment max-depth
       (lambda (environment)
         (funcall function (if project
                               (%project-bindings query environment)
                               environment))
         (when (and remaining (zerop (decf remaining)))
           (return-from search)))))
    nil))

(defun query-prolog (rulebase query &key (max-depth *max-prolog-depth*)
                                         environment
                                         (project t)
                                         limit)
  "Return the list of solutions for QUERY against RULEBASE.

Each solution is an alist of query-variable bindings (or a raw environment
when PROJECT is NIL).  LIMIT bounds the number of solutions returned."
  (let ((solutions '()))
    (map-prolog-solutions (lambda (solution) (push solution solutions))
                          rulebase query
                          :max-depth max-depth
                          :environment environment
                          :project project
                          :limit limit)
    (nreverse solutions)))

(defun query-prolog-first (rulebase query &key (max-depth *max-prolog-depth*))
  "Return the first solution for QUERY, or NIL when it has no proof."
  (first (query-prolog rulebase query :max-depth max-depth :limit 1)))

(defun prolog-succeeds-p (rulebase query &key (max-depth *max-prolog-depth*))
  "Return true when QUERY has at least one proof in RULEBASE."
  (%provable-p query rulebase '() max-depth))

(defun solution-binding (variable solution)
  "Return VARIABLE's value from a solution alist."
  (cdr (assoc variable solution)))
