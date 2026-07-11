;;;; Package for the cl-weave based test suite.
;;;;
;;;; This suite exercises the public cl-prolog surface through cl-weave's
;;;; Vitest-shaped `describe` / `it` / `expect` DSL.  It runs independently of
;;;; the homegrown `fx.prolog.tests` harness so both can coexist during and
;;;; after adoption.

(defpackage #:cl-prolog/weave-tests
  (:use #:cl)
  ;; cl-weave shadows CL:DESCRIBE, so it must be shadowing-imported.
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:expect)
  ;; Import only the public cl-prolog symbols the suite uses.  We deliberately
  ;; avoid (:use #:fx.prolog) so CL:/= does not collide with FX.PROLOG:/=.
  (:import-from #:fx.prolog
                #:prolog
                #:query-prolog
                #:query-prolog-first
                #:prolog-succeeds-p
                #:unify
                #:logic-substitute
                #:make-rulebase))
