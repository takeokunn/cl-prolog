(in-package #:cl-prolog)

(define-source-loading-builtin (consult source) 'consult
  (consult-prolog (%source-file-pathnames source environment 'consult)
                  rulebase))

(define-source-loading-builtin (load_files sources) 'load_files
  (consult-prolog (%source-file-pathnames sources environment 'load_files)
                  rulebase))

(define-source-loading-builtin (ensure_loaded sources) 'ensure_loaded
  (ensure-prolog-loaded (%source-file-pathnames sources environment
                                                'ensure_loaded)
                        rulebase))

(define-source-loading-builtin (load_files sources options) 'load_files
  (let ((if-loaded (%load-files-if-loaded-policy
                    options environment 'load_files))
        (pathnames (%source-file-pathnames sources environment 'load_files)))
    (with-prolog-loading-transaction (rulebase transaction initializations)
      (%load-prolog-source-into-rulebase
       pathnames transaction initializations :if-loaded if-loaded))))
