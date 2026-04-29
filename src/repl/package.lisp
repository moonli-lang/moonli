(uiop:define-package :moonli-user
  (:mix-reexport #:cl #:let-plus #:for #:parse-float)
  (:import-from #:moonli #:lm #:ifelse)
  (:export #:lm
           #:ifelse))

(trivial-package-local-nicknames:add-package-local-nickname '#:json '#:com.inuoe.jzon '#:moonli-user)

(defpackage :moonli/repl
  (:use :cl)
  (:local-nicknames (:ic :isocline)
                    (:ic-repl :isocline-repl))
  (:import-from #:isocline-repl
                #:completer
                #:highlighter)
  (:import-from #:moonli
                #:process-option)
  (:export #:main))
