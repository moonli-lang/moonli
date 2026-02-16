(uiop:define-package :moonli-user
  (:mix-reexport #:cl #:let-plus #:for #:parse-float)
  (:local-nicknames (#:json #:com.inuoe.jzon))
  (:import-from #:moonli #:lm #:ifelse)
  (:export #:lm
           #:ifelse))
