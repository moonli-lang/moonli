(defpackage :moonli/expressions
  (:use)
  (:export #:symbol
           #:character
           #:cons
           #:list
           #:vector
           #:function-call
           #:function-arglist
           #:hash-table
           #:hash-set

           #:let
           #:defun
           #:if
           #:loop

           #:defpackage
           #:defvar
           #:defparameter
           #:in-package
           #:declare
           #:declaim
           #:lambda))


(defpackage :moonli
  (:use :cl :let-plus)
  (:export #:with-syntax
           #:mark-syntax
           #:syntax-positions
           #:*read-without-interning*

           #:moonli
           #:moonli-expression
           #:read-moonli-from-stream
           #:read-moonli-from-string
           #:moonli-string-to-lisp-string
           #:compile-moonli-file
           #:load-moonli-file
           #:transpile-moonli-file
           #:define-moonli-macro
           #:define-moonli-short-macro
           #:define-moonli-infix-macro

           #:moonli-pprint-object
           #:moonli-hash-table-pprint-indent*

           #:main)
  (:local-nicknames (:expr :moonli/expressions)))


(5am:def-suite :moonli)
(5am:in-suite :moonli)
