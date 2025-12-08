(in-package :moonli)

(5am:in-suite :moonli)

(esrap:defrule defpackage/option
    (and string-designator
         *whitespace
         (esrap:? (or (and string-designator
                           *whitespace
                           (* (and #\,
                                   *whitespace
                                   string-designator
                                   *whitespace)))
                      (+ (and *whitespace
                              string-designator
                              *whitespace
                              #\,
                              *whitespace))))
         #\;
         *whitespace)
  (:function (lambda (expr)
               (optima:ematch expr
                 ((list option-name _ args _ _)
                  `(,(intern (string-upcase option-name) :keyword)
                    ,@(if (null (nthcdr 3 args)) ; length=3, first option
                          (cons (first args)
                                (mapcar #'third (third args)))
                          (mapcar #'second args))))))))

(define-moonli-macro defpackage
  ((name string-designator)
   (_ *whitespace)
   (options (* defpackage/option)))
  (let ((form `(defpackage ,name ,@options)))
    (eval form)
    form))

(def-test defpackage (macro-call)
  (:moonli "defpackage foo
  :use cl;
end"
   :lisp (defpackage foo (:use cl))))
