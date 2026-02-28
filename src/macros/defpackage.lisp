(in-package :moonli)

(5am:in-suite :moonli)

(esrap:defrule defpackage/option/arg
    (or (and string-designator +whitespace/internal #\= +whitespace/internal string-designator)
        string-designator)
  (:function (lambda (expr)
               (if (atom expr)
                   expr
                   (list (first expr) (fifth expr))))))

(esrap:defrule defpackage/option
    (and string-designator
         *whitespace
         (esrap:? (or (and defpackage/option/arg
                           *whitespace
                           (* (and #\,
                                   *whitespace
                                   defpackage/option/arg
                                   *whitespace)))
                      (+ (and *whitespace
                              defpackage/option/arg
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
  :local-nicknames :a = :alexandria;
end"
   :lisp (defpackage foo (:use cl)
                     (:local-nicknames (:a :alexandria)))))
