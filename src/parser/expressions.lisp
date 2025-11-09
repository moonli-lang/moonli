(in-package :moonli)

(5am:in-suite :moonli)

(esrap:defrule atomic-expression
    (or bracketed-expression
        chain
        quoted-expression
        expr:character
        string
        good-symbol
        number
        expr:vector
        expr:list
        expr:hash-table
        expr:hash-set))

(esrap:defrule moonli-expression
    (or comment
        macro-call
        short-macro-call
        infix-expression))

(esrap:defrule moonli-expression/whitespace
    (and *whitespace
         moonli-expression
         *whitespace)
  (:function second))

(esrap:defrule moonli
    (and *whitespace/all
         (and moonli-expression
              *whitespace/internal)
         (* (and *whitespace/all
                 moonli-expression
                 *whitespace/internal))
         *whitespace/all)
  (:function (lambda (exprs)
               `(progn
                  ,(first (second exprs))
                  ,@(mapcar #'second (third exprs))))))
