(in-package :moonli)

(5am:in-suite :moonli)

(esrap:defrule coalton-expression
    (or coalton:define
        coalton:declare
        coalton:fn
        infix-expression
        coalton-chain)
  (:error-report t))

(esrap:defrule coalton
    (and *whitespace/all
         (and coalton-expression
              *whitespace/internal)
         (* (and *whitespace/all
                 coalton-expression
                 *whitespace/internal))
         *whitespace/all)
  (:error-report :detail)
  (:function (lambda (exprs)
               `(progn
                  ,(first (second exprs))
                  ,@(mapcar #'second (third exprs))))))

(esrap:defrule coalton-chain
    (and atomic-expression
         (* expr:function-arglist))
  ;; This is based on the "chain" rule of moonli.
  ;; The main difference is that consecurive moonli function calls
  ;;   are transpiled with `funcall` forms
  ;; While this is not expected for coalton
  (:function (lambda (expr)
               (labels
                   ((build-prefix (expr)
                      (if (null (second expr))
                          (first expr)
                          (destructuring-bind (first (list/vector &rest rest-rest))
                              expr
                            (build-prefix
                             (ecase (first list/vector)
                               (list
                                (list (if (symbolp first)
                                          (cons first
                                                (rest list/vector))
                                          (list* first
                                                 (rest list/vector)))
                                      rest-rest))))))))
                 (build-prefix expr)))))

(define-moonli-macro coalton:coalton-toplevel
  ((_ (and ":" *whitespace/all))
   (body (esrap:? coalton)))
  `(coalton:coalton-toplevel
     ,@(rest body)))

(def-test coalton:coalton-toplevel (macro-call)
  (:lisp (coalton:coalton-toplevel)
   :moonli "coalton:coalton-toplevel :

end"))

(define-moonli-macro coalton:fn
  ((lambda-list lambda-parameter-list)
   (_ *whitespace/internal)
   (_ mandatory-colon)
   (_ *whitespace/internal)
   (body (esrap:? coalton)))
  `(coalton:fn ,lambda-list ,@(rest body)))


(esrap:defrule coalton-type
    infix-expression)

(define-moonli-short-macro coalton:declare
  ((var expr:symbol)
   (_ (and *whitespace/internal "::" *whitespace/internal))
   (type (and defmethod/lambda-list +whitespace/all "->" +whitespace/all coalton-type)))
  `(coalton:declare ,var (,@(first type) coalton:-> ,(fifth type))))

(def-test coalton:declare (short-macro-call)
  (:lisp (coalton:declare five (coalton:void coalton:-> coalton:integer))
   :moonli "coalton:declare five :: (coalton:void) -> coalton:integer")
  ;; FIXME: What is the syntax for forall types?
  ;; (:lisp (coalton:declare identity (coalton:forall coalton:-> coalton:integer))
  ;;  :moonli "coalton:declare identity :: forall(:item)(coalton:void) -> coalton:integer")
  (:lisp (coalton:declare make-default
                          (&key (:x coalton:Integer) coalton:-> coalton:Integer))
   :moonli "coalton:declare make-default :: (&key, :x :: coalton:integer) -> coalton:integer")
  (:lisp (coalton:declare sum-and-product
                          (coalton:integer * coalton:integer coalton:->
                                           coalton:integer * coalton:integer))
   :moonli "coalton:declare sum-and-product :: (coalton:integer * coalton:integer)
  -> coalton:integer * coalton:integer"))


(define-moonli-short-macro coalton:define
  ((fn-or-variable-forms
    (or (and expr:symbol
             +whitespace/all
             "="
             +whitespace/all
             moonli-expression)
        (and expr:symbol
             *whitespace/internal
             arglist
             *whitespace/internal
             ":"
             +whitespace/all
             (esrap:? coalton)
             (or (and "end"
                      (moonli/macro-predicates::|COALTON:DEFINE| expr:symbol))
                 "end")))))
  `(coalton:define
       ,@(optima:ematch fn-or-variable-forms
           ((list s _ _ _ expr)
            (list s expr))
           ((list name _ (list* params) _ _ _ body _)
            (list* (list* name params) (rest body))))))

(def-test coalton:define (macro-call)
  (:lisp (coalton:coalton-toplevel
           (coalton:define x 5))
   :moonli "coalton:coalton-toplevel :
  coalton:define x = 5;
end")
  (:lisp (coalton:coalton-toplevel
           (coalton:define (add x y)
             (+ x y)))
   :moonli "coalton:coalton-toplevel :
  coalton:define add(x, y):
    x + y
  end
end")
  (:lisp (coalton:coalton-toplevel
           (coalton:define x 5)
           (coalton:define (add x y)
             (+ x y)))
   :moonli "coalton:coalton-toplevel :

  coalton:define x = 5;

  coalton:define add(x, y):
    x + y
  end
end")
  (:lisp (coalton:coalton-toplevel
           (coalton:define (make-fma a)
             (coalton:fn (b)
               (coalton:fn (c)
                 (+ c (* a b)))))

           (coalton:define fma1 (make-fma 2))
           (coalton:define fma2 ((make-fma 2) 3))
           (coalton:define fma3 (((make-fma 2) 3) 4)))
  :moonli "coalton:coalton-toplevel :

  coalton:define make-fma(a):
    coalton:fn (b):
      coalton:fn (c):
        c + a * b
      end
    end
  end

  coalton:define fma1 = make-fma(2);

  coalton:define fma2 = make-fma(2)(3);

  coalton:define fma3 = make-fma(2)(3)(4);

end"))
