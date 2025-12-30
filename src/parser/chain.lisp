(in-package :moonli)

(5am:in-suite :moonli)

(esrap:defrule arglist
    expr:function-arglist
  (:function rest))

(esrap:defrule expr:function-arglist
    (or (and #\( *whitespace moonli-expression *whitespace #\))
        expr:list)
  (:function (lambda (expr)
               (optima:ematch expr
                 ((list "(" nil expr nil ")")
                  (list 'list expr))
                 ((list* 'list _)
                  expr)))))

(esrap:defrule chain
    (and atomic-expression
         (* (or expr:function-arglist
                expr:vector)))
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
                                          (list* 'funcall
                                                 first
                                                 (rest list/vector)))
                                      rest-rest))
                               (vector
                                (list
                                 `(,(cond ((find-package "PELTADOT-TRAITS-LIBRARY")
                                           (find-symbol "AT" "PELTADOT-TRAITS-LIBRARY"))
                                          ((find-package "ACCESS")
                                           (find-symbol "ACCESS" "ACCESS"))
                                          (t
                                           'cl:aref))
                                   ,first
                                   ,@(rest list/vector))
                                 rest-rest))))))))
                 (build-prefix expr)))))

(def-test chain (chain)
  (:lisp (aref x i j)
   :moonli "x[i,j]")
  (:lisp (aref (aref s i) j)
   :moonli "s[i][j]")
  (:lisp (aref (aref (make-array (list 2 3)) i) j)
   :moonli "make-array((2,3))[i][j]")
  (:lisp a :moonli "a")
  (:lisp (a) :moonli "a()")
  (:lisp (funcall (aref (a) i j))
   :moonli "a()[i,j]()")
  (:lisp (aref a i j)
   :moonli "a[i,j]")
  (:lisp (aref (vector 1 2 3) 0)
   :moonli "[1,2,3][0]"))


