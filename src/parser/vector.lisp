(in-package :moonli)

(5am:in-suite :moonli)

;; We probably won't emit literal vectors from the vector syntax.  This is
;; mainly because the use case of vectors within json needs evaluation. Further,
;; most lisp syntax does not rely on literal vectors, the way it relies on
;; literal lists and cons.

(esrap:defrule expr:vector
    (or (and #\[ *whitespace #\])
        (and #\[
             *whitespace
             moonli-expression
             *whitespace
             (* (and mandatory-comma
                     *whitespace
                     moonli-expression
                     *whitespace))
             #\])
        (and #\[
             (+ (and *whitespace
                     moonli-expression
                     *whitespace
                     mandatory-comma
                     *whitespace))
             #\]))
  (:function (lambda (expr)
               (cons 'vector
                     (if (null (cdddr expr)) ; length = 3, first or last
                         (mapcar (lambda (elt)
                                   (optima:ematch elt
                                     ((list _ expr _ _ _)
                                      expr)))
                                 (second expr))
                         (cons (third expr) ; middle
                               (mapcar (lambda (elt)
                                         (optima:ematch elt
                                           ((list _ _ expr _)
                                            expr)))
                                       (fifth expr))))))))

(def-test cl:vector (expr:vector)
  (:lisp (vector a (fn a b c) (+ (add a b) c))
   :moonli "[a, fn(a,b,c), add(a,b) + c]")
  (:lisp (vector (+ (add a b) c))
   :moonli "[add(a,b) + c]"))
