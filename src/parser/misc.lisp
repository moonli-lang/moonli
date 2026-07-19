(in-package :moonli)

(5am:in-suite :moonli)

(esrap:defrule bracketed-expression
    (and #\( *whitespace moonli-expression *whitespace #\))
  (:function third))

(esrap:defrule expr:character
    (and #\' character #\')
  (:function second))

;; TODO: Generalization for escape chars

(esrap:defrule string
    (and #\"
         (* (or (and #\\ #\")
                (not #\")))
         #\")
  (:function (lambda (expr)
               (with-output-to-string (s)
                 (dolist (elt (second expr))
                   (etypecase elt
                     (character (write-char elt s))
                     (cons
                      (assert (and (null (cddr elt))
                                   (string= "\\" (first elt))))
                      (write-string (second elt) s))))))))

(esrap:defrule string-designator
    (or string expr:symbol))

(esrap:defrule mandatory-comma
    (esrap:? #\,)
  (:lambda (char esrap:&bounds start)
    (if (and (stringp char)
             (string= char ","))
        ","
        (error 'moonli-parse-error :expectation "," :position (1- start)))))

(esrap:defrule expr:cons
    (and #\(
         *whitespace
         moonli-expression
         +whitespace
         #\.
         +whitespace
         moonli-expression
         *whitespace
         #\))
  (:function (lambda (expr)
               `(cons ,(third expr) ,(seventh expr)))))

(5am:def-test expr:cons ()
  (5am:is (equal '(cons a b)
                 (esrap:parse 'moonli-expression "(a . b)")))
  (5am:is (equal '(cons a b)
                 (esrap:parse 'moonli-expression "(a . b )")))
  (5am:is (equal '(cons a b)
                 (esrap:parse 'moonli-expression "( a . b )")))
  (5am:is (equal '(cons a (identity))
                 (esrap:parse 'moonli-expression "(a . identity())")))
  (5am:is (equal '(cons (identity) 42)
                 (esrap:parse 'moonli-expression "(identity() . 42)")))
  (5am:is (equal '(cons (identity) (list 1 2 3))
                 (esrap:parse 'moonli-expression "(identity() . (1, 2, 3))"))))

(esrap:defrule expr:list
    (or (and #\( *whitespace #\))
        (and #\(
             *whitespace
             moonli-expression
             *whitespace
             (+ (and mandatory-comma
                     *whitespace
                     moonli-expression
                     *whitespace))
             #\))
        (and #\(
             (+ (and *whitespace
                     moonli-expression
                     *whitespace
                     mandatory-comma
                     *whitespace))
             #\)))
  (:function (lambda (expr)
               (cons 'list
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

(5am:def-test expr:list ()
  (5am:is (equal '(list)
                 (esrap:parse 'expr:list "()")))
  (5am:is (equal '(list)
                 (esrap:parse 'expr:list "( )")))
  (5am:is (equal '(list)
                 (esrap:parse 'expr:list (format nil "(~%)"))))
  (5am:is (equal '(list 3)
                 (esrap:parse 'expr:list "(3 ,)")))
  (5am:is (equal '(list 3)
                 (esrap:parse 'expr:list (format nil "(~%  3~%,)"))))
  (5am:is (equal '(list 3)
                 (esrap:parse 'expr:list "(3,)")))
  (5am:is (equal '(list 3 :hello)
                 (esrap:parse 'expr:list "(3,:hello)")))
  (5am:is (equal '(list 3 :hello)
                 (esrap:parse 'expr:list "(3, :hello)")))
  (5am:is (equal '(list 3 :hello)
                 (esrap:parse 'expr:list "(3, :hello )")))
  (5am:is (equal '(list 3 :hello)
                 (esrap:parse 'expr:list "(3, :hello, )")))
  (5am:is (equal '(list 3 (null a))
                 (esrap:parse 'expr:list "(3,null(a))"))))
