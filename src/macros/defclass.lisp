(in-package :moonli)

(5am:in-suite :moonli)

(esrap:defrule defclass/slot/attribute
    (and expr:symbol
         *whitespace
         ":"
         *whitespace
         moonli-expression)
  (:function (lambda (expr)
               `(,(intern (symbol-name (first expr)) :keyword)
                 ,(fifth expr)))))

(esrap:defrule defclass/slot
    (and expr:symbol
         *whitespace/internal
         (or ";"
             (and ":"
                  *whitespace/all
                  defclass/slot/attribute
                  *whitespace/internal
                  (* (and ","
                          *whitespace/all
                          defclass/slot/attribute
                          *whitespace/internal))
                  ";")))
  (:function (lambda (expr)
               `(,(first expr)
                 ,@(if (and (stringp (third expr))
                            (string= ";" (third expr)))
                       ()
                       (nconc (third (third expr))
                              (alexandria:mappend #'third (fifth (third expr)))))))))

(esrap:defrule defclass/slots
    (and "slots"
         (and *whitespace/internal
              ":"
              *whitespace/all
              (* (and defclass/slot *whitespace/all))
              "end"))
  (:function (lambda (expr)
               (cons :slots
                     (list (mapcar #'first (fourth (second expr))))))))

(esrap:defrule defclass/option
    (and good-symbol
         *whitespace
         ":"
         *whitespace
         moonli-expression)
  (:function (lambda (expr)
               `(,(intern (symbol-name (first expr)) :keyword)
                 ,(fifth expr)))))

(esrap:defrule defclass/options
    (and defclass/option
         (* (and *whitespace/internal
                 ","
                 *whitespace/all
                 defclass/option))
         ";")
  (:function (lambda (expr)
               (cons :options
                     (cons (first expr)
                           (mapcar #'fourth (second expr)))))))

(esrap:defrule defclass/slots-and-options
    (or (and defclass/options)
        (and defclass/slots
             *whitespace
             defclass/options)
        (and defclass/slots))
  (:function (lambda (expr)
               (cond ((= 3 (length expr))
                      (nconc (rest (first expr))
                             (rest (third expr))))
                     ((eq :slots (first (first expr)))
                      (rest (first expr)))
                     ((eq :options (first (first expr)))
                      (cons () (rest (first expr))))))))

(define-moonli-macro defclass
  ((name good-symbol)
   (direct-superclasses expr:list)
   (_ (and *whitespace/internal ":" *whitespace/all))
   (slots-and-options (esrap:? defclass/slots-and-options))
   (_ *whitespace/all))
  `(defclass ,name ,(rest direct-superclasses)
     ,@(if (null slots-and-options)
           `(())
           slots-and-options)))

(def-test defclass (macro-call)
  (:moonli "defclass point():
end"
   :lisp (defclass point () ()))
  (:moonli "defclass point():
  metaclass: standard-class;
end"
   :lisp (defclass point ()
           ()
           (:metaclass standard-class)))
  (:moonli "defclass point():
  metaclass: standard-class,
  documentation: \"A class for Points!\";
end"
   :lisp (defclass point ()
           ()
           (:metaclass standard-class)
           (:documentation "A class for Points!")))
  (:moonli "defclass point():
  slots:
  end
end"
   :lisp (defclass point () ()))
  (:moonli "defclass point():
  slots:
    x:
      initform: 2.0,
      type: single-float,
      accessor: point-x;
  end
end"
   :lisp (defclass point ()
           ((x :initform 2.0 :type single-float :accessor point-x))))
  (:moonli "defclass point():
  slots:
    x:
      initform: 2.0,
      type: single-float,
      accessor: point-x;
    y:
      initform: 2.0,
      type: single-float,
      accessor: point-y;
  end
end"
   :lisp (defclass point ()
           ((x :initform 2.0 :type single-float :accessor point-x)
            (y :initform 2.0 :type single-float :accessor point-y))))
  (:moonli "defclass point():
  slots:
    x:
      initform: 2.0,
      type: single-float,
      accessor: point-x;
    y:
      initform: 2.0,
      type: single-float,
      accessor: point-y;
  end

  metaclass: standard-class,

  documentation: \"Two dimensional points.\";

end"
   :lisp (defclass point ()
           ((x :initform 2.0 :type single-float :accessor point-x)
            (y :initform 2.0 :type single-float :accessor point-y))
           (:metaclass standard-class)
           (:documentation "Two dimensional points."))))
