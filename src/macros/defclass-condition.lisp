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

(esrap:defrule defclass/slot/attributes
    (or ";"
        (and ":"
             *whitespace/all
             defclass/slot/attribute
             *whitespace/internal
             (* (and ","
                     *whitespace/all
                     defclass/slot/attribute
                     *whitespace/internal))
             ";"))
  (:function (lambda (expr)
               (if (and (stringp expr)
                        (string= ";" expr))
                   nil
                   (nconc (third expr)
                          (alexandria:mappend
                           #'third
                           (fifth expr)))))))

(define-condition moonli-defclass-parse-error (moonli-parse-error)
  ((section :initarg :section))
  (:report (lambda (c s)
             (with-slots (section) c
               (format s "Expected '~a' to end" (string-downcase section))))))

(esrap:defrule defclass/slots
    (and "slots"
         *whitespace/internal
         ":"
         *whitespace/all
         expr:symbol
         (* (and *whitespace/internal
                 defclass/slot/attributes
                 *whitespace/all
                 expr:symbol)))
  (:function (lambda (expr)
               (let ((endp (string-equal
                            "end"
                            (if (null (sixth expr))
                                (fifth expr)
                                (fourth (alexandria:lastcar (sixth expr)))))))
                 (unless endp
                   (error 'moonli-defclass-parse-error
                          :section 'slots)))
               (let ((slots
                       (loop :for slot-name
                               := (or next-slot (fifth expr))
                             :for attrs-slots :in (sixth expr)
                             :for attrs := (second attrs-slots)
                             :for next-slot := (fourth attrs-slots)
                             :collect (cons slot-name attrs))))
                 (cons :slots (list slots))))))

(esrap:defrule defclass/options
    (and "options"
         *whitespace/internal
         ":"
         *whitespace/all
         expr:symbol
         (* (and *whitespace/internal
                 ":"
                 *whitespace/internal
                 moonli-expression
                 *whitespace/internal
                 ";"
                 *whitespace/all
                 expr:symbol)))
  (:function (lambda (expr)
               (let ((endp (string-equal
                            "end"
                            (if (null (sixth expr))
                                (fifth expr)
                                (eighth (alexandria:lastcar (sixth expr)))))))
                 (unless endp
                   (error 'moonli-defclass-parse-error
                          :section 'options)))
               (let ((options
                       (loop :for option-name
                               := (or next-option-name (fifth expr))
                             :for value-options :in (sixth expr)
                             :for value := (fourth value-options)
                             :for next-option-name := (eighth value-options)
                             :collect
                             (list (intern (string option-name) :keyword)
                                   value))))
                 (cons :options options)))))

(esrap:defrule defclass/slots-and-options
    (or (and defclass/options
             *whitespace/all
             (esrap:? defclass/slots))
        (and defclass/slots
             *whitespace/all
             (esrap:? defclass/options)))
  (:function (lambda (expr)
               (let ((expr (list (first expr) (third expr))))
                 (cond ((and (first expr)
                             (second expr))
                        (nconc (cdr (assoc :slots expr))
                               (cdr (assoc :options expr))))
                       ((eq :slots (first (first expr)))
                        (rest (first expr)))
                       ((eq :options (first (first expr)))
                        (cons () (rest (first expr)))))))))

(define-moonli-macro defclass
  ((name good-symbol)
   (direct-superclasses expr:function-arglist)
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
  options:
    metaclass: standard-class;
  end
end"
   :lisp (defclass point ()
           ()
           (:metaclass standard-class)))
  (:moonli "defclass point():
  options:
    metaclass: standard-class;
    documentation: \"A class for Points!\";
  end
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
    x;
    y;
  end
end"
   :lisp (defclass point ()
           ((x) (y))))
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
  options:
    metaclass: standard-class;

    documentation: \"Two dimensional points.\";

  end
end"
   :lisp (defclass point ()
           ((x :initform 2.0 :type single-float :accessor point-x)
            (y :initform 2.0 :type single-float :accessor point-y))
           (:metaclass standard-class)
           (:documentation "Two dimensional points."))))

(define-moonli-macro define-condition
  ((name good-symbol)
   (direct-superclasses expr:function-arglist)
   (_ (and *whitespace/internal ":" *whitespace/all))
   (slots-and-options (esrap:? defclass/slots-and-options))
   (_ *whitespace/all))
  `(define-condition ,name ,(rest direct-superclasses)
     ,@(if (null slots-and-options)
           `(())
           slots-and-options)))
