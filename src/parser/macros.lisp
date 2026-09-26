(in-package :moonli)

(defpackage #:moonli/macro-predicates
  (:use))


(defun namep-symbol (name)
  (intern (format nil "~A::~A"
                  (package-name (symbol-package name))
                  (symbol-name name))
          '#:moonli/macro-predicates))



(defmacro define-moonli-macro (name &body (moonli-macro-bindings . body))
  (alexandria:with-gensyms (expr subexpr args oidx idx symbol end-sym start-pos end-pos)
    (let* ((namep (namep-symbol name))
           (macro-rule `(and (,namep expr:symbol)
                             +whitespace
                             ,@(mapcar #'second moonli-macro-bindings)
                             *whitespace "end"
                             (esrap:? +whitespace/internal)
                             (esrap:? expr:symbol))))
      `(progn
         (defun ,namep (,symbol)
           (eq ,symbol ',name))
         (esrap:defrule ,name
             ,macro-rule
           (:lambda (,args esrap:&bounds ,start-pos ,end-pos)
             (declare (optimize debug)
                      (ignore ,start-pos))
             (optima:ematch (cddr ,args)
               ((list ,@(mapcar #'first moonli-macro-bindings)
                      _ "end" _ ,end-sym)
                (unless (or (null ,end-sym)
                            (eq ',name ,end-sym))
                  (error 'moonli-parse-error
                         :position (- ,end-pos
                                      (length (string ,end-sym)))
                         :expectation (format nil "'~S'" ',name)))
                ,@body)))
           (:error-report t))
         (let* ((,oidx (gethash ',name *moonli-macro-functions*))
                (,expr (esrap:rule-expression
                        (esrap:find-rule 'macro-call)))
                (,subexpr ,expr)
                (,idx (or ,oidx (1- (length ,subexpr)))))
           (if ,oidx
               (setf (nth ,oidx (cdr ,subexpr))
                     ',name)
               (setf (cdr ,subexpr)
                     (nconc (cdr ,subexpr)
                            (list ',name))))
           (esrap:change-rule 'macro-call ,expr)
           (setf (gethash ',name *moonli-macro-functions*)
                 ,idx))))))

(defmacro define-moonli-short-macro
    (name &body (moonli-macro-bindings . body))
  (alexandria:with-gensyms (expr subexpr args oidx idx symbol)
    (let* ((namep (namep-symbol name))
           (macro-rule `(and (,namep expr:symbol)
                             +whitespace/internal
                             ,@(mapcar #'second moonli-macro-bindings))))
      `(progn
         (defun ,namep (,symbol) (eq ,symbol ',name))
         (esrap:defrule ,name
             ,macro-rule
           (:function (lambda (,args)
                        (optima:ematch (cddr ,args)
                          ((list ,@(mapcar #'first moonli-macro-bindings))
                           ,@body))))
           (:error-report t))
         (let* ((,oidx (gethash ',name *moonli-short-macro-functions*))
                (,expr (esrap:rule-expression
                        (esrap:find-rule 'short-macro-call)))
                (,subexpr ,expr)
                (,idx (or ,oidx (1- (length ,subexpr)))))
           (if ,oidx
               (setf (nth ,oidx (cdr ,subexpr))
                     ',name)
               (setf (cdr ,subexpr)
                     (nconc (cdr ,subexpr)
                            (list ',name))))
           (esrap:change-rule 'short-macro-call ,expr)
           (setf (gethash ',name *moonli-short-macro-functions*)
                 ,idx))))))

;; (setf *moonli-macro-functions* (make-hash-table))
(when (zerop (hash-table-count *moonli-macro-functions*))
  (esrap:add-rule 'macro-call
                  (make-instance
                   'esrap:rule
                   :expression
                   (copy-tree `(or) ;; This will be filled by the macro
                                    )))
  (esrap:add-rule 'short-macro-call
                  (make-instance
                   'esrap:rule
                   :expression (copy-tree
                                `(or)))))

