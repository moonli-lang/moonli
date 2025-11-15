(in-package :moonli)

(defpackage #:moonli/macro-predicates
  (:use))

(defvar *moonli-macro-functions* (make-hash-table))
(defun expand-moonli-macro (expression)
  (funcall (car (gethash (first expression) *moonli-macro-functions*))
           (rest expression)))

(defvar *moonli-short-macro-functions* (make-hash-table))
(defun expand-moonli-short-macro (expression)
  (funcall (car (gethash (first expression) *moonli-short-macro-functions*))
           (rest expression)))

(defun namep-symbol (name)
  (intern (format nil "~A::~A"
                  (package-name (symbol-package name))
                  (symbol-name name))
          '#:moonli/macro-predicates))



(defmacro define-moonli-macro (name &body (moonli-macro-bindings . body))
  (alexandria:with-gensyms (expr subexpr args fn idx symbol)
    (let* ((namep (namep-symbol name))
           (macro-rule `(and (,namep expr:symbol)
                             +whitespace
                             ,@(mapcar #'second moonli-macro-bindings)
                             *whitespace "end"
                             (esrap:? +whitespace/internal)
                             (esrap:? (,namep expr:symbol)))))
      `(progn
         (defun ,namep (,symbol)
           (eq ,symbol ',name))
         (destructuring-bind (&optional ,fn &rest ,idx)
             (gethash ',name *moonli-macro-functions*)
           (let* ((,expr (esrap:rule-expression
                          (esrap:find-rule 'macro-call)))
                  (,subexpr ,expr)
                  (,idx (or ,idx (1- (length ,subexpr)))))
             (if ,fn
                 (setf (nth ,idx (cdr ,subexpr))
                       ',macro-rule)
                 (setf (cdr ,subexpr)
                       (nconc (cdr ,subexpr)
                              (list ',macro-rule))))
             (esrap:change-rule 'macro-call ,expr)
             (setf (gethash ',name *moonli-macro-functions*)
                   (cons (lambda (,args)
                           (optima:ematch (rest ,args)
                             ((list ,@(mapcar #'first moonli-macro-bindings)
                                    _ "end" _ (or nil ',name))
                              ,@body)))
                         ,idx))))))))

(defmacro define-moonli-short-macro
    (name &body (moonli-macro-bindings . body))
  (alexandria:with-gensyms (expr subexpr args fn idx symbol)
    (let* ((namep (namep-symbol name))
           (macro-rule `(and (,namep expr:symbol)
                             +whitespace/internal
                             ,@(mapcar #'second moonli-macro-bindings))))
      `(progn
         (defun ,namep (,symbol) (eq ,symbol ',name))
         (destructuring-bind (&optional ,fn &rest ,idx)
             (gethash ',name *moonli-short-macro-functions*)
           (let* ((,expr (esrap:rule-expression
                          (esrap:find-rule 'short-macro-call)))
                  (,subexpr ,expr)
                  (,idx (or ,idx (1- (length ,subexpr)))))
             (if ,fn
                 (setf (nth ,idx (cdr ,subexpr))
                       ',macro-rule)
                 (setf (cdr ,subexpr)
                       (nconc (cdr ,subexpr)
                              (list ',macro-rule))))
             (esrap:change-rule 'short-macro-call ,expr)
             (setf (gethash ',name *moonli-short-macro-functions*)
                   (cons (lambda (,args)
                           (optima:ematch (rest ,args)
                             ((list ,@(mapcar #'first moonli-macro-bindings))
                              ,@body)))
                         ,idx))))))))

;; (setf *moonli-macro-functions* (make-hash-table))
(when (zerop (hash-table-count *moonli-macro-functions*))
  (esrap:add-rule 'macro-call
                  (make-instance
                   'esrap:rule
                   :expression
                   (copy-tree `(or) ;; This will be filled by the macro
                                    )
                   :transform
                   (lambda (production start end)
                     (declare (ignorable start end))
                     ((lambda (expr)
                        ;; (print expr)
                        (expand-moonli-macro expr))
                      production))))
  (esrap:add-rule 'short-macro-call
                  (make-instance
                   'esrap:rule
                   :expression (copy-tree
                                `(or))
                   :transform
                   (lambda (production start end)
                     (declare (ignorable start end))
                     ((lambda (expr)
                        (expand-moonli-short-macro expr))
                      production)))))

