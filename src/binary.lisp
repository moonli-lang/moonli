(uiop:define-package :moonli-user
  (:mix-reexport #:cl #:let-plus))

(in-package :moonli)

(unix-opts:define-opts

  (:name :help
   :description "Print this help and exit."
   :short #\h
   :long "help")

  (:name :version
   :description "Show the version info and exit."
   :short #\v
   :long "version")

  (:name :load
   :description "Load a file"
   :short #\l
   :long "load"
   :arg-parser #'identity)

  (:name :transpile
   :description "Transpile moonli file to lisp file"
   :short #\t
   :long "transpile"
   :arg-parser #'identity)

  (:name :funcall
   :description "Call a function with given arguments. For example, -f uiop:strcat hello world"
   :short #\f
   :long "funcall"
   :arg-parser #'identity)

  (:name :values-separator
   :description "String to use as the separator between multiple values"
   :long "values-separator"
   :arg-parser #'identity))


(defgeneric process-option (option argument))

(defmethod process-option ((option (eql :help)) arg)
  (declare (ignore option arg))
  (cons 100
        (lambda ()
          (opts:describe :prefix "A very basic Moonli REPL"
                         :usage-of "moonli"
                         :args "script-1 script-2 ...")
          (uiop:quit 0))))

(defmethod process-option ((option (eql :version)) arg)
  (declare (ignore option arg))
  (cons 1000
        (lambda ()
          (format t "v~a~&" (asdf:component-version (asdf:find-system "moonli")))
          (uiop:quit 0))))

(defmethod process-option ((option (eql :eval)) arg)
  (declare (ignore option))
  (cons 0
        (lambda ()
          (eval (moonli:read-moonli-from-string arg)))))

(defmethod process-option ((option (eql :load)) arg)
  (declare (ignore option))
  (cons 0
        (lambda ()
          (cond ((member (pathname-type arg)
                         '("lisp" "lsp")
                         :test #'string-equal)
                 (load arg))
                ((string-equal "moonli" (pathname-type arg))
                 (moonli:load-moonli-file arg :transpile nil))))))

(defmethod process-option ((option (eql :transpile)) arg)
  (declare (ignore option))
  (cons 0
        (lambda ()
          (moonli:transpile-moonli-file arg))))

(defmethod process-option ((option (eql :funcall)) arg)
  (declare (ignore option))
  (cons 0
        (lambda ()
          (let ((*read-eval* nil))
            (let ((fn-form (second (read-moonli-from-string arg))))
              (if (and (listp fn-form)
                       (eq 'lm (first fn-form)))
                  (macroexpand-1 fn-form)
                  fn-form))))))

(defvar *values-separator* (string #\newline))
(defmethod process-option ((option (eql :values-separator)) arg)
  (declare (ignore option))
  (cons 90
        (lambda ()
          (setf *values-separator* arg)
          (when (find-package :isocline-repl)
            (setf (symbol-value (find-symbol "*VALUES-SEPARATOR*" :isocline-repl))
                  arg)))))

(esrap:defrule moonsh-atomic-expression
    ;; This is identical to moonli's atomic-expression, but without any symbol,
    ;; string or chain
    (or bracketed-expression
        quoted-expression
        expr:character
        number
        expr:vector
        expr:cons
        expr:list
        expr:hash-table
        expr:hash-set))

(defun main (&optional (argv nil argvp))
  (let ((*package* (find-package :moonli-user))
        (*print-case* :downcase))
    (multiple-value-bind (options free-args)
        (handler-case
            (if argvp (opts:get-opts argv) (opts:get-opts))
          (error (e)
            (format uiop:*stderr* "~a: ~a"
                    (class-name (class-of e))
                    e)
            (uiop:print-backtrace :stream uiop:*stderr* :condition e)
            (format t "try `moonli --help`~&")
            (uiop:quit 1)))
      (handler-bind ((error
                       (lambda (c)
                         (format *error-output* "~A" c)
                         (uiop:print-backtrace
                          :condition c :stream *error-output*)
                         (when free-args (uiop:quit 1)))))

        (let ((processors nil))
          (alexandria:doplist (key arg options)
            (push (process-option key arg) processors))
          (setf processors (stable-sort processors #'> :key #'car))
          (mapcar #'funcall (mapcar #'cdr processors)))

        ;; If it was a funcall, pass rest of the arguments to it.
        (cond ((getf options :funcall)
               (let ((results (multiple-value-list
                               (eval `(,(funcall (cdr (process-option :funcall (getf options :funcall))))
                                       ,@(mapcar (lambda (arg)
                                                   (handler-case
                                                       (esrap:parse 'moonsh-atomic-expression arg)
                                                     (esrap:esrap-parse-error () arg)))
                                                 free-args))))))
                 (loop :for i :from 0
                       :for result :in results
                       :do (unless (zerop i)
                             (write-string *values-separator*))
                           (if (stringp result)
                               (write-string result)
                               (write result))))
               (terpri)
               (uiop:quit 0))
              (free-args
               ;; Otherwise process scripts
               (dolist (file-name free-args)
                 (funcall (cdr (process-option :load file-name))))
               (uiop:quit 0)))))

    (loop :initially (write-string "* ")
                     (force-output)
          :for result := (eval (read-moonli-from-stream *standard-input* nil))
          :do (format t "~S~%* " result)
              (force-output))))
