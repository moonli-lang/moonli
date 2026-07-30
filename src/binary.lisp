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
   :arg-parser (lambda (x)
                 (let ((*read-eval* nil))
                   (read-from-string x)))))


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
        (lambda () arg)))

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

        (when free-args
          ;; If it was a funcall, pass rest of the arguments to it.
          (cond ((getf options :funcall)
                 (write (eval `(,(getf options :funcall)
                                ,@(mapcar (lambda (arg)
                                            (handler-case (esrap:parse 'number arg)
                                              (esrap:esrap-parse-error () arg)))
                                          free-args))))
                 (terpri))
                (t
                 ;; Otherwise process scripts
                 (dolist (file-name free-args)
                   (funcall (cdr (process-option :load file-name))))))
          (uiop:quit 0))))

    (loop :initially (write-string "* ")
                     (force-output)
          :for result := (eval (read-moonli-from-stream *standard-input* nil))
          :do (format t "~S~%* " result)
              (force-output))))
