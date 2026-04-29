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
   :arg-parser #'identity))


(defgeneric process-option (option argument))

(defmethod process-option ((option (eql :help)) arg)
  (declare (ignore option arg))
  (opts:describe :prefix "A very basic Moonli REPL"
                 :usage-of "moonli"
                 :args "script-1 script-2 ...")
  (uiop:quit 0))

(defmethod process-option ((option (eql :version)) arg)
  (declare (ignore option arg))
  (format t "v~a~&" (asdf:component-version (asdf:find-system "moonli")))
  (uiop:quit 0))

(defmethod process-option ((option (eql :eval)) arg)
  (declare (ignore option))
  (eval (moonli:read-moonli-from-string arg)))

(defmethod process-option ((option (eql :load)) arg)
  (declare (ignore option))
  (cond ((member (pathname-type arg)
                 '("lisp" "lsp")
                 :test #'string-equal)
         (load arg))
        ((string-equal "moonli" (pathname-type arg))
         (moonli:load-moonli-file arg :transpile nil))))

(defmethod process-option ((option (eql :transpile)) arg)
  (declare (ignore option))
  (moonli:transpile-moonli-file arg))

(defun main (&optional (argv nil argvp))
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
    (alexandria:doplist (key arg options)
      (process-option key arg))
    (handler-bind ((error
                     (lambda (c)
                       (format *error-output* "~A" c)
                       (uiop:print-backtrace
                        :condition c :stream *error-output*)
                       (uiop:quit 1))))
      (dolist (file-name free-args)
        (process-option :load file-name)
        (uiop:quit 0))))
  (let ((*package* (find-package :moonli-user)))
    (loop :initially (write-string "* ")
                     (force-output)
          :for result := (eval (read-moonli-from-stream *standard-input* nil))
          :do (format t "~S~%* " result)
              (force-output))))
