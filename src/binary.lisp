(uiop:define-package :moonli-user
  (:mix-reexport #:cl #:let-plus))

(in-package :moonli)

(unix-opts:define-opts

  (:name :help
   :description "Print this help text"
   :short #\h
   :long "help")

  (:name :load-lisp
   :description "Load lisp file"
   :short #\l
   :long "load-lisp"
   :arg-parser #'cl:load)

  (:name :load-moonli
   :description "Load moonli file"
   :short #\m
   :long "load-moonli"
   :arg-parser (alexandria:compose #'cl:load #'transpile-moonli-file))

  (:name :transpile-moonli
   :description "Transpile moonli file to lisp file"
   :short #\t
   :long "transpile-moonli"
   :arg-parser #'transpile-moonli-file))

(defmacro when-option ((options opt) &body body)
  (let ((it (gensym "IT")))
    `(let ((,it (getf ,options ,opt)))
       (when ,it
         ,@body))))

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
    (declare (ignore free-args))
    (when-option (options :help)
      (opts:describe
       :prefix "A basic moonli transpiler over SBCL")
      (uiop:quit 0))
    (when-option (options :version)
      (format t "moonli v~a~&"
              (slot-value (asdf:find-system "moonli") 'asdf::version))
      (uiop:quit 0)))
  (let ((*package* (find-package :moonli-user)))
    (loop :initially (write-string "* ")
                     (force-output)
          :for result := (eval (read-moonli-from-stream *standard-input*))
          :do (format t "~S~%* " result)
              (force-output))))
