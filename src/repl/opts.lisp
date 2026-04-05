(in-package :moonli/repl)

(opts:define-opts
  (:name :help
   :description "Print this help and exit."
   :short #\h
   :long "help")
  (:name :version
   :description "Show the version info and exit."
   :short #\v
   :long "version")
  (:name :no-init
   :description "Skip to load init file."
   :short #\n
   :long "no-init")
  (:name :history-file
   :description "Specifies which history file to use. If unspecified, this is the .moonli-repl file in $HOME directory."
   :long "history-file"
   :arg-parser #'identity)
  (:name :load
   :description "Load a file"
   :short #\l
   :long "load"
   :arg-parser #'identity)
  (:name :transpile
   :description "Transpile moonli file"
   :short #\t
   :long "transpile"
   :arg-parser #'identity)
  (:name :eval
   :description "Eval a form"
   :short #\e
   :long "eval"
   :arg-parser #'identity)
  (:name :enable-debugger
   :description "Enable debugger: print error and drop into the debugger"
   :short #\d
   :long "enable-debugger")
  (:name :silent
   :description "Don't print logo and program information"
   :short #\s
   :long "silent"))

(defgeneric process-option (option argument))

(defmethod process-option ((option (eql :help)) arg)
  (declare (ignore option arg))
  (opts:describe
   :prefix "A full-featured Moonli REPL")
  (uiop:quit 0))

(defvar *debugger-enabled-p* nil)

(defmethod process-option ((option (eql :enable-debugger)) arg)
  (declare (ignore option arg))
  (setq *debugger-enabled-p* t))

(defmethod process-option ((option (eql :version)) arg)
  (declare (ignore option arg))
  (format t "moonli-repl v~a~&" *versions*)
  (uiop:quit 0))

(defvar *site-init* t)
(defvar *site-init-path*)

(defmethod process-option ((option (eql :no-init)) arg)
  (declare (ignore option arg))
  (setf *site-init* nil))

(defvar *silent* nil)

(defmethod process-option ((option (eql :silent)) arg)
  (declare (ignore option arg))
  (setf *silent* t))

(defmethod process-option ((option (eql :history-file)) arg)
  (declare (ignore option))
  (setf ic-repl:*history-file* (uiop:native-namestring arg)))

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
