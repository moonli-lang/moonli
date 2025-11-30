(in-package :cl-repl)

(setf *logo*
  "
___  ___                      _  _  ______  _____ ______  _
|  \\/  |                     | |(_) | ___ \\|  ___|| ___ \\| |
| .  . |  ___    ___   _ __  | | _  | |_/ /| |__  | |_/ /| |
| |\\/| | / _ \\  / _ \\ | '_ \\ | || | |    / |  __| |  __/ | |
| |  | || (_) || (_) || | | || || | | |\\ \\ | |___ | |    | |____
\\_|  |_/ \\___/  \\___/ |_| |_||_||_| \\_| \\_|\\____/ \\_|    \\_____/
")

(setf *copy* "Moonli (C) 2025 Shubhamkar Ayare")
(setf *maintain* (format nil "CL-REPL (C) 2017-2018 TANI Kojiro~%  (Maintained by https://github.com/lisp-maintainers/cl-repl)"))

(setf *read-function* 'moonli:read-moonli-from-string)
(setf *line-continue-function*
      (lambda (string)
        (let* ((ends-with-newlines
                 (alexandria:ends-with-subseq (format nil "~%") string))
               (expr-errors-p
                 ;; The resulting expression produces an error
                 (nth-value 1
                            (ignore-errors
                             (moonli:read-moonli-from-string string)))))
          (if ends-with-newlines
              ;; Don't continue if there are no newlines at end
              nil
              expr-errors-p))))

(setf *versions*
      (format nil "moonli-repl ~a on ~?~a ~a"
              (asdf:component-version (asdf:find-system "moonli"))
              #+ros.script
              "Roswell ~a, "
              #-ros.script
              ""
              #+ros.script
              `(,(ros::version))
              #-ros.script
              nil
              (lisp-implementation-type)
              (lisp-implementation-version)))

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
   :description "Specifies which history file to use. If unspecified, this is the .cl-repl file in $HOME directory."
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
   :long "enable-debugger"))

(defun main (&optional (argv nil argvp) &key (show-logo t))
  (main-prep)
  (let ((*debugger-enabled-p* nil)
        (*print-case* :downcase))
    (multiple-value-bind (options free-args)
        (handler-case
            (if argvp (opts:get-opts argv) (opts:get-opts))
          (error (e)
            (format uiop:*stderr* "~a: ~a"
                    (class-name (class-of e))
                    e)
            (uiop:print-backtrace :stream uiop:*stderr* :condition e)
            (format t "try `cl-repl --help`.~&")
            (uiop:quit 1)))
      (declare (ignore free-args))
      (when-option (options :help)
        (opts:describe
         :prefix "A full-featured Common Lisp REPL implementation.")
        (uiop:quit 0))
      (when-option (options :enable-debugger)
        (setq *debugger-enabled-p* t))
      (when-option (options :version)
        (format t "cl-repl v~a~&" +version+)
        (uiop:quit 0))
      (when-option (options :no-init)
        (setf *site-init-path* nil))
      (setf *history-filename*
            (or (getf options :history-file)
                (format nil "~a/.moonli-repl" (uiop:getenv "HOME"))))
      (when *site-init-path*
        (site-init))
      (setf *history* (load-history))
      (loop for (k v) on options by #'cddr
            do (case k
                 (:eval (eval (moonli:read-moonli-from-string v)))
                 (:load (cond ((member (pathname-type v)
                                       '("lisp" "lsp")
                                       :test #'string-equal)
                               (load v))
                              ((string-equal "moonli" (pathname-type v))
                               (moonli:load-moonli-file v :transpile nil))))
                 (:transpile (moonli:transpile-moonli-file v)))))
    (when *repl-flush-screen* (flush-screen))
    (with-cursor-hidden
      (when show-logo
        (format t (color *logo-color* *logo* :prompt-chars nil)))
      (format t "~a~%~a~%~a~2%" *versions* *copy* *maintain*))
    (unwind-protect
         (let* ((*package* (find-package :moonli-user))
                (*debugger-hook* (if *debugger-enabled-p*
                                     #'debugger
                                     #'display-error-without-debugging))
                (*print-pretty* t)
                (*print-pprint-dispatch* moonli::*moonli-pprint-dispatch*))
           (repl))
      (save-history)
      (rl:deprep-terminal))
    (when *repl-flush-screen* (flush-screen))))
