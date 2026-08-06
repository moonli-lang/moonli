(in-package :moonli/repl)

;;; Generated using https://patorjk.com/software/taag/
;;; Font: Doom, Text: "Moonli REPL"
;;; Replace \ with \\
(defvar *logo*
"___  ___                      _  _  ______  _____ ______  _
|  \\/  |                     | |(_) | ___ \\|  ___|| ___ \\| |
| .  . |  ___    ___   _ __  | | _  | |_/ /| |__  | |_/ /| |
| |\\/| | / _ \\  / _ \\ | '_ \\ | || | |    / |  __| |  __/ | |
| |  | || (_) || (_) || | | || || | | |\\ \\ | |___ | |    | |____
\\_|  |_/ \\___/  \\___/ |_| |_||_||_| \\_| \\_|\\____/ \\_|    \\_____/")

(defvar *logo-color* "#ef2929")

(defvar *copy* "Moonli (C) 2025 Shubhamkar Ayare (https://github.com/moonli-lang/moonli)")
(defvar *maintain* (format nil "Isocline (C) 2021 Daan Leijen (https://github.com/daanx/isocline)"))

(defvar *versions*
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

(cffi:defcallback highlighter :void
    ((henv (:pointer (:struct ic:highlight-env)))
     (input :string)
     (arg :pointer))
  (declare (optimize (speed 1) safety debug)
           (ignore arg))
  (moonli:with-syntax
    (let ((moonli:*read-without-interning* t))
      (moonli:read-moonli-from-string input t))
    (loop :for (kind . hl-class) :in '((keyword . "keyword")
                                       (control . "control")
                                       (string . "string")
                                       (comment . "comment")
                                       (number . "number")
                                       (type . "type")
                                       (constant . "constant"))
          :do (loop :for (start . end) :in (moonli:syntax-positions kind)
                    :do (ic:highlight henv start (- end start) hl-class)))))

(defun main (&optional (argv (opts:argv) argvp))
  (let* ((ic-repl:*read-function*
           (lambda (stream)
             (moonli:read-moonli-from-string
              (with-output-to-string (out)
                (loop :while (listen stream)
                      :do (write-char (read-char stream) out))))))
         (*print-pprint-dispatch* moonli::*moonli-pprint-dispatch*)
         (*print-case* :downcase)
         (*print-pretty* t)
         (*print-length* 10)
         (ic-repl:*debugger-enabled-p* nil)
         (*debugger-hook* 'ic-repl:debugger)
         (ic-repl:*output-marker* "#=>"))

    (multiple-value-bind (options free-args)

        (handler-case
            (if argvp (opts:get-opts argv) (opts:get-opts))
          (error (e)
            (format uiop:*stderr* "~a: ~a"
                    (class-name (class-of e))
                    e)
            (uiop:print-backtrace :stream uiop:*stderr* :condition e)
            (format t "try `moonli-repl --help`.~&")
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

          ;; Process options with positive priorities
          (dolist (processor processors)
            (when (< 0 (car processor))
              (funcall (cdr processor))))

          (unless (boundp 'ic-repl:*history-file*)
            (setf ic-repl:*history-file*
                  (uiop:native-namestring
                   (merge-pathnames ".moonli-repl" (user-homedir-pathname)))))

          (setf *site-init-path*
                (uiop:native-namestring
                 (merge-pathnames ".moonlirc" (user-homedir-pathname))))
          (when (and *site-init* (probe-file *site-init-path*))
            (moonli:load-moonli-file *site-init-path* :transpile nil))

          ;; Finally process options with non-positive priorities
          (dolist (processor processors)
            (when (>= 0 (car processor))
              (funcall (cdr processor)))))

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
                 (dolist (script-file free-args)
                   (funcall (cdr (process-option :load script-file))))))
          (uiop:quit 0))))

    (unless *silent*
      (ic:println (format nil "[color=~a]~a[/color]" *logo-color* *logo*))
      (format t "~a~%~a~%~a~%~%" *versions* *copy* *maintain*)
      (ic:term-italic t)
      (ic:println "  Press F1 to see available keybindings.")
      (ic:term-italic nil)
      (terpri))

    (asdf:initialize-source-registry (list :source-registry
                                           (list :directory (uiop:getcwd))
                                           :inherit-configuration))
    (setf ql-setup:*quicklisp-home*
          (make-pathname :defaults "~/quicklisp/"))
    (ic:set-default-completer (cffi:callback completer) (cffi:null-pointer))
    (ic:set-default-highlighter (cffi:callback highlighter) (cffi:null-pointer))
    (ic:set-prompt-marker "> " "")
    (ic:enable-multiline-indent nil)
    (ic-repl:repl)))
