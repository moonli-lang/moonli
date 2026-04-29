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

(defun main (&optional (argv (opts:argv) argvp))
  (let* ((ic-repl:*read-function*
           (lambda (stream)
             (moonli:read-moonli-from-string
              (with-output-to-string (out)
                (loop :while (listen stream)
                      :do (write-char (read-char stream) out))))))
         (*print-pprint-dispatch* moonli::*moonli-pprint-dispatch*)
         (*print-pretty* t)
         (*print-length* 10)
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
      (alexandria:doplist (key arg options)
        (process-option key arg))
      (setf *site-init-path*
            (uiop:native-namestring
             (merge-pathnames ".moonlirc" (user-homedir-pathname))))
      (when (and *site-init* (probe-file *site-init-path*))
        (moonli:load-moonli-file *site-init-path* :transpile nil))
      (handler-bind ((error
                       (lambda (c)
                         (format *error-output* "~A" c)
                         (uiop:print-backtrace
                          :condition c :stream *error-output*)
                         (uiop:quit 1))))
          (dolist (file-name free-args)
            (process-option :load file-name)
            (uiop:quit 0)))
      (unless (boundp 'ic-repl:*history-file*)
        (setf ic-repl:*history-file*
              (uiop:native-namestring
               (merge-pathnames ".moonli-repl" (user-homedir-pathname))))))
    (unless *silent*
      (ic:println (format nil "[color=~a]~a[/color]" *logo-color* *logo*))
      (format t "~a~%~a~%~a~%~%" *versions* *copy* *maintain*))
    (asdf:initialize-source-registry (list :source-registry
                                           (list :directory (uiop:getcwd))
                                           :inherit-configuration))
    (ic:set-default-completer (cffi:callback completer) (cffi:null-pointer))
    (ic:set-default-highlighter (cffi:callback highlighter) (cffi:null-pointer))
    (ic:set-prompt-marker "> " "")
    (ic:enable-multiline-indent nil)
    (ic-repl:repl)))
