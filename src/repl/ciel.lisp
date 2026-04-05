(in-package :moonli/repl)

;;; Generated using https://patorjk.com/software/taag/
;;; Font: Doom, Text: "(   Moonli   .   CIEL   )"
;;; Replace \ with \\
(setf *logo*
      "
  __   ___  ___                  _ _           _____ _____ _____ _        __
 / /   |  \\/  |                 | (_)         /  __ \\_   _|  ___| |       \\ \\
| |    | .  . | ___   ___  _ __ | |_          | /  \\/ | | | |__ | |        | |
| |    | |\\/| |/ _ \\ / _ \\| '_ \\| | |         | |     | | |  __|| |        | |
| |    | |  | | (_) | (_) | | | | | |    _    | \\__/\\_| |_| |___| |____    | |
| |    \\_|  |_/\\___/ \\___/|_| |_|_|_|   (_)    \\____/\\___/\\____/\\_____/    | |
 \\_\\                                                                      /_/

")

(setf *maintain* (format nil "Isocline (C) 2021 Daan Leijen (https://github.com/daanx/isocline)"))

(setf *versions*
      (format nil "moonli-repl ~a on ~?~a ~a, CIEL ~a"
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
              (lisp-implementation-version)
              (asdf:component-version (asdf:find-system "ciel"))))
