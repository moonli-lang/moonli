(in-package :moonli)

(defvar *sbcl-home* (sb-int:sbcl-homedir-pathname))

(defun contrib-names ()
  (let* ((contrib-dirs
           (uiop:directory-files
            (merge-pathnames #p"contrib/" *sbcl-home*)))
         (contrib-names
           (alexandria:mappend
            (lambda (dirname)
              (let ((files (uiop:directory-files dirname)))
                (loop :for file :in files
                      :nconcing
                      (let ((name (pathname-name file))
                            (type (pathname-type file)))
                        (when (string= type "asd")
                          (list (intern (string-upcase name)
                                        :keyword)))))))
            (cons (merge-pathnames #p"contrib/" *sbcl-home*)
                  contrib-dirs))))
    (remove-duplicates contrib-names)))

(defvar *contrib-blacklist* ())

(defun require-all-contribs ()
  (dolist (c (contrib-names))
    (unless (member c *contrib-blacklist*)
      (ignore-errors (progn
                       (format t "Requiring ~S~%" c)
                       (force-output)
                       (let ((system (asdf:find-system c)))
                         (when system
                           (asdf:load-system system)
                           (asdf:register-immutable-system system)))
                       (require c))))))
