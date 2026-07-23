(in-package :moonli)

(defvar *moonli-parse-string*)

(defvar *transpilation-line-number*)
(defvar *transpilation-character-offset*)

(defvar *transpilation-definition-source-form-table*)

(defvar *syntax-positions*)
(declaim (type syntax-positions *syntax-positions*))
(defstruct (syntax-positions (:conc-name syntax-))
  (keywords)
  (controls)
  (strings)
  (comments)
  (numbers)
  (types)
  (constants))

(defmacro with-syntax (&body body)
  `(let ((*syntax-positions* (make-syntax-positions)))
     ,@body))

(defun mark-syntax (kind start end)
  (when (boundp '*syntax-positions*)
    (let* ((retrieval-function (let ((*package* (find-package :moonli)))
                                 (alexandria:symbolicate 'syntax- kind 's)))
           (setter-function (fdefinition `(setf ,retrieval-function))))
      (funcall setter-function
               (cons (cons start end)
                     (funcall retrieval-function *syntax-positions*))
               *syntax-positions*))))

(defun syntax-positions (kind)
  (let* ((retrieval-function (let ((*package* (find-package :moonli)))
                                 (alexandria:symbolicate 'syntax- kind 's))))
    (funcall retrieval-function *syntax-positions*)))

(defun position-to-line-column (text position)
  (let ((line (count #\newline text :end position))
        (col  (- position
                 (or (position #\newline text :end position :from-end t)
                     0))))
    (list line col)))

(defun format-moonli-parse-error (string position)
  ;; Find the distance of POSITION from the preceding and next newline
  (let* ((pn (position #\newline string :end position :from-end t))
         (nn (position #\newline string :start position))
         ;; START-DIST: Distance from preceding newline
         (start-dist (if pn (- position pn) position))
         ;; END-DIST: DIstance to next newline
         (end-dist (if nn (- nn position) (- (length string) position))))
    (with-output-to-string (*standard-output*)
      (write-string (subseq string 0 nn))
      (write-char #\newline)
      (loop :repeat start-dist :do (write-char #\-))
      (write-char #\^)
      (loop :repeat (- end-dist 1) :do (write-char #\-))
      (when (member (char string position) '(#\( #\{ #\[) :test #'char=)
        (format t "~%Perhaps an unclosed bracket?~%"))
      (when nn (write-string (subseq string nn))))))

(define-condition moonli-parse-error (parse-error)
  ((position :initarg :position
             :reader parse-error-position)
   (expectation :initarg :expectation
                :initform nil
                :reader parse-error-expectations)
   (may-be-errors :initarg :may-be-errors
                  :initform ()))
  (:report (lambda (c s)
             (with-slots (position expectation may-be-errors) c
               (if (boundp '*moonli-parse-string*)
                   (destructuring-bind (line col)
                       (position-to-line-column *moonli-parse-string* position)
                     (format s "Parse error at line ~a, column ~a. " line col)
                     (if expectation
                         (format s "Expected ~a" expectation)
                         (format s "Unexpected token"))
                     (terpri s)
                     (terpri s)
                     (write-string
                      (format-moonli-parse-error *moonli-parse-string* position)
                      s))
                   (progn
                     (format s "Parse error at position ~a. " position)
                     (if expectation
                         (format s "Expected ~a" expectation)
                         (format s "Unexpected token."))))
               (terpri s)
               (format s "~%~{~A~^~%~}" may-be-errors)
               (terpri s)))))

(define-condition moonli-may-be-parse-error (condition) ())

(defstruct (comment (:constructor make-comment (depth string)))
  (depth 0)
  (string ""))

(defmethod print-object ((c comment) s)
  (format s "~&")
  (dotimes (i (comment-depth c))
    (write-char #\; s))
  (format s "~a~&" (comment-string c)))

(esrap:defrule comment
    (and (+ #\#)
         (* (not (or #\newline #\return)))
         (or #\newline #\return))
  (:error-report nil)
  (:function (lambda (expr)
               (when expr
                 (make-comment (length (first expr))
                               (esrap:text (second expr)))))))

(esrap:defrule whitespace/internal
    (or #\Space #\Tab)
  (:constant nil)
  (:error-report nil))

(esrap:defrule *whitespace/internal
    (* whitespace/internal)
  (:constant nil)
  (:error-report nil))

(esrap:defrule +whitespace/internal
    (+ whitespace/internal)
  (:constant nil)
  (:error-report nil))

(esrap:defrule whitespace/newline
    (or #\newline #\return)
  (:constant nil)
  (:error-report nil)
  (:around ()
    (esrap:call-transform)
    (when (boundp '*transpilation-line-number*)
      (incf *transpilation-line-number*))))

(esrap:defrule whitespace/end
    (or comment whitespace/newline #\;)
  (:constant nil)
  (:error-report nil))

(esrap:defrule whitespace
    (or comment #\space #\tab whitespace/newline)
  (:constant nil)
  (:error-report nil))

(esrap:defrule +whitespace
    (+ whitespace)
  (:constant nil)
  (:error-report nil))

(esrap:defrule *whitespace
    (* whitespace)
  (:constant nil)
  (:error-report nil))

(esrap:defrule *whitespace/end
    (* whitespace/end)
  (:constant nil)
  (:error-report nil))

(esrap:defrule *whitespace/all
    (* (or comment whitespace/end whitespace/internal))
  (:error-report nil))

(esrap:defrule +whitespace/all
    (+ (or comment whitespace/end whitespace/internal))
  (:error-report nil))

(esrap:defrule numeric-character (esrap:character-ranges (#\0 #\9))
  (:error-report :context))

(esrap:defrule non-symbol-chars
    (or #\| #\: #\, #\$ #\' #\" #\( #\[ #\{ #\) #\] #\}
        whitespace/internal whitespace/end)
  (:error-report nil))
