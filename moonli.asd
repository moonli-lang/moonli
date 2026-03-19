(defsystem "moonli"
  :depends-on ("alexandria"
               "esrap"
               "definitions/swank"
               "fiveam"
               "let-plus"
               "optima"
               "parse-number"
               (:feature (:not :swank) "swank")
               "unix-opts")
  :licence "MIT"
  :author "Shubhamkar Ayare (digikar@proton.me)"
  :version "0.0.9"
  :pathname #p"src/"
  :serial t
  :components ((:file "package")
               (:file "testdoc")
               (:module "parser"
                :components ((:file "basic")
                             (:file "mandatory")
                             (:file "number")
                             (:file "symbol")
                             (:file "hash-table-or-set")
                             (:file "macros")
                             (:file "with")
                             (:file "quote")
                             (:file "misc")
                             (:file "infix")
                             (:file "vector")
                             (:file "chain")
                             (:file "expressions")))
               (:module "macros"
                :components ((:file "moonli-macro")
                             (:file "moonli-short-macro")
                             (:file "functions")
                             (:file "defpackage")
                             (:file "defstruct")
                             (:file "defclass-condition")
                             (:file "labels")
                             (:file "let-plus")
                             (:file "match")))
               (:file "moonli")
               (:file "pretty-printer")
               (:file "binary")
               (:file "contribs"))
  :perform (test-op (c s)
             (eval (read-from-string "(5AM:RUN! :MOONLI)")))
  :build-operation "program-op"
  :build-pathname "../moonli"
  :entry-point "moonli:main")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (eval (print
         `(push :sb-aclrepl ,(find-symbol "*CONTRIB-BLACKLIST*" :moonli))))
  (uiop:symbol-call :moonli '#:require-all-contribs)
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   :compression t))

(defsystem "moonli/asdf"
  :depends-on ("moonli")
  :pathname #p"src/"
  :components ((:file "asdf")))

(defsystem "moonli/repl"
  :depends-on ("uiop"
               "moonli"
               "cl-repl"
               "for"
               "com.inuoe.jzon"
               "parse-float")
  :build-operation "program-op"
  :build-pathname "../moonli.repl"
  :entry-point "cl-repl:main"
  :pathname #p"src/"
  :license "GPL3v3" ;; due to cl-repl
  :components ((:file "repl/package")
               (:module "extra-macros"
                :components ((:file "for")))
               (:file "repl/repl")))

(defsystem "moonli/coalton"
  :depends-on ("moonli"
               "coalton")
  :pathname #p"src/extra-macros/"
  :components ((:file "coalton")))


(defsystem "moonli/ciel"
  :depends-on ("moonli/repl"
               "ciel")
  :build-operation "program-op"
  :build-pathname "../moonli.ciel"
  :entry-point "cl-repl:main"
  :pathname "src/"
  :license "GPL3v3" ;; due to cl-repl
  :components ((:file "repl/ciel")))

(defsystem "moonli/alive-lsp"
  :pathname #p"src/"
  :depends-on ("moonli"
               "alive-lsp")
  :components ((:file "alive-lsp")))
