(defsystem "moonli"
  :depends-on ("alexandria"
               "esrap"
               "fiveam"
               "let-plus"
               "optima"
               "parse-number"
               (:feature (:not :swank) "swank")
               "unix-opts")
  :licence "MIT"
  :author "Shubhamkar Ayare (digikar@proton.me)"
  :version "0.0.6"
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
                             (:file "defclass")
                             (:file "labels")
                             (:file "let-plus")
                             (:file "match")))
               (:file "moonli")
               (:file "pretty-printer")
               (:file "binary"))
  :perform (test-op (c s)
             (eval (read-from-string "(5AM:RUN! :MOONLI)")))
  :perform (program-op (o c)
             (uiop:dump-image "moonli" :executable t
                                       :compression #+sb-core-compression 22 #-sb-core-compression nil))
  :build-operation "program-op"
  :entry-point "moonli:main")

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
  :components ((:file "repl/package")
               (:module "extra-macros"
                :components ((:file "short")
                             (:file "for")))
               (:file "repl/main")))

(defsystem "moonli/alive-lsp"
  :pathname #p"src/"
  :depends-on ("moonli"
               "alive-lsp")
  :components ((:file "alive-lsp")))

(defsystem "moonli/swank"
  :pathname #p"src/"
  :depends-on ("moonli"
               (:feature (:not :swank) "swank"))
  :components ((:file "swank")))
