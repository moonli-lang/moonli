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
  :version "0.0.10"
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
               (:file "contribs" :if-feature :sbcl))
  :perform (test-op (c s)
             (eval (read-from-string "(5AM:RUN! :MOONLI)")))
  :build-operation "program-op"
  :build-pathname "../moonli"
  :entry-point "moonli:main")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (eval `(push :sb-aclrepl ,(find-symbol "*CONTRIB-BLACKLIST*" :moonli)))
  (uiop:symbol-call :moonli '#:require-all-contribs)
  (when (find-package :cffi)
    (mapc (fdefinition (find-symbol "CLOSE-FOREIGN-LIBRARY" "CFFI"))
          (remove (find-symbol "LIBISOCLINE" "ISOCLINE")
                  (uiop:symbol-call :cffi "LIST-FOREIGN-LIBRARIES")
                  :key (find-symbol "FOREIGN-LIBRARY-NAME" "CFFI"))))
  (asdf:clear-configuration)
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
               "binding-arrows"
               "isocline-repl"
               "for"

               "docsearch"
               "parse-float"
               "trivial-posix-fs"
               "com.inuoe.jzon"
               "file-attributes"
               "trivial-package-local-nicknames")
  :build-operation "program-op"
  :build-pathname "../moonli.repl"
  :entry-point "moonli/repl:main"
  :pathname #p"src/"
  :license "MIT"
  :serial t
  :components ((:module "repl"
                :components ((:file "package")
                             (:file "utils")
                             (:file "opts")
                             (:file "repl")))
               (:module "extra-macros"
                :components ((:file "for")
                             (:file "arrows")))))

(defsystem "moonli/ciel"
  :depends-on ("moonli/repl"
               "ciel")
  :build-operation "program-op"
  :build-pathname "../moonli.ciel"
  :entry-point "cl-repl:main"
  :pathname "src/"
  :license "MIT"
  :components ((:file "repl/ciel")))

(defsystem "moonli/alive-lsp"
  :pathname #p"src/"
  :depends-on ("moonli"
               "alive-lsp")
  :components ((:file "alive-lsp")))

(defsystem "moonli/curated-libraries"
  :description "A collection of curated libraries designed to correspond with the functionalities of Python's standard libraries."
  :depends-on (;; Threading
               "bordeaux-threads"

               ;; Text Processing Services
               "cl-ppcre"
               "str"

               ;; Numeric and Mathematical Libraries
               "array-operations"
               "nibbles"

               ;; Functional Programming Libraries
               "alexandria"
               "coalton"
               "fset"

               ;; File and Directory Access
               "file-finder"
               "trivial-posix-fs" ; experimental
               "file-attributes"

               ;; Data Persistence
               "postmodern"
               "mito"
               "clsql"
               "bknr.datastore"

               ;; TODO: Data Compression and Archiving

               ;; File Formats
               "file-formats"
               "com.inuoe.jzon"
               "shasht"
               "fare-csv"

               ;; Cryptographic Services
               "ironclad"

               ;; TODO: Generic Operating System Services

               ;; Command-line Interface
               "unix-opts"
               "clingon"

               ;; Concurrent Execution
               "lparallel"

               ;; TODO: Internet Data Handling

               ;; TODO: Structured Markup Processing Tools

               ;; Internet Protocols and Support
               "usocket"
               "hunchentoot"
               "hunchensocket"
               "dexador"

               ;; TODO: Multimedia Services

               ;; Internationalization

               "local-time"

               ;; Graphical User Interfaces
               "clog"
               "ltk"
               "isocline-repl"

               ;; Development Tools
               "esrap"
               "quicksearch"
               "quickproject"
               (:feature (:not :ql-https) "ql-https")
               (:feature (:not :swank) "swank")
               "trivial-package-local-nicknames"

               ;; Debugging and Profiling
               "swank"
               "log4cl"

               ;; Software Packaging and Distribution
               "deploy"

               ;; TODO: Runtime Services
               "closer-mop"

               ;; TODO: Language Services
               "trivial-features"
               "policy-cond"
               "cl-environments"
               "cl-form-types"
               "float-features"
               "trivial-garbage"

               ;; Foreign Libraries
               "cffi"
               "cl-autowrap"
               "py4cl2"
               "py4cl2-cffi"

               ;; Iteration
               "iterate"
               "series"
               "picl"

               ;; Pattern Matching
               "optima"

               ;; Code Generation
               "cl-who"
               "parenscript"

               ;; Documentation
               "mgl-pax"
               "docsearch"


               ;; Testing
               "fiveam"

               ;; TODO: Windows specific

               ;; Unix Specific
               "osicat"))
