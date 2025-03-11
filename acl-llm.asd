(defpackage #:acl-llm-system
  (:use #:common-lisp #:asdf #:asdf-extensions))

(in-package #:acl-llm-system)

(defsystem acl-llm
    :class franz-system
    :serial t
    :depends-on (:db.agraph.triple-store)
    :components
    (
     (:file "llm")
     (:file "util")
     ;;(:file "vector-database")
     ;;(:file "writing-floats")
     (:file "embed" :depends-on ("util"))
     ;;(:file "shortq")
     ;;(:file "llama-cpp" :depends-on ("util"))
     (:file "openai" :depends-on ("util"))
     (:file "serp" :depends-on ("util"))
     (:file "protocol")
     (:module "vendors"
      :depends-on ("protocol")
      :components ((:file "anthropic")
                   (:file "ollama")
                   (:file "openai")
                   ;; OpenAI-compatible vendors
                   (:module "openai-compatible"
                    :pathname ""
                    :depends-on ("openai")
                    :components ((:file "deepseek")
                                 (:file "groq")
                                 (:file "voyage")
                                 (:file "databricks")))))))
