;;;; See the file LICENSE for the full license governing this code.
(in-package #:cl-user)

(defpackage #:acl-llm-tests-system
  (:use #:common-lisp
        #:asdf
        #:asdf-extensions))

(in-package #:acl-llm-tests-system)

(defsystem #:acl-llm-tests
  :class franz-system
  :depends-on (#:acl-llm
               #:db-agraph-tests-utilities)
  :components
  ((:module "tests"
    :serial t
    :components ((:file "llm")
                 (:file "llm-vendor-utils" :depends-on ("llm"))
                 (:file "ollama" :depends-on ("llm"))
                 (:file "openai" :depends-on ("llm"))
                 (:module "openai-compatible-vendors"
                  :pathname ""
                  :depends-on ("openai")
                  :components ((:file "deepseek")
                               (:file "groq")
                               (:file "voyage")))))))
