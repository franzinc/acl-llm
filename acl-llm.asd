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
     (:file "vector-database")
     (:file "writing-floats")
     (:file "embed")
     (:file "shortq")
     (:file "util")
     (:file "llama-cpp")
     (:file "openai")))


     
