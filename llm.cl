;; See the file "LICENSE" for the full license governing this code.

#|
;; not needed when found in agraph source
#+(or (version= 10 1) (version= 11 0 beta))
(sys:defpatch "acl-llm" 4
  "v4: fixed off-by-one error in shortq
v3: code to work with LLM embeddings
v2: fix interaction between :output-format and :function in ask-chat;
v1: Function-calling API;
v0: Initial release of the :acl-llm module."
  :type :system
  :post-loadable t)
  |#


(eval-when (compile load eval)
  (require :aserve)
  (require :st-json))

(defpackage :llm
  (:use :cl :excl :st-json)
  (:export
   #:*default-vector-database-name*
   #:*default-vector-database-dir*
   #:mag
   #:nn
   #:read-vector-database
   #:single-float-array
   #:vector-database
   #:vector-database-name
   #:make-vector-database
   #:sample-vector-database           
   #:vector-database-embedding-vectors
   #:vector-database-property-vectors
   #:write-vector-database
   #:jso-val
   #:json-string           
   #:pushjso
   #:read-lines-from-stream
   ))


(defpackage :llm.gpt
  (:shadowing-import-from :llm
                          #:read-lines-from-stream
                          #:pushjso
                          #:jso-val
                          #:json-string)
  (:use :cl :excl)
  (:import-from :st-json #:jso #:read-json)  
  (:import-from :llm
                #:*default-vector-database-name*
                #:*default-vector-database-dir*
                #:mag
                #:nn
                #:read-vector-database
                #:single-float-array
                #:vector-database
                #:vector-database-name
                #:make-vector-database
                #:sample-vector-database           
                #:vector-database-embedding-vectors
                #:vector-database-property-vectors
                #:write-vector-database
                )
  (:nicknames :gpt)
  (:export
;;; export all the symbols imported from nn
   #:*default-vector-database-name*
   #:*default-vector-database-dir*
   #:mag
   #:nn
   #:read-vector-database
   #:single-float-array
   #:vector-database
   #:vector-database-name
   #:make-vector-database
   #:sample-vector-database           
   #:vector-database-embedding-vectors
   #:vector-database-property-vectors
   #:write-vector-database
  
;;; export symbols from gpt
   #:*openai-api-key*
   #:ask-chat
   #:ask-embed
   #:ask-for-list
   #:call-oopenai
   #:cancel-fine-tune
   #:chat
   #:delete-fine-tuned-model
   #:delete-openai-file
   #:embed
   #:fine-tune
   #:fine-tune-report
   #:fine-tune-status
   #:list-openai-files
   #:list-openai-models
   #:set-openai-api-key
   #:upload-openai-file)
  )

(defpackage :llm.llama.cpp
  (:shadowing-import-from :llm
                          #:read-lines-from-stream
                          #:pushjso
                          #:jso-val
                          #:json-string)

  (:use :cl :excl)
  (:import-from :st-json #:jso #:read-json)
  (:import-from :llm
                #:*default-vector-database-name*
                #:*default-vector-database-dir*
                #:mag
                #:nn
                #:read-vector-database
                #:single-float-array
                #:vector-database
                #:vector-database-name
                #:make-vector-database
                #:sample-vector-database           
                #:vector-database-embedding-vectors
                #:vector-database-property-vectors
                #:write-vector-database
                )
  (:nicknames :llama2 :llama.cpp)
  (:export
;;; export all the symbols imported from nn
   #:*default-vector-database-name*
   #:*default-vector-database-dir*
   #:mag
   #:nn
   #:read-vector-database
   #:single-float-array
   #:vector-database
   #:vector-database-name
   #:make-vector-database
   #:sample-vector-database           
   #:vector-database-embedding-vectors
   #:vector-database-property-vectors
   #:write-vector-database

;;; export symbols from llama2
   #:*llama-cpp-python-api-protocol*
   #:*llama-cpp-python-api-host*
   #:*llama-cpp-python-api-port*
   #:ask-chat
   #:embed

   ))


(provide :acl-llm)
