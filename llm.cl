;; See the file "LICENSE" for the full license governing this code.


#+(and acl-llm-build (version= 10 1))
(sys:defpatch "acl-llm" 5
"v5: ask-for-table added/LLAMA2 API refinement/ask-serp SERP API interface
v4: fixed off-by-one error in shortq
v3: code to work with LLM embeddings
v2: fix interaction between :output-format and :function in ask-chat;
v1: Function-calling API;
v0: Initial release of the :acl-llm module."
  :type :system
  :post-loadable t)

#+(and acl-llm-build (version= 11 0))
(sys:defpatch "acl-llm" 0
  "v0: Initial release of the :acl-llm module."
  :type :system
  :post-loadable t)

(eval-when (compile load eval)
  (require :aserve)
  (require :st-json))

(defpackage :llm
  (:documentation "llm package contains code that works with any LLM model (GPT, LLAMA2 etc.")
  (:use :cl :excl :st-json)
  (:export
   #:*default-vector-database-name*
   #:*default-vector-database-dir*
   #:args-list-macro
   #:handle-llm-error
   #:key-args-fun
   #:key-args-list
   #:key-args-signature
   #:key-args-pushjso
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
   #:vector-database-embedder
   #:describe-vector-database
   #:write-vector-database
   #:jso-val
   #:json-string
   #:pushjso
   #:delete-jso-val
   #:read-lines-from-stream
   #:cosine-similarity
   #:*serp-api-key*
   #:ask-serp
   #:set-serp-api-key
   #:call-serpapi
   #:traverse-serp-jso
   #:log-llm
   ))


(defpackage :llm.gpt
  (:shadowing-import-from :llm
                          #:handle-llm-error                          
                          #:args-list-macro
                          #:key-args-fun
                          #:key-args-list
                          #:key-args-signature
                          #:key-args-pushjso
                          #:read-lines-from-stream
                          #:pushjso
                          #:delete-jso-val
                          #:jso-val
                          #:json-string
                          )
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
                #:*serp-api-key*                
                #:ask-serp
                #:set-serp-api-key
                #:call-serpapi
                #:traverse-serp-jso
                #:log-llm                
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
   #:vector-database-embedder
   #:describe-vector-database
   #:write-vector-database

;;; export symbols from gpt
   #:*openai-api-key*
   #:ask-chat
   #:ask-embed
   #:ask-for-list
   #:ask-for-table
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
                          #:delete-jso-val
                          #:jso-val
                          #:json-string)

  (:use :cl :excl)
  (:import-from :st-json #:jso #:read-json)
  (:import-from :llm
                #:*default-vector-database-name*
                #:*default-vector-database-dir*
                #:handle-llm-error                
                #:args-list-macro
                #:key-args-fun
                #:key-args-list
                #:key-args-signature
                #:key-args-pushjso
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
                #:vector-database-embedder
                #:describe-vector-database
                #:write-vector-database
                #:*serp-api-key*                
                #:ask-serp
                #:set-serp-api-key
                #:call-serpapi
                #:traverse-serp-jso
                #:log-llm                
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
