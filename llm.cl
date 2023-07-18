;; See the file "LICENSE" for the full license governing this code.

#+(or (version= 10 1) (version= 11 0 beta))
(sys:defpatch "acl-llm" 2
  "v2: fix interaction between :output-format and :function in ask-chat;
   v1: Function-calling API;
   v0: Initial release of the :acl-llm module."
  :type :system
  :post-loadable t)

(eval-when (compile load eval)
  (require :aserve)
  (require :st-json))


(defpackage :llm.gpt 
  (:use :cl :excl :st-json)
  
  (:nicknames :gpt)
  
  (:export #:*openai-api-key*
           
           #:ask-chat
           #:ask-for-list
           #:call-openai
           #:cancel-fine-tune
           #:chat
           #:delete-fine-tuned-model
           #:delete-openai-file
           #:fine-tune
           #:fine-tune-report
           #:fine-tune-status
           #:list-openai-files
           #:list-openai-models
           #:set-openai-api-key
           #:upload-openai-file
           ))

(defpackage :llm.gpt-user
  (:use :cl :excl :st-json :llm.gpt)
  (:nicknames :gpt-user))

(provide :acl-llm)




        

