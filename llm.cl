;; See the file "LICENSE" for the full license governing this code.;;

(eval-when (compile load eval)
  (require :aserve)
  (require :st-json))


(defpackage :llm.gpt 
  (:use :cl :excl :st-json)
  
  (:nicknames :gpt)
  
  (:export #:*openai-api-key*
           
           #:ask-chat
           #:ask-json
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

   




        

