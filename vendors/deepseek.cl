;;;; See the file LICENSE for the full license governing this code.
(in-package #:cl-user)

(defpackage #:acl-llm.protocol
  (:export #:llm-deepseek
           #:make-llm-deepseek))

(in-package #:acl-llm.protocol)

(defconstant +llm-deepseek-endpoint+ "https://api.deepseek.com")

(defconstant +llm-deepseek-default-model+ "deepseek-chat")

(defclass llm-deepseek (llm-openai-compatible)
  ()
  (:documentation "See DeepSeek API documentation at:
https://api-docs.deepseek.com/api"))

(defun make-llm-deepseek (&key key (chat-model +llm-deepseek-default-model+))
  (make-instance 'llm-deepseek
                 :endpoint +llm-deepseek-endpoint+
                 :key key
                 :chat-model chat-model))

;;; DeepSeek doesn't support JSON Schema yet
;;; See here: https://api-docs.deepseek.com/api/create-chat-completion
(defmethod llm-vendor-chat-request :before ((vendor llm-deepseek) prompt streaming)
  (declare (ignore streaming))
  (let ((response-format (llm-chat-prompt-response-format prompt)))
    (when (consp response-format)
      (error "DeepSeek API doesn't support JSON Schema for structured outputs!"))))

(defmethod llm-embedding ((vendor llm-deepseek) text)
  (declare (ignore text))
  (error "DeekSeek API doesn't support embeddings!"))

(defmethod llm-batch-embeddings ((vendor llm-deepseek) batch)
  (declare (ignore batch))
  (error "DeekSeek API doesn't support batch embeddings!"))
