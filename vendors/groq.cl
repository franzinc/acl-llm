;;;; See the file LICENSE for the full license governing this code.
(defpackage #:acl-llm.protocol
  (:export #:llm-groq
           #:make-llm-groq))

(in-package #:acl-llm.protocol)

(defconstant +llm-groq-endpoint+ "https://api.groq.com/openai/v1")

(defclass llm-groq (llm-openai-compatible)
  ()
  (:documentation "See Groq API documentation at:
https://console.groq.com/docs/api-reference"))

(defun make-llm-groq (&key key chat-model &allow-other-keys)
  (make-instance 'llm-groq
                 :endpoint +llm-groq-endpoint+
                 :key key
                 :chat-model chat-model))

;;; Groq doesn't support JSON Schema yet
;;; See here: https://console.groq.com/docs/api-reference#chat-create
(defmethod llm-vendor-chat-request :before ((vendor llm-groq) prompt streaming)
  (declare (ignore streaming))
  (let ((response-format (llm-chat-prompt-response-format prompt)))
    (when (consp response-format)
      (error "Groq API doesn't support JSON Schema for structured outputs!"))))

(defmethod llm-embedding ((vendor llm-groq) text)
  (declare (ignore text))
  (error "Groq API doesn't support embeddings!"))

(defmethod llm-batch-embeddings ((vendor llm-groq) batch)
  (declare (ignore batch))
  (error "Groq API doesn't support batch embeddings!"))
