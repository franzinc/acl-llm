;;;; See the file LICENSE for the full license governing this code.
(in-package #:cl-user)

(defpackage #:acl-llm.protocol
  (:export #:llm-voyage
           #:make-llm-voyage))

(in-package #:acl-llm.protocol)

(defconstant +llm-voyage-endpoint+ "https://api.voyageai.com/v1")

(defclass llm-voyage (llm-openai-compatible)
  ()
  (:documentation "See Voyage API documentation at:
https://docs.voyageai.com/reference/embeddings-api"))

(defun make-llm-voyage (&key key embedding-model)
  (make-instance 'llm-voyage
                 :endpoint +llm-voyage-endpoint+
                 :key key
                 :embedding-model embedding-model))

(defmethod llm-chat ((vendor llm-voyage) prompt)
  (declare (ignore prompt))
  (error "Voyage API doesn't support chat!"))
