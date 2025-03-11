(in-package #:cl-user)

(defpackage #:acl-llm.protocol
  (:export #:llm-databricks
           #:make-llm-databricks
           #:llm-databricks-endpoint-url))

(in-package #:acl-llm.protocol)

(defclass llm-databricks (llm-openai-compatible)
  ((endpoint-url :initarg :endpoint-url
                 :accessor llm-databricks-endpoint-url)))

(defun make-llm-databricks (&key key endpoint-url)
  (make-instance 'llm-databricks
                 :key key
                 :endpoint-url endpoint-url))

(defmethod llm-vendor-chat-url ((vendor llm-databricks))
  (llm-databricks-endpoint-url vendor))

(defmethod llm-vendor-embedding-url ((vendor llm-databricks))
  (llm-databricks-endpoint-url vendor))
