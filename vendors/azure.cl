;;;; See the file LICENSE for the full license governing this code.
(defpackage #:acl-llm.protocol
  (:export #:llm-azure
           #:make-llm-azure))

(in-package #:acl-llm.protocol)

(defclass llm-azure (llm-openai-compatible)
  ((endpoint-url :initarg :endpoint-url
                 :accessor llm-azure-endpoint-url)))

;;; Mircosoft Azure has multiple ways for authentication.
;;; We'll stick to the easiest one for now.
;;; That is, the endpoint-url here is the OpenAI service deployment's *Target URI*,
;;; and the key here is also the deployment's *Key*.
;;; Both of them can be found at the service's deployment page.
;;;
;;; In the future, we might have to implement more sophisticated strategies for
;;; authentication.

(defun make-llm-azure (&key key endpoint-url &allow-other-keys)
  (make-instance 'llm-azure
                 :key key
                 :endpoint-url endpoint-url))

(defmethod llm-vendor-chat-url ((vendor llm-azure))
  (llm-azure-endpoint-url vendor))

(defmethod llm-vendor-embedding-url ((vendor llm-azure))
  (llm-azure-endpoint-url vendor))
