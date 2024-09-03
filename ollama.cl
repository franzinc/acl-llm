;;;; See the file LICENSE for the full license governing this code.

(in-package #:acl-llm.protocol)

(defvar *llm-ollama-example-prelude* "Examples of how you should respond follow."
  "The prelude to use for examples in Ollama chat prompts.")

(defvar *llm-ollama-chat-timeout* 300
  "Timeout for sync ollama chat calls.")

(defparameter *llm-ollama-scheme* "http")
(defparameter *llm-ollama-host* "localhost")
(defparameter *llm-ollama-port* 11434)
(defparameter *llm-ollama-chat-model* nil)
(defparameter *llm-ollama-embedding-model* nil)

(defclass llm-ollama (llm-standard-full-vendor)
  ((scheme :initarg :scheme :initform "http" :accessor llm-ollama-scheme)
   (host :initarg :host :initform "localhost" :accessor llm-ollama-host)
   (port :initarg :port :initform 11434 :accessor llm-ollama-port)
   (chat-model :initarg :chat-model :accessor llm-ollama-chat-model)
   (embedding-model :initarg :embedding-model :accessor llm-ollama-embedding-model))
  (:documentation "A class for holding information needed by Ollama's API.

`scheme' is the http scheme to use, a string. It is optional and default to
`http'.

`host' is the host that Ollama is running on. It is optional and default to
localhost.

`port' is the localhost port that Ollama is running on. It is optional.

`chat-model' is the model to use for chat queries. It is required.

`embedding-model' is the model to use for embeddings.  It is required."))

(defmethod llm-capabilities ((vendor llm-ollama))
  (list 'streaming 'embeddings))

(defmethod llm-name ((vendor llm-ollama))
  (llm-ollama-chat-model vendor))

(defmethod llm-chat-token-limit ((vendor llm-ollama))
  (llm-vendor-utils-model-token-limit (llm-ollama-chat-model vendor)))

(defun llm-ollama-url (vendor method)
  (with-slots (scheme host port) vendor
    (format nil "~a://~a:~d/api/~a" scheme host port method)))

(defmethod llm-vendor-chat-url ((vendor llm-ollama))
  (llm-ollama-url vendor "chat"))

(defmethod llm-vendor-chat-timeout ((vendor llm-ollama))
  *llm-ollama-chat-timeout*)

(defmethod llm-vendor-chat-extract-error ((vendor llm-ollama) response)
  (st-json:getjso "error" response))

(defmethod llm-vendor-chat-extract-result ((vendor llm-ollama) response)
  "Return the chat response from the server RESPONSE."
  (st-json:getjso "content" (st-json:getjso "message" response)))

(defun ollama-function-call-spec (call)
  (declare (type llm-function-call call))
  (let* ((properties (st-json:jso))
         (spec (st-json:jso
                "type" "function"
                "function" (st-json:jso
                            "name" (llm-function-call-name call)
                            "description" (llm-function-call-description call)
                            "parameters" (st-json:jso
                                          "type" "object"
                                          "properties" properties
                                          "required" (mapcar #'llm-function-arg-name
                                                             (remove-if-not #'llm-function-arg-required (llm-function-call-args call))))))))
    (dolist (arg (llm-function-call-args call))
      (setf (st-json:getjso (llm-function-arg-name arg) properties)
            (st-json:jso
             "type" (string (llm-function-arg-type arg))
             "description" (llm-function-arg-description arg))))
    spec))

(defmethod llm-vendor-chat-request ((vendor llm-ollama) prompt streaming)
  (let* ((options (st-json:jso))
         (request (st-json:jso
                   "model" (llm-ollama-chat-model vendor)
                   "messages" (list)
                   "options" options
                   "stream" (st-json:as-json-bool streaming)
                   "tools" (list))))
    ;; populate messages
    (dolist (exchange (llm-chat-prompt-exchanges prompt))
      (push (st-json:jso "role" (symbol-name (llm-chat-prompt-exchange-role exchange))
                         "content" (llm-chat-prompt-exchange-content exchange))
            (st-json:getjso "messages" request)))
    (when (llm-chat-prompt-context prompt)
      (push (st-json:jso "role" "system"
                         "content" (llm-vendor-utils-get-system-prompt vendor))
            (st-json:getjso "messages" request)))
    ;; populate function calls
    (when (llm-chat-prompt-functions prompt)
      (when streaming
        (error "Ollama does not yet support streaming with tool calls"))
      (dolist (call (llm-chat-prompt-functions prompt))
        (push (ollama-function-call-spec call)
              (st-json:getjso "tools" request))))
    ;; populate options
    (when (llm-chat-prompt-temperature prompt)
      (setf (st-json:getjso "temperature" options) (llm-chat-prompt-temperature vendor)))
    (when (llm-chat-prompt-max-tokens prompt)
      (setf (st-json:getjso "num_predict" options) (llm-chat-prompt-max-tokens vendor)))
    ;; non-standard parameters
    (loop for (k . v) in (llm-chat-prompt-non-standard-params prompt)
          do (setf (st-json:getjso k options) v))
    request))

(defmethod llm-vendor-embedding-url ((vendor llm-ollama))
  (llm-ollama-url vendor "embeddings"))

(defmethod llm-vendor-embedding-request ((vendor llm-ollama) text)
  (st-json:jso "prompt" text
               "model" (llm-ollama-embedding-model vendor)))

(defmethod llm-vendor-embedding-extract-result ((vendor llm-ollama) response)
  (st-json:getjso "embedding" response))

(defmethod llm-vendor-embedding-extract-error ((vendor llm-ollama) response)
  (st-json:getjso "error" response))
