;;;; See the file LICENSE for the full license governing this code.
(defpackage #:acl-llm.protocol
  (:export #:*llm-ollama-example-prelude*
           #:llm-ollama-chat-timeout*
           #:llm-ollama
           #:make-llm-ollama
           #:llm-ollama-scheme
           #:llm-ollama-host
           #:llm-ollama-port
           #:llm-ollama-chat-model
           #:llm-ollama-embedding-model))

(in-package #:acl-llm.protocol)

(defvar *llm-ollama-example-prelude* nil
  "The prelude to use for examples in Ollama chat prompts.")

(defvar *llm-ollama-chat-timeout* 300
  "Timeout for sync ollama chat calls.")

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

(defun make-llm-ollama (&key (scheme "http") (host "localhost") (port 11434) chat-model embedding-model &allow-other-keys)
  (make-instance 'llm-ollama
                 :scheme scheme
                 :host host
                 :port port
                 :chat-model chat-model
                 :embedding-model embedding-model))

(defmethod llm-capabilities ((vendor llm-ollama))
  (list :streaming :embeddings :embeddings-batch :tool-uses))

(defmethod llm-name ((vendor llm-ollama))
  (or (llm-ollama-chat-model vendor)
      (llm-ollama-embedding-model vendor)))

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
  (st-json:getjso
   "content"
   (st-json:getjso "message" response)))

(defmethod llm-vendor-extract-tool-uses ((vendor llm-ollama) response)
  (loop with message = (st-json:getjso "message" response)
        for call in (st-json:getjso "tool_calls" message)
        for function = (st-json:getjso "function" call)
        collect (make-llm-vendor-utils-tool-use
                 :name (st-json:getjso "name" function)
                 :args (st-json:getjso "arguments" function))))

(defmethod llm-vendor-populate-tool-uses ((vendor llm-ollama) prompt tool-uses)
  (llm-vendor-utils-append-to-prompt
   prompt
   (loop for tool-use in tool-uses
         collect (st-json:jso
                  "function" (st-json:jso
                              "name" (llm-vendor-utils-tool-use-name tool-use)
                              "arguments" (llm-vendor-utils-tool-use-args tool-use))))))

(defun llm-ollama-response-format (format)
  (etypecase format
    (keyword (if* (eq format :json)
                then "json"
                else (error "When response-format is given as a keyword, it must be \":json\" but ~s was given." format)))
    (cons (llm-vendor-utils-convert-plist-to-jso format))
    (string (st-json:read-json-from-string format))
    (st-json:jso format)))

(defmethod llm-vendor-chat-request ((vendor llm-ollama) prompt streaming)
  (llm-vendor-utils-combine-to-system-prompt prompt *llm-ollama-example-prelude*)
  (let* ((options (st-json:jso))
         (request (st-json:jso
                   "model" (llm-ollama-chat-model vendor)
                   "messages" (loop for exchange in (llm-chat-prompt-exchanges prompt)
                                    for role = (llm-chat-prompt-exchange-role exchange)
                                    collect (st-json:jso
                                             "role" (symbol-name role)
                                             "content" (llm-chat-prompt-exchange-content exchange)))
                   "options" options
                   "stream" (st-json:as-json-bool streaming))))
    ;; populate tool calls
    (when (llm-chat-prompt-tools prompt)
      (setf (st-json:getjso "tools" request)
            (loop for tool in (llm-chat-prompt-tools prompt)
                  collect (llm-vendor-utils-openai-tool-spec tool))))
    ;; response-format
    (when (llm-chat-prompt-response-format prompt)
      (setf (st-json:getjso "format" request)
            (llm-ollama-response-format (llm-chat-prompt-response-format prompt))))
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
  (llm-ollama-url vendor "embed"))

(defmethod llm-vendor-embedding-request ((vendor llm-ollama) text)
  (st-json:jso "input" text
               "model" (llm-ollama-embedding-model vendor)))

(defmethod llm-vendor-batch-embeddings-request ((vendor llm-ollama) text-sequence)
  (st-json:jso "input" text-sequence
               "model" (llm-ollama-embedding-model vendor)))

(defmethod llm-vendor-embedding-extract-result ((vendor llm-ollama) response)
  (first (st-json:getjso "embeddings" response)))

(defmethod llm-vendor-batch-embeddings-extract-result ((vendor llm-ollama) response)
  (st-json:getjso "embeddings" response))

(defmethod llm-vendor-embedding-extract-error ((vendor llm-ollama) response)
  (st-json:getjso "error" response))
