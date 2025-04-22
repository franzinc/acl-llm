;;;; See the file LICENSE for the full license governing this code.
(defpackage #:acl-llm.protocol
  (:export #:*llm-openai-example-prelude*
           #:llm-openai
           #:make-llm-openai
           #:llm-openai-key
           #:llm-openai-chat-model
           #:llm-openai-embedding-model
           #:llm-openai-embedding-length
           #:llm-openai-compatible
           #:llm-openai-compatible-endpoint))

(in-package #:acl-llm.protocol)

(defvar *llm-openai-example-prelude* nil
  "The prelude to use for examples in OpenAI chat prompts.")

(defconstant +llm-openai-endpoint-url+ "https://api.openai.com/v1/")
(defconstant +llm-openai-default-chat-model+ "gpt-4o")
(defconstant +llm-openai-default-embedding-model+ "text-embedding-3-small")
(defconstant +llm-openai-default-embedding-length+ 1536)

(defclass llm-openai (llm-standard-full-vendor)
  ((key :initarg :key :accessor llm-openai-key :type (or function simple-string))
   (chat-model :initform +llm-openai-default-chat-model+ :initarg :chat-model :accessor llm-openai-chat-model :type simple-string)
   (embedding-model :initform +llm-openai-default-embedding-model+ :initarg :embedding-model :accessor llm-openai-embedding-model :type simple-string))
  (:documentation "A structure for holding information needed by OpenAI's API.

`key' is the API key for OpenAI, which is required.

`chat-model' is the model to use for chat queries. If unset, it will use a
reasonable default.

`embedding-model' is the model to use for embeddings. If unset, it will use a
reasonable default."))

(defun make-llm-openai (&key key chat-model embedding-model embedding-length
                        &allow-other-keys)
  (let ((es (db.agraph::internal-lookup-embedder "openai"
                                                 embedding-model
                                                 embedding-length)))
    ;; use defaults for model if not given
    ;; find length if not given from model if possible
    (make-instance 'llm-openai 
      :key (or key gpt:*openai-api-key*) ; hack, must generalize
      :chat-model (or chat-model +llm-openai-default-chat-model+)
      :embedding-model (or embedding-model 
                           (and es (db.agraph::embedder-model es))
                           +llm-openai-default-embedding-model+)
      :llm-embedding-length (or embedding-length
                                (and es (db.agraph::embedder-length es))))))

(db.agraph::define-embedder "openai"
    :models (("text-embedding-ada-002" 
              :length 1536
              :needs-api-key-p t
              :defaultp t)
             ("text-embedding-3-large" 
              :length 1536
              :can-set-length-p t   
              :needs-api-key-p t
              :defaultp nil)
             ("text-embedding-3-small" 
              :length 1536
              :can-set-length-p t
              :needs-api-key-p t
              :defaultp nil)
             ))
             
              

(defun llm-openai-api-key (openai)
  (when (not (slot-boundp openai 'key))
    (error "To call OpenAI API, please add an API key to ~a" openai))
  (let ((key (llm-openai-key openai)))
    (if* (stringp key)
       then key
     elseif (functionp key)
       then (funcall key)
       else (error "API key must either be a string or a function that takes no argument, but got ~a" key))))


;; this is here to make the cypress test suite pass
;; and will be modified later
(defmethod llm-vendor-request-prelude ((vendor llm-openai))
  (or (and (> (length (llm-openai-key vendor)) 0)
           (not (equal (llm-openai-key vendor) "missing")))
      (error 'llm-api-key-missing)))

(defmethod llm-vendor-headers ((vendor llm-openai))
  (list (cons "Authorization"
              (string+ "Bearer " (llm-openai-api-key vendor)))))

(defmethod llm-chat-token-limit ((vendor llm-openai))
  (llm-vendor-utils-model-token-limit (llm-openai-chat-model vendor)))

(defun llm-openai-url (method)
  (string+ +llm-openai-endpoint-url+ method))

(defmethod llm-vendor-chat-url ((vendor llm-openai))
  (llm-openai-url "chat/completions"))

(defun llm-openai-extract-error (response)
  (let ((err (st-json:getjso "error" response)))
    (when err
      (format nil "OpenAI returned error type: ~s~%code: ~s~%message: ~s"
              (st-json:getjso "type" err)
              (st-json:getjso "code" err)
              (st-json:getjso "message" err)))))

(defmethod llm-vendor-chat-extract-error ((vendor llm-openai) response)
  (llm-openai-extract-error response))

(defmethod llm-vendor-chat-extract-result ((vendor llm-openai) response)
  (st-json:getjso
   "content"
   (st-json:getjso
    "message"
    (first (st-json:getjso "choices" response)))))

(defmethod llm-vendor-extract-tool-uses ((vendor llm-openai) response)
  (loop with choice = (first (st-json:getjso "choices" response))
        with message = (st-json:getjso "message" choice)
        with tool-calls = (st-json:getjso "tool_calls" message)
        for call in tool-calls
        for call-id = (st-json:getjso "id" call)
        for tool = (st-json:getjso "function" call)
        for tool-name = (st-json:getjso "name" tool)
        for arguments = (st-json:read-json-from-string (st-json:getjso "arguments" tool))
        collect (make-llm-vendor-utils-tool-use
                 :id call-id
                 :name tool-name
                 :args arguments)))

(defmethod llm-vendor-populate-tool-uses ((vendor llm-openai) prompt tool-uses)
  (llm-vendor-utils-append-to-prompt prompt tool-uses nil 'assistant))

(defun llm-openai-response-format (format)
  (labels ((search-and-set-additional-properties (k v)
             "This function recursively search and add \"additionalProperties\" field if it's
necessary to do so."
             (declare (ignore k))
             (when (string= "array" (st-json:getjso "type" v))
               (set-additional-properties (st-json:getjso "items" v))))
           (set-additional-properties (schema)
             (when schema
               (setf (st-json:getjso "additionalProperties" schema)
                     (st-json:as-json-bool nil))
               (let ((properties (st-json:getjso "properties" schema)))
                 (when properties
                   (st-json:mapjso #'search-and-set-additional-properties
                                   properties))))))
    (etypecase format
      (keyword (if* (eq format :json)
                  then (st-json:jso "type" "json_object")
                  else (error "When response-format is given as a keyword, it must be \":json\" but ~s was given." format)))
      (string (st-json:read-json-from-string format))
      (st-json:jso format)
      (cons (st-json:jso
             "type" "json_schema"
             "json_schema" (st-json:jso
                            "name" "response"
                            "strict" (st-json:as-json-bool t)
                            "schema" (let ((schema (llm-vendor-utils-convert-plist-to-jso format)))
                                       ;; check top-level "type"
                                       ;; for OpenAI, it must be "object"
                                       (unless (and #1=(st-json:getjso "type" schema)
                                                    (string-equal "object" #1#))
                                         (error "OpenAI's json schema must have type \"object\" at the top level but got: ~s"
                                                #1#))
                                       (set-additional-properties schema)
                                       schema)))))))

(defun llm-openai-build-tool-uses (tool-uses)
  (loop for tool-use in tool-uses
        for id = (llm-vendor-utils-tool-use-id tool-use)
        for function-name = (llm-vendor-utils-tool-use-name tool-use)
        for function-arguments = (st-json:write-json-to-string (llm-vendor-utils-tool-use-args tool-use))
        collect (st-json:jso
                 "id" id
                 "type" "function"
                 "function" (st-json:jso
                             "name" function-name
                             "arguments" function-arguments))))

(defun llm-openai-build-tool-exchange (exchange)
  (loop for tool-result in (llm-chat-prompt-exchange-tool-results exchange)
        for msg = (st-json:jso
                   "role" "tool"
                   "name" (llm-chat-prompt-tool-result-tool-name tool-result)
                   "content" (format nil "Result of tool call is ~s" (llm-chat-prompt-tool-result-result tool-result)))
        when (llm-chat-prompt-tool-result-call-id tool-result)
          do (setf (st-json:getjso "tool_call_id" msg)
                   (llm-chat-prompt-tool-result-call-id tool-result))
        collect msg))

(defun llm-openai-build-messages (prompt)
  (let (messages)
    (dolist (exchange (llm-chat-prompt-exchanges prompt) (nreverse messages))
      (if* (llm-chat-prompt-exchange-tool-results exchange)
         then (loop for msg in (llm-openai-build-tool-exchange exchange)
                    do (push msg messages))
         else (let ((msg (st-json:jso "role" (string+ (llm-chat-prompt-exchange-role exchange))))
                    (content (llm-chat-prompt-exchange-content exchange)))
                (when content
                  (if* (and (consp content)
                            (llm-vendor-utils-tool-use-p (car content)))
                     then (setf (st-json:getjso "tool_calls" msg)
                                (llm-openai-build-tool-uses content))
                     else (setf (st-json:getjso "content" msg) content)))
                (push msg messages))))))

(defmethod llm-vendor-chat-request ((vendor llm-openai) prompt streaming)
  (llm-vendor-utils-combine-to-system-prompt prompt *llm-openai-example-prelude*)
  (let ((request (st-json:jso
                  "model" (llm-openai-chat-model vendor)
                  "stream" (st-json:as-json-bool streaming))))
    ;; tmeperature
    (when (llm-chat-prompt-temperature prompt)
      (setf (st-json:getjso "temperature" request) (llm-chat-prompt-temperature prompt)))
    ;; max tokens
    (when (llm-chat-prompt-max-tokens prompt)
      (setf (st-json:getjso "max_completion_tokens" request) (llm-chat-prompt-max-tokens prompt)))
    ;; response format
    (when (llm-chat-prompt-response-format prompt)
      (setf (st-json:getjso "response_format" request)
            (llm-openai-response-format (llm-chat-prompt-response-format prompt))))
    ;; tools
    (when (llm-chat-prompt-tools prompt)
      (setf (st-json:getjso "tools" request)
            (loop for tool in (llm-chat-prompt-tools prompt)
                  collect (llm-vendor-utils-openai-tool-spec tool)))
      (setf (st-json:getjso "tool_choice" request) "required"))
    ;; messages
    (setf (st-json:getjso "messages" request)
          (llm-openai-build-messages prompt))
    ;; Merge non-standard params
    (loop for (k . v) in (llm-chat-prompt-non-standard-params prompt)
          do (setf (st-json:getjso (string+ k) request) v))
    request))

(defmethod llm-vendor-embedding-url ((vendor llm-openai))
  (llm-openai-url "embeddings"))

(defmethod llm-vendor-embedding-request ((vendor llm-openai) text)
  (st-json:jso
   "input" text
   "model" (llm-openai-embedding-model vendor)))

(defmethod llm-vendor-batch-embeddings-request ((vendor llm-openai) text-sequence)
  (st-json:jso
   "input" text-sequence
   "model" (llm-openai-embedding-model vendor)))

(defmethod llm-vendor-embedding-extract-result ((vendor llm-openai) response)
  (st-json:getjso
   "embedding"
   (first (st-json:getjso "data" response))))

(defmethod llm-vendor-batch-embeddings-extract-result ((vendor llm-openai) response)
  (loop with data = (st-json:getjso "data" response)
        with res = (make-array (length data) :element-type 't)
        for item in data
        for idx = (st-json:getjso "index" item)
        for embedding = (st-json:getjso "embedding" item)
        do (setf (svref res idx) embedding)
        finally (return (coerce res 'list))))

(defmethod llm-vendor-embedding-extract-error ((vendor llm-openai) response)
  (llm-openai-extract-error response))

(defclass llm-openai-compatible (llm-openai)
  ((endpoint :initform nil :initarg :endpoint :accessor llm-openai-compatible-endpoint))
  (:documentation "A class for other APIs that is compatible with the OpenAI's API.

`endpoint' is the `endpoint' to use for the API, up to the command. So, for
example, if the API for chat is at https://api.example.com/v1/chat, then
`endpoint' should be \"https://api.example.com/v1/\"."))

(defun llm-openai-compatible-url (vendor method)
  (flet ((suffix-p (string suffix)
           (if* (and (find-package "uiop")
                     (find-symbol "string-suffix-p" "uiop"))
              then (funcall (find-symbol "string-suffix-p" "uiop") string suffix)
              else (match-re (string+ ".*" suffix "$") string))))
    (let ((prefix (llm-openai-compatible-endpoint vendor)))
      (string+ prefix
               (when (not (suffix-p prefix "/"))
                 "/")
               method))))

(defmethod llm-vendor-chat-url ((vendor llm-openai-compatible))
  (llm-openai-compatible-url vendor "chat/completions"))

(defmethod llm-vendor-embedding-url ((vendor llm-openai-compatible))
  (llm-openai-compatible-url vendor "embeddings"))
