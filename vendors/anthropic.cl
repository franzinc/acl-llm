;;;; See the file LICENSE for the full license governing this code.
(defpackage #:acl-llm.protocol
  (:export #:llm-claude
           #:make-llm-claude
           #:llm-claude-key
           #:llm-claude-chat-model))

(in-package #:acl-llm.protocol)

(defconstant +llm-anthropic-version+ "2023-06-01"
  "See documentation here: https://docs.anthropic.com/en/api/versioning")

(defconstant +llm-claude-chat-url+ "https://api.anthropic.com/v1/messages")

(defconstant +llm-claude-default-chat-model+ "claude-3-7-sonnet-latest"
  "See documentation here: https://docs.anthropic.com/en/docs/about-claude/models/all-models")

(defclass llm-claude (llm-standard-chat-vendor)
  ((key :initarg :key :accessor llm-claude-key :type (or function simple-string))
   (chat-model :initform +llm-claude-default-chat-model+
               :initarg :chat-model
               :accessor llm-claude-chat-model
               :type simple-string)))

(defun make-llm-claude (&key key (chat-model +llm-claude-default-chat-model+) &allow-other-keys)
  (make-instance 'llm-claude :key key
                             :chat-model chat-model))

(defmethod llm-capabilities ((vendor llm-claude))
  (list :streaming :tool-uses))

(defmethod llm-chat-token-limit ((vendor llm-claude))
  (llm-vendor-utils-model-token-limit (llm-claude-chat-model vendor)))

(defmethod llm-name ((vendor llm-claude))
  "Return the name of the provider."
  "Claude")

(defun llm-claude-api-key (claude)
  (when (not (slot-boundp claude 'key))
    (error "To call Claude API, please add an API key to ~a" claude))
  (let ((key (llm-claude-key claude)))
    (if* (stringp key)
       then key
     elseif (functionp key)
       then (funcall key)
       else (error "API key must either be a string or a function that takes no argument, but got ~a" key))))

(defmethod llm-vendor-headers ((vendor llm-claude))
  (list (cons "x-api-key" (llm-claude-api-key vendor))
        (cons "anthropic-version" +llm-anthropic-version+)))

(defmethod llm-vendor-chat-url ((vendor llm-claude))
  "Return the URL for the Claude API."
  +llm-claude-chat-url+)

(defmethod llm-vendor-chat-extract-error ((vendor llm-claude) response)
  (when (st-json:getjso "error" response)
    (let ((err (st-json:getjso "error" response)))
      (format nil "Error ~s: ~s"
              (st-json:getjso "type" err)
              (st-json:getjso "message" err)))))

(defmethod llm-vendor-chat-extract-result ((vendor llm-claude) response)
  (let ((content (first (st-json:getjso "content" response))))
    (if* (string= (st-json:getjso "type" content) "text")
       then (st-json:getjso "text" content)
       else (error "Unsupported non-text response: ~s" content))))

(defmethod llm-vendor-populate-tool-uses ((vendor llm-claude) prompt tool-uses)
  (llm-vendor-utils-append-to-prompt
   prompt
   (loop for call in tool-uses
         collect (st-json:jso
                  "type" "tool_use"
                  "id" (llm-vendor-utils-tool-use-id call)
                  "name" (llm-vendor-utils-tool-use-name call)
                  "input" (llm-vendor-utils-tool-use-args call)))))

(defmethod llm-vendor-extract-tool-uses ((vendor llm-claude) response)
  (loop with content = (st-json:getjso "content" response)
        for item in content
        when (and (st-json:getjso "type" item)
                  (string= (st-json:getjso "type" item) "tool_use"))
          collect (make-llm-vendor-utils-tool-use
                   :id (st-json:getjso "id" item)
                   :name (st-json:getjso "name" item)
                   :args (st-json:getjso "input" item))))

(defun llm-claude-build-messages (prompt)
  (loop for exchange in (llm-chat-prompt-exchanges prompt)
        for role = (ecase (llm-chat-prompt-exchange-role exchange)
                     (tool_results "user")
                     (tool_use     "assistant")
                     (assistant    "assistant")
                     (user         "user"))
        for content = (if* (llm-chat-prompt-exchange-tool-results exchange)
                         then (loop for result in (llm-chat-prompt-exchange-tool-results exchange)
                                    collect (st-json:jso
                                             "type" "tool_result"
                                             "tool_use_id" (llm-chat-prompt-tool-result-call-id result)
                                             "content" (string+ "Result of tool call is "
                                                                (llm-chat-prompt-tool-result-result result))))
                         else (llm-chat-prompt-exchange-content exchange))
        collect (st-json:jso "role" role
                             "content" content)))

(defmethod llm-vendor-chat-request ((vendor llm-claude) prompt stream)
  (let ((request (st-json:jso
                  "model" (llm-claude-chat-model vendor)
                  "stream" (st-json:as-json-bool stream)
                  "max_tokens" (or (llm-chat-prompt-max-tokens prompt) 4096) ; Claude requires max_tokens
                  "messages" (llm-claude-build-messages prompt)))
        (system (llm-vendor-utils-get-system-prompt prompt)))
    ;; tool calls
    (when (llm-chat-prompt-tools prompt)
      (setf (st-json:getjso "tools" request)
            (loop for tool in (llm-chat-prompt-tools prompt)
                  collect (st-json:jso
                           "name" (llm-tool-name tool)
                           "description" (llm-tool-description tool)
                           "input_schema" (llm-vendor-utils-openai-arguments
                                           (llm-tool-args tool))))))
    ;; system
    (when (> (length system) 0)
      (setf (st-json:getjso "system" request) system))
    ;; temperature
    (when (llm-chat-prompt-temperature prompt)
      (setf (st-json:getjso "temperature" request) (llm-chat-prompt-temperature prompt)))
    ;; non-standard parameters
    (loop for (k . v) in (llm-chat-prompt-non-standard-params prompt)
          do (setf (st-json:getjso k request) v))
    request))

(defmethod llm-vendor-append-to-prompt ((vendor llm-claude) prompt result &optional tool-use-results)
  ;; Claude doesn't have a 'function role, so we just always use assistant here.
  ;; But if it's a function result, it considers that a 'user response, which
  ;; needs to be sent back.
  (llm-vendor-utils-append-to-prompt
   prompt
   result
   tool-use-results
   (if tool-use-results 'user 'assistant)))
