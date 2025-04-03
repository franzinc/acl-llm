;;;; See the file LICENSE for the full license governing this code.
(in-package #:acl-llm.protocol)

(deftestsuite llm-openai-tests (llm-vendor-tests)
  (openai)
  (:setup (setf openai (make-llm-openai :key (get-dummy-api-key-string)))))

(addtest (llm-openai-tests) test-llm-openai-default-chat-model
  (ensure-same (llm-openai-chat-model openai)
               acl-llm.protocol::+llm-openai-default-chat-model+))

(addtest (llm-openai-tests) test-llm-openai-default-embedding-model
  (ensure-same (llm-openai-embedding-model openai)
               acl-llm.protocol::+llm-openai-default-embedding-model+))

(defun test-llm-openai-headers (openai actual-key)
  (let* ((headers (llm-vendor-headers openai))
         (auth (assoc "Authorization" headers :test 'string-equal))
         (expected-key (subseq (cdr auth) #.(length "Bearer "))))
    (ensure-same actual-key expected-key)))

(addtest (llm-openai-tests) test-llm-openai-headers-api-key-given-as-plain-text
  (let ((actual (get-dummy-api-key-string)))
    (setf (llm-openai-key openai) actual)
    (test-llm-openai-headers openai actual)))

(addtest (llm-openai-tests) test-llm-openai-headers-api-key-given-as-a-function
  (let ((actual (get-dummy-api-key-function)))
    (setf (llm-openai-key openai) actual)
    (test-llm-openai-headers openai (funcall actual))))

(addtest (llm-openai-tests) test-llm-openai-headers-missing-api-key
  (setf openai (make-llm-openai))
  (ensure-error (llm-vendor-headers openai)))

(addtest (llm-openai-tests) test-llm-openai-headers-invalid-api-key
  (setf (llm-openai-key openai) 'foo)
  (ensure-error (llm-vendor-headers openai)))

(addtest (llm-openai-tests) test-llm-openai-chat-url
  (ensure-same (llm-vendor-chat-url openai)
               (string+ acl-llm.protocol::+llm-openai-endpoint-url+ "chat/completions")))

(addtest (llm-openai-tests) test-llm-openai-embedding-url
  (ensure-same (llm-vendor-embedding-url openai)
               (string+ acl-llm.protocol::+llm-openai-endpoint-url+ "embeddings")))

(addtest (llm-openai-tests) test-llm-openai-chat
  (let ((answer "The sky is blue because it is the color of the sky."))
    ;; mocking `llm-request-sync'
    (def-fwrapper mock-llm-request-sync (url &key headers content timeout)
      (declare (ignore url headers timeout))
      (ensure-same query
                   (st-json:getjso
                    "content"
                    (first
                     (st-json:getjso
                      "messages"
                      (st-json:read-json-from-string content)))))
      ;; mocked response
      (st-json:jso
       "choices" (list (st-json:jso
                        "message" (st-json:jso
                                   "content" answer)))))
    (fwrap 'llm-request-sync 'mocked-llm-request-sync 'mock-llm-request-sync)
    (unwind-protect (ensure-same (llm-chat openai (make-llm-chat-prompt query))
                                 answer)
      (funwrap 'llm-request-sync 'mocked-llm-request-sync))))

(addtest (llm-openai-tests) test-llm-openai-chat-error
  (let ((err (st-json:jso
              "type" "test-llm-chat-error-type"
              "code" "test-llm-chat-error-code"
              "message" "test-llm-chat-error-message")))
    ;; mocking `llm-request-sync'
    (def-fwrapper mock-llm-request-sync (url &key headers content timeout)
      (declare (ignore url headers content timeout))
      ;; mocked response
      (st-json:jso "error" err))
    (fwrap 'llm-request-sync 'mocked-llm-request-sync 'mock-llm-request-sync)
    (unwind-protect (ensure-error (llm-chat openai (make-llm-chat-prompt query)))
      (funwrap 'llm-request-sync 'mocked-llm-request-sync))))

(addtest (llm-openai-tests) test-llm-openai-chat-tool-uses
  (let* ((tool-name "add-two-numbers")
         (tool-description "Add two numbers")
         (tool (make-llm-tool :function #'(lambda (x y) (+ x y))
                              :name tool-name
                              :description tool-description
                              :args '((:name "x" :type :integer)
                                      (:name "y" :type :integer))))
         (x (random 10))
         (y (random 10))
         (answer (list (cons "add-two-numbers"  (+ x y)))))
    ;; mocking `llm-request-sync'
    (def-fwrapper mock-llm-request-sync (url &key headers content timeout)
      (declare (ignore url headers timeout))
      (let* ((payload (st-json:read-json-from-string content))
             (tools (st-json:getjso "tools" payload))
             (tool (first tools))
             (function (st-json:getjso "function" tool))
             (parameters (st-json:getjso "parameters" function))
             (properties (st-json:getjso "properties" parameters))
             (arg-x (st-json:getjso "x" properties))
             (arg-y (st-json:getjso "y" properties))
             (required (st-json:getjso "required" parameters)))
        (ensure-same "function" (st-json:getjso "type" tool))
        (ensure-same tool-name (st-json:getjso "name" function))
        (ensure-same tool-description (st-json:getjso "description" function))
        (ensure-same "integer" (st-json:getjso "type" arg-x))
        (ensure-same "integer" (st-json:getjso "type" arg-y))
        (ensure (and (member "x" required :test 'string=) (member "y" required :test 'string=)))
        ;; mocked response
        (st-json:jso
         "choices" (list (st-json:jso
                          "message" (st-json:jso
                                     "tool_calls" (list (st-json:jso
                                                         "function" (st-json:jso
                                                                     "name" tool-name
                                                                     "arguments" (st-json:write-json-to-string
                                                                                  (st-json:jso "x" x "y" y)))))))))))
    (fwrap 'llm-request-sync 'mocked-llm-request-sync 'mock-llm-request-sync)
    (unwind-protect (ensure-same (llm-chat openai (make-llm-chat-prompt query :tools (list tool)))
                                 answer)
      (funwrap 'llm-request-sync 'mocked-llm-request-sync))))

(addtest (llm-openai-tests) test-llm-openai-response-format-json-mode
  (ensure-same-jso (acl-llm.protocol::llm-openai-response-format :json)
                   (st-json:jso "type" "json_object")))

(addtest (llm-openai-tests) test-llm-openai-response-format-invalid-top-level-type
  (ensure-error (acl-llm.protocol::llm-openai-response-format '(:type "foo"))))

(addtest (llm-openai-tests) tes-llm-openai-response-format-set-additional-properties
  (let* ((format '(:type "object"
                   :properties (:x (:type "number"))
                   :required #("x")))
         (response-format (acl-llm.protocol::llm-openai-response-format format))
         (schema (st-json:getjso "schema" (st-json:getjso "json_schema" response-format))))
    (ensure-null (st-json:from-json-bool (st-json:getjso "additionalProperties" schema)))))

(addtest (llm-openai-tests) tes-llm-openai-response-format-set-additional-properties-in-nested-schema
  (let* ((format '(:type "object"
                   :properties (:outer (:type "array"
                                        :items (:type "object"
                                                :properties (:inner (:type "string"))
                                                :required #("inner"))))
                   :required #("outer")))
         (response-format (acl-llm.protocol::llm-openai-response-format format))
         (outer-schema (st-json:getjso "schema" (st-json:getjso "json_schema" response-format)))
         (inner-schema (st-json:getjso
                        "items"
                        (st-json:getjso
                         "outer"
                         (st-json:getjso "properties" outer-schema)))))
    (ensure-null (st-json:from-json-bool (st-json:getjso "additionalProperties" outer-schema)))
    (ensure-null (st-json:from-json-bool (st-json:getjso "additionalProperties" inner-schema)))))

(addtest (llm-openai-tests) test-llm-openai-chat-response-format
  (let ((query "List one Sci-Fi and Horror book. Respond using JSON")
        (response-format '(:type "object"
                           :properties (:answers (:type "array"
                                                  :items (:type "object"
                                                          :properties (:author (:type "string")
                                                                       :published (:type "number")
                                                                       :title (:type "string")
                                                                       :ISBN (:type "string"))
                                                          :required #("author" "title" "published" "ISBN"))))
                           :required #("answers"))))
    ;; mocking `llm-request-sync'
    (def-fwrapper mock-llm-request-sync (url &key headers content timeout)
      (declare (ignore url headers timeout))
      (let* ((actual-payload (st-json:read-json-from-string content))
             (expected-payload (st-json:read-json-from-string "{
  \"messages\": [
    {
      \"content\": \"List one Sci-Fi and Horror book. Respond using JSON\",
      \"role\": \"user\"
    }
  ],
  \"response_format\": {
    \"type\": \"json_schema\",
    \"json_schema\": {
      \"name\": \"response\",
      \"strict\": true,
      \"schema\": {
        \"additionalProperties\": false,
        \"required\": [
          \"answers\"
        ],
        \"properties\": {
          \"answers\": {
            \"items\": {
              \"additionalProperties\": false,
              \"required\": [
                \"author\",
                \"title\",
                \"published\",
                \"ISBN\"
              ],
              \"properties\": {
                \"ISBN\": {
                  \"type\": \"string\"
                },
                \"title\": {
                  \"type\": \"string\"
                },
                \"published\": {
                  \"type\": \"number\"
                },
                \"author\": {
                  \"type\": \"string\"
                }
              },
              \"type\": \"object\"
            },
            \"type\": \"array\"
          }
        },
        \"type\": \"object\"
      }
    }
  },
  \"model\": \"gpt-4o-mini\",
  \"stream\": false
}")))
        (ensure-same-jso actual-payload expected-payload))
      ;; mocked response
      (st-json:jso
       "choices" (list (st-json:jso
                        "message" (st-json:jso "content" "{
  \"answers\": [
    {
      \"ISBN\": \"978-0743275244\",
      \"title\": \"The Shining\",
      \"published\": 1977,
      \"author\": \"Stephen King\"
    }
  ]
}")))))
    (fwrap 'llm-request-sync 'mocked-llm-request-sync 'mock-llm-request-sync)
    (unwind-protect (let* ((jso (st-json:read-json-from-string
                                 (llm-chat openai (make-llm-chat-prompt query :response-format response-format))))
                           (book (first (st-json:getjso "answers" jso))))
                      (ensure-same "978-0743275244" (st-json:getjso "ISBN" book))
                      (ensure-same "The Shining"    (st-json:getjso "title" book))
                      (ensure-same 1977             (st-json:getjso "published" book))
                      (ensure-same "Stephen King"   (st-json:getjso "author" book)))
      (funwrap 'llm-request-sync 'mocked-llm-request-sync))))

(addtest (llm-openai-tests) test-llm-openai-embedding
  (let* ((*llm-embedding-default-float-format* (nth (random 2) '(single-float double-float)))
         (dim 4096)
         (embedding (get-dummy-embedding dim)))
    ;; mocking `llm-request-sync'
    (def-fwrapper mock-llm-request-sync (url &key headers content timeout)
      (declare (ignore headers timeout))
      (ensure-same url (llm-vendor-embedding-url openai))
      (ensure-same query
                   (st-json:getjso
                    "input"
                    (st-json:read-json-from-string content)))
      ;; mocked response
      (st-json:jso
       "data" (list (st-json:jso
                     "embedding" embedding))))
    (fwrap 'llm-request-sync 'mocked-llm-request-sync 'mock-llm-request-sync)
    (unwind-protect (ensure-same (llm-embedding openai query)
                                 embedding)
      (funwrap 'llm-request-sync 'mocked-llm-request-sync))))

(addtest (llm-openai-tests) test-llm-openai-embedding-error
  (let ((err (st-json:jso
              "type" "test-llm-embedding-error-type"
              "code" "test-llm-embedding-error-code"
              "message" "test-llm-embedding-error-message")))
    ;; mocking `llm-request-sync'
    (def-fwrapper mock-llm-request-sync (url &key headers content timeout)
      (declare (ignore url headers content timeout))
      ;; mocked response
      (st-json:jso "error" err))
    (fwrap 'llm-request-sync 'mocked-llm-request-sync 'mock-llm-request-sync)
    (unwind-protect (ensure-error (llm-embedding openai query))
      (funwrap 'llm-request-sync 'mocked-llm-request-sync))))

(addtest (llm-openai-tests) test-llm-openai-batch-embeddings
  (let* ((queries '("Why is sky blue?"
                    "Why is water wet?"
                    "Why did Judas rat to Romans while Jesus slept?"))
         (*llm-embedding-default-float-format* (nth (random 2) '(single-float double-float)))
         (dim 4096)
         (embeddings (loop for i from 1 to (length queries)
                           collect (get-dummy-embedding dim))))
    ;; mocking `llm-request-sync'
    (def-fwrapper mock-llm-request-sync (url &key headers content timeout)
      (declare (ignore headers timeout))
      (ensure-same queries
                   (st-json:getjso
                    "input"
                    (st-json:read-json-from-string content)))
      ;; mocked response
      (st-json:jso
       "data"
       ;; OpenAI doesn't return embeddings in order,
       ;; but return embedding with its index instead.
       ;; So we will shuffle the embeddings for this test
       ;; to have some randomness.
       (loop with indices = (triple-store-tests::shuffle!
                             (loop for i from 0 below (length queries)
                                   collect i))
             for i in indices
             for embedding = (nth i embeddings)
             collect (st-json:jso
                      "embedding" embedding
                      "index" i))))
    (fwrap 'llm-request-sync 'mocked-llm-request-sync 'mock-llm-request-sync)
    (unwind-protect (ensure-same (llm-batch-embeddings openai queries)
                                 embeddings)
      (funwrap 'llm-request-sync 'mocked-llm-request-sync))))

(defclass .openai-compatible-dummy. (llm-openai-compatible)
  ())

(deftestsuite llm-openai-compatible-tests (llm-vendor-tests)
  (dummy
   (endpoint "http://api.dummy.com"))
  (:setup (setf dummy (make-instance '.openai-compatible-dummy. :endpoint endpoint))))

(addtest (llm-openai-compatible-tests) test-llm-openai-compatible-chat-url-without-last-slash
  (ensure-same (llm-vendor-chat-url dummy)
               (string+ endpoint #\/ "chat/completions")))

(addtest (llm-openai-compatible-tests) test-llm-openai-compatible-chat-url-with-last-slash
  (setf (llm-openai-compatible-endpoint dummy) (string+ endpoint #\/))
  (ensure-same (llm-vendor-chat-url dummy)
               (string+ endpoint #\/ "chat/completions")))

(addtest (llm-openai-compatible-tests) test-llm-openai-compatible-embedding-url-without-last-slash
  (ensure-same (llm-vendor-embedding-url dummy)
               (string+ endpoint #\/ "embeddings")))

(addtest (llm-openai-compatible-tests) test-llm-openai-compatible-embedding-url-with-last-slash
  (setf (llm-openai-compatible-endpoint dummy) (string+ endpoint #\/))
  (ensure-same (llm-vendor-embedding-url dummy)
               (string+ endpoint #\/ "embeddings")))
