;;;; See the file LICENSE for the full license governing this code.
(in-package #:acl-llm.protocol)

(deftestsuite llm-ollama-tests (llm-vendor-tests)
  (ollama
   (query "Why is sky blue?"))
  (:setup (setf ollama (make-llm-ollama :chat-model nil :embedding-model nil))))

(addtest (llm-ollama-tests) test-llm-ollama-chat-url
  (ensure-same (llm-vendor-chat-url ollama)
               "http://localhost:11434/api/chat")

  (setf (llm-ollama-scheme ollama) "https")
  (ensure-same (llm-vendor-chat-url ollama)
               "https://localhost:11434/api/chat")

  (setf (llm-ollama-scheme ollama) "http")
  (setf (llm-ollama-host ollama) "franz.com")
  (ensure-same (llm-vendor-chat-url ollama)
               "http://franz.com:11434/api/chat")

  (setf (llm-ollama-scheme ollama) "http")
  (setf (llm-ollama-host ollama) "localhost")
  (setf (llm-ollama-port ollama) "8080")
  (ensure-same (llm-vendor-chat-url ollama)
               "http://localhost:8080/api/chat"))

(addtest (llm-ollama-tests) test-llm-ollama-chat-timeout
  (let ((*llm-ollama-chat-timeout* (random (1+ 300))))
    (ensure-same (llm-vendor-chat-timeout ollama)
                 *llm-ollama-chat-timeout*)))

(addtest (llm-ollama-tests) test-llm-ollama-chat-prompt-temperature
  (let* ((st-json:*json-read-default-float-format* 'single-float)
         (temperature (random 1.0))
         (prompt (make-llm-chat-prompt query :temperature temperature))
         (request (llm-vendor-chat-request ollama prompt nil))
         (jso (st-json:read-json-from-string request)))
    (ensure (< (abs (- temperature
                       (st-json:getjso "temperature"
                                       (st-json:getjso "options" jso))))
               1e-5))))

(addtest (llm-ollama-tests) test-llm-ollama-chat
  (let ((answer "The sky is blue because it is the color of the sky."))
    ;; mocking `llm-request-sync'
    (def-fwrapper mock-llm-request-sync (url &key headers content timeout)
      (ensure-same url (llm-vendor-chat-url ollama))
      (ensure-null headers)
      (ensure-same query
                   (st-json:getjso
                    "content"
                    (first
                     (st-json:getjso
                      "messages"
                      (st-json:read-json-from-string content)))))
      (ensure-same timeout (llm-vendor-chat-timeout ollama))
      ;; mocked response
      (st-json:jso
       "message" (st-json:jso
                  "content" answer)))
    (fwrap 'llm-request-sync 'mocked-llm-request-sync 'mock-llm-request-sync)
    (unwind-protect (ensure-same (llm-chat ollama (make-llm-chat-prompt query))
                                 answer)
      (funwrap 'llm-request-sync 'mocked-llm-request-sync))))

(addtest (llm-ollama-tests) test-llm-ollama-chat-error
  (let ((err "test-llm-chat-error"))
    ;; mocking `llm-request-sync'
    (def-fwrapper mock-llm-request-sync (url &key headers content timeout)
      (declare (ignore url headers content timeout))
      ;; mocked response
      (st-json:jso "error" err))
    (fwrap 'llm-request-sync 'mocked-llm-request-sync 'mock-llm-request-sync)
    (unwind-protect (ensure-error (llm-chat ollama (make-llm-chat-prompt query)))
      (funwrap 'llm-request-sync 'mocked-llm-request-sync))))

(addtest (llm-ollama-tests) test-llm-ollama-chat-tool-uses
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
         "message" (st-json:jso
                    "tool_calls" (list (st-json:jso
                                        "function" (st-json:jso
                                                    "name" tool-name
                                                    "arguments" (st-json:jso "x" x "y" y))))))))
    (fwrap 'llm-request-sync 'mocked-llm-request-sync 'mock-llm-request-sync)
    (unwind-protect (ensure-same (llm-chat ollama (make-llm-chat-prompt query :tools (list tool)))
                                 answer)
      (funwrap 'llm-request-sync 'mocked-llm-request-sync))))

(addtest (llm-ollama-tests) test-llm-ollama-chat-response-format
  (let ((query "List one Sci-Fi and Horror book. Respond using JSON")
        (response-format '(:type "array"
                           :items (:type "object"
                                   :properties (:author (:type "string")
                                                :published (:type "number")
                                                :title (:type "string")
                                                :ISBN (:type "string"))
                                   :required #("author" "title" "published" "ISBN")))))
    ;; mocking `llm-request-sync'
    (def-fwrapper mock-llm-request-sync (url &key headers content timeout)
      (declare (ignore url headers timeout))
      (let* ((payload (st-json:read-json-from-string content))
             (format (st-json:getjso "format" payload))
             (items (st-json:getjso "items" format))
             (properties (st-json:getjso "properties" items))
             (required (st-json:getjso "required" items)))
        (ensure-same "array" (st-json:getjso "type" format))
        (ensure-same "object" (st-json:getjso "type" items))
        (ensure-same "string" (st-json:getjso "type" (st-json:getjso "author" properties)))
        (ensure-same "number" (st-json:getjso "type" (st-json:getjso "published" properties)))
        (ensure-same "string" (st-json:getjso "type" (st-json:getjso "title" properties)))
        (ensure-same "string" (st-json:getjso "type" (st-json:getjso "ISBN" properties)))
        (dolist (k '("author" "title" "published" "ISBN"))
          (ensure (member k required :test 'string=))))
      ;; mocked response
      (st-json:jso
       "message" (st-json:jso "content" "[
  {
    \"ISBN\": \"978-0743275244\",
    \"title\": \"The Shining\",
    \"published\": 1977,
    \"author\": \"Stephen King\"
  }
]")))
    (fwrap 'llm-request-sync 'mocked-llm-request-sync 'mock-llm-request-sync)
    (unwind-protect (let* ((jso (st-json:read-json-from-string
                                 (llm-chat ollama (make-llm-chat-prompt query :response-format response-format))))
                           (book (first jso)))
                      (ensure-same "978-0743275244" (st-json:getjso "ISBN" book))
                      (ensure-same "The Shining"    (st-json:getjso "title" book))
                      (ensure-same 1977             (st-json:getjso "published" book))
                      (ensure-same "Stephen King"   (st-json:getjso "author" book)))
      (funwrap 'llm-request-sync 'mocked-llm-request-sync))))

(addtest (llm-ollama-tests) test-llm-ollama-embedding-url
  (ensure-same (llm-vendor-embedding-url ollama)
               "http://localhost:11434/api/embed")
  (ensure-same (llm-vendor-embedding-url ollama)
               "http://localhost:11434/api/embed")

  (setf (llm-ollama-scheme ollama) "https")
  (ensure-same (llm-vendor-embedding-url ollama)
               "https://localhost:11434/api/embed")

  (setf (llm-ollama-scheme ollama) "http")
  (setf (llm-ollama-host ollama) "franz.com")
  (ensure-same (llm-vendor-embedding-url ollama)
               "http://franz.com:11434/api/embed")

  (setf (llm-ollama-scheme ollama) "http")
  (setf (llm-ollama-host ollama) "localhost")
  (setf (llm-ollama-port ollama) "8080")
  (ensure-same (llm-vendor-embedding-url ollama)
               "http://localhost:8080/api/embed"))

(addtest (llm-ollama-tests) test-llm-ollama-embedding
  (let* ((*llm-embedding-default-float-format* (nth (random 2) '(single-float double-float)))
         (dim 4096)
         (embedding (get-dummy-embedding dim)))
    ;; mocking `llm-request-sync'
    (def-fwrapper mock-llm-request-sync (url &key headers content timeout)
      (ensure-same url (llm-vendor-embedding-url ollama))
      (ensure-null headers)
      (ensure-same query
                   (st-json:getjso
                    "input"
                    (st-json:read-json-from-string content)))
      (ensure-same timeout *llm-ollama-chat-timeout*)
      ;; mocked response
      (st-json:jso
       "embeddings" (list embedding)))
    (fwrap 'llm-request-sync 'mocked-llm-request-sync 'mock-llm-request-sync)
    (unwind-protect (ensure-same (llm-embedding ollama query)
                                 embedding)
      (funwrap 'llm-request-sync 'mocked-llm-request-sync))))

(addtest (llm-ollama-tests) test-llm-ollama-embedding-error
  (let ((err "test-llm-embedding-error"))
    ;; mocking `llm-request-sync'
    (def-fwrapper mock-llm-request-sync (url &key headers content timeout)
      (declare (ignore url headers content timeout))
      ;; mocked response
      (st-json:jso "error" err))
    (fwrap 'llm-request-sync 'mocked-llm-request-sync 'mock-llm-request-sync)
    (unwind-protect (ensure-error (llm-embedding ollama query))
      (funwrap 'llm-request-sync 'mocked-llm-request-sync))))

(addtest (llm-ollama-tests) test-llm-ollama-batch-embeddings
  (let* ((queries '("Why is sky blue?"
                    "Why is water wet?"
                    "Why did Judas rat to Romans while Jesus slept?"))
         (*llm-embedding-default-float-format* (nth (random 2) '(single-float double-float)))
         (dim 4096)
         (embeddings (loop for i from 1 to (length queries)
                           collect (get-dummy-embedding dim))))
    ;; mocking `llm-request-sync'
    (def-fwrapper mock-llm-request-sync (url &key headers content timeout)
      (ensure-same url (llm-vendor-embedding-url ollama))
      (ensure-null headers)
      (ensure-same queries
                   (st-json:getjso
                    "input"
                    (st-json:read-json-from-string content)))
      (ensure-same timeout *llm-ollama-chat-timeout*)
      ;; mocked response
      (st-json:jso
       "embeddings" embeddings))
    (fwrap 'llm-request-sync 'mocked-llm-request-sync 'mock-llm-request-sync)
    (unwind-protect (ensure-same (llm-batch-embeddings ollama queries)
                                 embeddings)
      (funwrap 'llm-request-sync 'mocked-llm-request-sync))))
