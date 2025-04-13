;;;; See the file LICENSE for the full license governing this code.
(in-package #:acl-llm.protocol)

(deftestsuite llm-claude-tests (llm-vendor-tests)
  (claude)
  (:setup (setf claude (make-llm-claude :key (get-dummy-api-key-string)))))

(defun test-llm-claude-headers (claude actual-key)
  (let* ((headers (llm-vendor-headers claude))
         (auth (assoc "x-api-key" headers :test 'string-equal))
         (expected-key (cdr auth)))
    (ensure-same actual-key expected-key)))

(addtest (llm-claude-tests) test-llm-claude-headers-api-key-given-as-plain-text
  (let ((actual (get-dummy-api-key-string)))
    (setf (llm-claude-key claude) actual)
    (test-llm-claude-headers claude actual)))

(addtest (llm-claude-tests) test-llm-claude-headers-api-key-given-as-a-function
  (let ((actual (get-dummy-api-key-function)))
    (setf (llm-claude-key claude) actual)
    (test-llm-claude-headers claude (funcall actual))))

(addtest (llm-claude-tests) test-llm-claude-headers-missing-api-key
  (setf claude (make-llm-claude))
  (ensure-error (llm-vendor-headers claude)))

(addtest (llm-claude-tests) test-llm-claude-headers-invalid-api-key
  (setf (llm-claude-key claude) 'foo)
  (ensure-error (llm-vendor-headers claude)))

(addtest (llm-claude-tests) test-llm-claude-chat-url
  (ensure-same (llm-vendor-chat-url claude)
               acl-llm.protocol::+llm-claude-chat-url+))

(addtest (llm-claude-tests) test-llm-claude-chat-prompt-temperature
  (let* ((st-json:*json-read-default-float-format* 'single-float)
         (temperature (random 1.0))
         (prompt (make-llm-chat-prompt query :temperature temperature))
         (request (llm-vendor-chat-request claude prompt nil))
         (jso (st-json:read-json-from-string request)))
    (ensure (< (abs (- temperature
                       (st-json:getjso "temperature" jso)))
               1e-5))))

(addtest (llm-claude-tests) test-llm-claude-chat
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
       "type" "message"
       "content" (list (st-json:jso
                        "type" "text"
                        "text" answer))))
    (fwrap 'llm-request-sync 'mocked-llm-request-sync 'mock-llm-request-sync)
    (unwind-protect (ensure-same (llm-chat claude (make-llm-chat-prompt query))
                                 answer)
      (funwrap 'llm-request-sync 'mocked-llm-request-sync))))

(addtest (llm-claude-tests) test-llm-claude-chat-error
  (let ((err (st-json:jso
              "type" "test-llm-chat-error-type"
              "message" "test-llm-chat-error-message")))
    ;; mocking `llm-request-sync'
    (def-fwrapper mock-llm-request-sync (url &key headers content timeout)
      (declare (ignore url headers content timeout))
      ;; mocked response
      (st-json:jso "error" err))
    (fwrap 'llm-request-sync 'mocked-llm-request-sync 'mock-llm-request-sync)
    (unwind-protect (ensure-error (llm-chat claude (make-llm-chat-prompt query)))
      (funwrap 'llm-request-sync 'mocked-llm-request-sync))))

(addtest (llm-claude-tests) test-llm-claude-chat-tool-uses
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
             (input-schema (st-json:getjso "input_schema" tool))
             (properties (st-json:getjso "properties" input-schema))
             (arg-x (st-json:getjso "x" properties))
             (arg-y (st-json:getjso "y" properties))
             (required (st-json:getjso "required" input-schema)))
        (ensure-same tool-name (st-json:getjso "name" tool))
        (ensure-same tool-description (st-json:getjso "description" tool))
        (ensure-same "integer" (st-json:getjso "type" arg-x))
        (ensure-same "integer" (st-json:getjso "type" arg-y))
        (ensure (and (member "x" required :test 'string=) (member "y" required :test 'string=)))
        ;; mocked response
        (st-json:jso
         "role" "assistant"
         "content" (list (st-json:jso
                          "type" "text"
                          "text": "I can help you")
                         (st-json:jso
                          "type" "tool_use"
                          "id" "id-test-llm-claude-chat-tool-uses"
                          "name" tool-name
                          "input" (st-json:jso "x" x "y" y))))))
    (fwrap 'llm-request-sync 'mocked-llm-request-sync 'mock-llm-request-sync)
    (unwind-protect (ensure-same (llm-chat claude (make-llm-chat-prompt query :tools (list tool)))
                                 answer)
      (funwrap 'llm-request-sync 'mocked-llm-request-sync))))
