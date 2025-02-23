;;;; See the file LICENSE for the full license governing this code.
(in-package #:acl-llm.protocol)

(deftestsuite llm-vendor-utils-tests (llm-tests)
  ())

(addtest (llm-vendor-utils-tests) test-llm-vendor-utils-convert-plist-to-jso
  (let (jso)
    ;; case 1
    (setq jso (llm-vendor-utils-convert-plist-to-jso '(:a 1 :b 2)))
    (ensure-same (st-json:getjso "a" jso) 1)
    (ensure-same (st-json:getjso "b" jso) 2)
    ;; case 2
    (setq jso (llm-vendor-utils-convert-plist-to-jso '(:a "1" :b foo)))
    (ensure-same (st-json:getjso "a" jso) "1")
    (ensure-same (st-json:getjso "b" jso) "foo")
    ;; case 3
    (setq jso (llm-vendor-utils-convert-plist-to-jso '(:inner (:a foo :b bar))))
    (ensure-same (st-json:getjso "a" (st-json:getjso "inner" jso)) "foo")
    (ensure-same (st-json:getjso "b" (st-json:getjso "inner" jso)) "bar")))

(addtest (llm-vendor-utils-tests) test-llm-vendor-utils-openai-arguments
  (let* ((args (list
                ;; A required string arg
                '(:name "location"
                  :type string
                  :description "The city and state, e.g. San Francisco, CA")
                ;; A string arg with an name
                '(:name "unit"
                  :type string
                  :description "The unit of temperature, either 'celsius' or 'fahrenheit'"
                  :enum ("celsius" "fahrenheit")
                  :optional t)
                '(:name "postal_codes"
                  :type array
                  :description "Specific postal codes"
                  :items (:type string)
                  :optional t)))
         (actual (llm-vendor-utils-openai-arguments args))
         (expected (st-json:jso
                    "type" "object"
                    "properties" (st-json:jso
                                  "location" (st-json:jso
                                              "type" "string"
                                              "description" "The city and state, e.g. San Francisco, CA")
                                  "unit" (st-json:jso
                                          "type" "string"
                                          "description" "The unit of temperature, either 'celsius' or 'fahrenheit'"
                                          "enum" (list "celsius" "fahrenheit"))
                                  "postal_codes" (st-json:jso
                                                  "type" "array"
                                                  "description" "Specific postal codes"
                                                  "items" (st-json:jso
                                                           "type" "string")))
                    "required" (list "location"))))
    ;; test "type"
    (ensure-same (st-json:getjso "type" actual) (st-json:getjso "type" expected))
    ;; test "properties"
    (let* ((actual-properties (st-json:getjso "properties" actual))
           (expected-properties (st-json:getjso "properties" expected))
           (actual-location (st-json:getjso "location" actual-properties))
           (expected-location (st-json:getjso "location" expected-properties))
           (actual-unit (st-json:getjso "unit" actual-properties))
           (expected-unit (st-json:getjso "unit" expected-properties))
           (actual-postal-codes (st-json:getjso "postal_codes" actual-properties))
           (expected-postal-codes (st-json:getjso "postal_codes" expected-properties)))
      ;; test "location"
      (ensure-same (st-json:getjso "type" actual-location) (st-json:getjso "type" expected-location))
      (ensure-same (st-json:getjso "description" actual-location) (st-json:getjso "description" expected-location))
      ;; test "unit
      (ensure-same (st-json:getjso "type" actual-unit) (st-json:getjso "type" expected-unit))
      (ensure-same (st-json:getjso "description" actual-unit) (st-json:getjso "description" expected-unit))
      (ensure-same (st-json:getjso "enum" actual-unit) (st-json:getjso "enum" expected-unit))
      ;; test "postal_codes"
      (ensure-same (st-json:getjso "type" actual-postal-codes) (st-json:getjso "type" expected-postal-codes))
      (ensure-same (st-json:getjso "description" actual-postal-codes) (st-json:getjso "description" expected-postal-codes))
      (ensure-same (st-json:getjso "type" (st-json:getjso "items" actual-postal-codes))
                   (st-json:getjso "type" (st-json:getjso "items" expected-postal-codes))))
    ;; test "required"
    (ensure-same (st-json:getjso "required" actual) (st-json:getjso "required" expected))))

(addtest (llm-vendor-utils-tests) llm-vendor-utils-combine-to-system-prompt
  (let* ((exchange1 (make-llm-chat-prompt-exchange :role 'user :content "Hello"))
         (example1 (cons "Request 1" "Response 1"))
         (example2 (cons "Request 2" "Response 2"))
         (prompt-for-first-request
           (llm.protocol::%make-llm-chat-prompt
            :context "Example context"
            :exchanges (list (copy-llm-chat-prompt-exchange exchange1))
            :examples (list example1 example2)))
         (prompt-with-existing-system-prompt
           (llm.protocol::%make-llm-chat-prompt
            :context "Example context"
            :exchanges (list
                        (make-llm-chat-prompt-exchange :role 'system :content "Existing system prompt.")
                        (copy-llm-chat-prompt-exchange exchange1))
            :examples (list example1 example2))))
    (llm-vendor-utils-combine-to-system-prompt prompt-for-first-request)
    (ensure (= 2 (length (llm-chat-prompt-exchanges prompt-for-first-request))))
    (ensure (equal (format nil "Example context~%Here are 2 examples of how to respond:~%~%User: Request 1~%Assistant: Response 1~%User: Request 2~%Assistant: Response 2")
                   (llm-chat-prompt-exchange-content (nth 0 (llm-chat-prompt-exchanges prompt-for-first-request)))))
    (ensure (equal "Hello" (llm-chat-prompt-exchange-content (nth 1 (llm-chat-prompt-exchanges prompt-for-first-request)))))
    (ensure-null (llm-chat-prompt-context prompt-for-first-request))
    (ensure-null (llm-chat-prompt-examples prompt-for-first-request))

    ;; On the request with the existing system prompt, it should append the new
    ;; text to the existing system prompt.
    (llm-vendor-utils-combine-to-system-prompt prompt-with-existing-system-prompt)
    (ensure (= 2 (length (llm-chat-prompt-exchanges prompt-with-existing-system-prompt))))
    (ensure (equal (format nil "Existing system prompt.~%Example context~%Here are 2 examples of how to respond:~%~%User: Request 1~%Assistant: Response 1~%User: Request 2~%Assistant: Response 2")
                   (llm-chat-prompt-exchange-content (nth 0 (llm-chat-prompt-exchanges prompt-with-existing-system-prompt)))))))

(addtest (llm-vendor-utils-tests) llm-vendor-utils-combine-to-user-prompt
  (let* ((exchange1 (make-llm-chat-prompt-exchange :role 'user :content "Hello"))
         (example1 (cons "Request 1" "Response 1"))
         (example2 (cons "Request 2" "Response 2"))
         (prompt-for-first-request
           (llm.protocol::%make-llm-chat-prompt
            :context "Example context"
            :exchanges (list (copy-llm-chat-prompt-exchange exchange1))
            :examples (list example1 example2))))
    ;; In the first request, the system prompt should be prepended to the user request.
    (llm-vendor-utils-combine-to-user-prompt prompt-for-first-request)
    (ensure (= 1 (length (llm-chat-prompt-exchanges prompt-for-first-request))))
    (ensure-null (llm-chat-prompt-context prompt-for-first-request))
    (ensure-null (llm-chat-prompt-examples prompt-for-first-request))
    (ensure (equal (format nil "Example context~%Here are 2 examples of how to respond:~%~%User: Request 1~%Assistant: Response 1~%User: Request 2~%Assistant: Response 2~%Hello")
                   (llm-chat-prompt-exchange-content (nth 0 (llm-chat-prompt-exchanges prompt-for-first-request)))))))

(addtest (llm-vendor-utils-tests) llm-vendor-utils-collapse-history
  (let* ((exchange1 (make-llm-chat-prompt-exchange :role 'user :content "Hello"))
         (exchange2 (make-llm-chat-prompt-exchange :role 'assistant :content "Hi! How can I assist you?"))
         (exchange3 (make-llm-chat-prompt-exchange :role 'assistant :content "Earl Grey, hot."))
         (prompt-for-first-request
           (llm.protocol::%make-llm-chat-prompt
            :exchanges (list (copy-llm-chat-prompt-exchange exchange1))))
         (prompt-for-second-request
           (llm.protocol::%make-llm-chat-prompt
            :exchanges (list (copy-llm-chat-prompt-exchange exchange1)
                             (copy-llm-chat-prompt-exchange exchange2)
                             (copy-llm-chat-prompt-exchange exchange3)))))
    ;; In the first request, there's no history, so nothing should be done.
    (llm-vendor-utils-collapse-history prompt-for-first-request)
    (ensure (= 1 (length (llm-chat-prompt-exchanges prompt-for-first-request))))
    (ensure (equalp exchange1 (nth 0 (llm-chat-prompt-exchanges prompt-for-first-request))))

    ;; In the second request we ensure have the history prepended.
    (llm-vendor-utils-collapse-history prompt-for-second-request)
    (ensure (= 1 (length (llm-chat-prompt-exchanges prompt-for-first-request))))
    (ensure (equal (format nil "Previous exchanges:~%~%User: Hello~%Assistant: Hi! How can I assist you?~%~%The current conversation follows:~%~%Earl Grey, hot.")
                   (llm-chat-prompt-exchange-content (nth 0 (llm-chat-prompt-exchanges prompt-for-second-request)))))))
