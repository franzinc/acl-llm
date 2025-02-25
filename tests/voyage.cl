;;;; See the file LICENSE for the full license governing this code.
(in-package #:acl-llm.protocol)

(deftestsuite llm-voyage-tests (llm-openai-compatible-tests)
  (voyage)
  (:setup (setf voyage (make-llm-voyage :key (get-dummy-api-key-string)))))

(addtest (llm-voyage-tests) test-llm-voyage-chat-not-supported-error
  (ensure-error (llm-chat voyage (make-llm-chat-prompt query))))
