;;;; See the file LICENSE for the full license governing this code.
(in-package #:acl-llm.protocol)

(deftestsuite llm-deepseek-tests (llm-openai-compatible-tests)
  (deepseek)
  (:setup (setf deepseek (make-llm-deepseek :key (get-dummy-api-key-string)))))

(addtest (llm-deepseek-tests) test-llm-deepseek-chat-json-schema-not-supported-error
  (ensure-error (llm-chat deepseek (make-llm-chat-prompt
                                    query
                                    :response-format '(:type "object"
                                                       :properties (:x (:type "number")))))))

(addtest (llm-deepseek-tests) test-llm-deepseek-embedding-not-supported-error
  (ensure-error (llm-embedding deepseek query)))

(addtest (llm-deepseek-tests) test-llm-deepseek-batch-embeddings-not-supported-error
  (ensure-error (llm-batch-embeddings deepseek query)))
