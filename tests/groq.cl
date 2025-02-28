;;;; See the file LICENSE for the full license governing this code.
(in-package #:acl-llm.protocol)

(deftestsuite llm-groq-tests (llm-openai-compatible-tests)
  (groq)
  (:setup (setf groq (make-llm-groq :key (get-dummy-api-key-string)))))

(addtest (llm-groq-tests) test-llm-groq-chat-json-schema-not-supported-error
  (ensure-error (llm-chat groq (make-llm-chat-prompt
                                query
                                :response-format '(:type "object"
                                                   :properties (:x (:type "number")))))))

(addtest (llm-groq-tests) test-llm-groq-embedding-not-supported-error
  (ensure-error (llm-embedding groq query)))

(addtest (llm-groq-tests) test-llm-groq-batch-embeddings-not-supported-error
  (ensure-error (llm-batch-embeddings groq query)))
