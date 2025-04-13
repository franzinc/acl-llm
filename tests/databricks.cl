;;;; See the file LICENSE for the full license governing this code.
(in-package #:acl-llm.protocol)

(deftestsuite llm-databricks-tests (llm-openai-compatible-tests)
  ((endpoint-url "http://dummy.example")
   databricks)
  (:setup (setf databricks (make-llm-databricks :endpoint-url endpoint-url
                                                :key (get-dummy-api-key-string)))))

(addtest (llm-databricks-tests) test-llm-databricks-chat-url
  (ensure-same (llm-vendor-chat-url databricks)
               endpoint-url))

(addtest (llm-databricks-tests) test-llm-databricks-embedding-url
  (ensure-same (llm-vendor-embedding-url databricks)
               endpoint-url))
