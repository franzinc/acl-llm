;;;; See the file LICENSE for the full license governing this code.
(in-package #:acl-llm.protocol)

(deftestsuite llm-azure-tests (llm-openai-compatible-tests)
  ((endpoint-url "http://dummy.example")
   azure)
  (:setup (setf azure (make-llm-azure :endpoint-url endpoint-url
                                      :key (get-dummy-api-key-string)))))

(addtest (llm-azure-tests) test-llm-azure-chat-url
  (ensure-same (llm-vendor-chat-url azure)
               endpoint-url))

(addtest (llm-azure-tests) test-llm-azure-embedding-url
  (ensure-same (llm-vendor-embedding-url azure)
               endpoint-url))
