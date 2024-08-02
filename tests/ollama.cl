;;;; ollama.cl
;;;; See the file LICENSE for the full license governing this code.

;;;; To run this test suite, an `ollama' server must be running locally with port 11434
;;;; Please see https://github.com/ollama/ollama for how to quickly set up one
;;;; (lift:run-tests :suite "llm-ollama-tests")

(in-package #:acl-llm.protocol)

(lift:deftestsuite llm-ollama-tests ()
  (ollama
   (text "Why is sky blue?"))
  (:setup (setf ollama (make-instance 'llm-ollama :chat-model nil :embedding-model nil)))
  (:tests (test-llm-capabilities
           (lift:ensure-same (llm-capabilities ollama)
                             '(streaming embeddings)))

          (test-llm-name
           (lift:ensure-null (llm-name ollama))
           (setf (llm-ollama-chat-model ollama) "llama3")
           (lift:ensure-same (llm-name ollama) "llama3" :test 'string=))

          (test-llm-vendor-chat-url
           (lift:ensure-same (llm-vendor-chat-url ollama)
                             "http://localhost:11434/api/chat")

           (setf (llm-ollama-scheme ollama) "https")
           (lift:ensure-same (llm-vendor-chat-url ollama)
                             "https://localhost:11434/api/chat")

           (setf (llm-ollama-scheme ollama) "http")
           (setf (llm-ollama-host ollama) "franz.com")
           (lift:ensure-same (llm-vendor-chat-url ollama)
                             "http://franz.com:11434/api/chat")

           (setf (llm-ollama-scheme ollama) "http")
           (setf (llm-ollama-host ollama) "localhost")
           (setf (llm-ollama-port ollama) "8080")
           (lift:ensure-same (llm-vendor-chat-url ollama)
                             "http://localhost:8080/api/chat"))

          (test-llm-chat-timeout
           (lift:ensure-same (llm-vendor-chat-timeout ollama)
                             *llm-ollama-chat-timeout*))

          (test-llm-chat
           (setf (llm-ollama-chat-model ollama) "llama3")
           (lift:ensure (stringp (llm-chat ollama (make-llm-chat-prompt text)))))

          (test-llm-chat-unknown-model
           (setf (llm-ollama-chat-model ollama) "foo")
           (lift:ensure-error (llm-chat ollama (make-llm-chat-prompt text))))

          (test-llm-embedding-url
           (lift:ensure-same (llm-vendor-embedding-url ollama)
                             "http://localhost:11434/api/embeddings")
           (lift:ensure-same (llm-vendor-embedding-url ollama)
                             "http://localhost:11434/api/embeddings")

           (setf (llm-ollama-scheme ollama) "https")
           (lift:ensure-same (llm-vendor-embedding-url ollama)
                             "https://localhost:11434/api/embeddings")

           (setf (llm-ollama-scheme ollama) "http")
           (setf (llm-ollama-host ollama) "franz.com")
           (lift:ensure-same (llm-vendor-embedding-url ollama)
                             "http://franz.com:11434/api/embeddings")

           (setf (llm-ollama-scheme ollama) "http")
           (setf (llm-ollama-host ollama) "localhost")
           (setf (llm-ollama-port ollama) "8080")
           (lift:ensure-same (llm-vendor-embedding-url ollama)
                             "http://localhost:8080/api/embeddings"))

          (test-llm-embedding
           (setf (llm-ollama-embedding-model ollama) "llama3")
           (let ((embedding (llm-embedding ollama text)))
             (lift:ensure-same (length embedding) 4096)
             (lift:ensure (every 'double-float-p embedding))))

          (test-llm-embedding-unknown-model
           (setf (llm-ollama-embedding-model ollama) "foo")
           (lift:ensure-error (llm-embedding ollama text)))))
