
(eval-when (compile eval)
  (when (equalp "yes" (sys:getenv "ACL_LLM_BUILD_FOR_ACL"))
    (format t ";; BUILDING FOR ACL~%")
    (push :acl-llm-build *features*)))

(with-compilation-unit nil
  #+fast-dot-product (load "./simdot.so")
  (dolist (file '("llm.cl"
                  "util.cl"
                  "vector-database.cl"
                  "writing-floats.cl"
                  "embed.cl"
                  "shortq.cl"

                  #+fast-dot-product "simdot.cl"
;;; load the :llama2 package definitions
                  "llama-cpp.cl"
;;; load the :gpt package definitions
                  "openai.cl"
                  "serp.cl"
                  "protocol.cl"
                  ))
    (load (compile-file file))))
