(with-compilation-unit nil
  #+fast-dot-product (load "./simdot.so")
  (dolist (file '("llm.cl"
                  "util.cl"
                  "vector-database.cl"
                  "writing-floats.cl"
                  "embed.cl"
                  "shortq.cl"

                  #+fast-dot-product "simdot.cl"
;;; load the :llama2 package( definitions
                  "llama-cpp.cl"
;;; load the :gpt package definitions
                  "openai.cl"
                  ))
    (load (compile-file file))))
