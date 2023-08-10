(load "load.cl")
(in-package :llama2)
(setf *llama-cpp-python-api-protocol* (sys:getenv "ACL_LLM_LLAMA_CPP_PYTHON_API_PROTOCOL"))
(setf *llama-cpp-python-api-host* (sys:getenv "ACL_LLM_LLAMA_CPP_PYTHON_API_HOST"))
(setf *llama-cpp-python-api-port* (sys:getenv "ACL_LLM_LLAMA_CPP_PYTHON_API_PORT"))
(in-package :gpt)
(setf *default-vector-database-dir* (sys:getenv "ACL_LLM_DEFAULT_VECTOR_DATABASE_DIR"))
(set-openai-api-key (sys:getenv "ACL_LLM_OPENAI_API_KEY"))


