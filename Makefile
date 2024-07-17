## llm

mlisp      ?= /fi/cl/10.1/bin/mlisp-64
mlisp_args ?=

FASL_FILES = llm.fasl vector-database.fasl writing-floats.fasl embed.fasl \
	     shortq.fasl util.fasl llama-cpp.fasl openai.fasl serp.fasl protocol.fasl ollama.fasl

fasls:
	@if  [ ! -e $(mlisp) ] ; then \
	    echo you must specify the location of an mlisp; \
	    exit 1; \
	fi
	$(mlisp) $(mlisp_args) -L load.cl --kill
	cat $(FASL_FILES) > acl-llm.fasl

acl_fasls:
	env ACL_LLM_BUILD_FOR_ACL=yes $(MAKE) fasls

clean:
	rm -fr *.fasl *~  .*~





