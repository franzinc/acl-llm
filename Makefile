## llm

mlisp      ?= /fi/cl/10.1/bin/mlisp-64
mlisp_args ?=

FASL_FILES = llm.fasl vector-database.fasl writing-floats.fasl acl-specific.fasl embed.fasl shortq.fasl \
             util.fasl llama-cpp.fasl openai.fasl

fasls:
	@if  [ ! -e $(mlisp) ] ; then \
	    echo you must specify the location of an mlisp; \
	    exit 1; \
	fi
	$(mlisp) $(mlisp_args) -L load.cl --kill
	cat $(FASL_FILES) > acl-llm.fasl

clean:
	rm -fr *.fasl *~  .*~





