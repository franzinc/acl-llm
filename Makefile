## llm

mlisp      ?= /fi/cl/10.1/bin/mlisp-64
mlisp_args ?=

fasls:
	@if  [ ! -e $(mlisp) ] ; then \
	    echo you must specify the location of an mlisp; \
	    exit 1; \
	fi
	$(mlisp) $(mlisp_args) -L load.cl --kill
	cat llm.fasl openai.fasl > acl-llm.fasl

clean:
	rm -fr openai.fasl llm.fasl *~  .*~
