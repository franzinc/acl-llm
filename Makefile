## llm

mlisp ?= /fi/cl/10.1/bin/mlisp-64

fasls:
	@if  [ ! -e $(mlisp) ] ; then echo you must specify the location of an mlisp ; exit 1 ; fi
	$(mlisp) < load.cl

clean:
	rm -fr openai.fasl llm.fasl *~  .*~
