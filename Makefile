ifeq ($(shell which rebar3),)
$(error "rebar3 not found")
endif

.PHONY: all install

all:
	rebar3 escriptize

install: all
	sudo cp _build/default/bin/relflow /usr/bin/relflow

clean:
	rebar3 clean
