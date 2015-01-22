ERLFLAGS= -pa $(CURDIR)/ebin -pa $(CURDIR)/deps/*/ebin
ERL = $(shell which erl)

ifeq ($(ERL),)
$(error "Erlang not found")
endif

# Just use local rebar, don't want any nasty surprises
REBAR=$(CURDIR)/rebar

.PHONY: all compile escript

all: deps compile escript

deps:
	$(REBAR) get-deps
	$(REBAR) compile

compile:
	$(REBAR) skip_deps=true compile

escript: deps compile
	$(REBAR) skip_deps=true escriptize

clean:
	- rm -rf $(CURDIR)/ebin
	$(REBAR) skip_deps=true clean

distclean: clean
	- rm -rvf $(CURDIR)/deps
