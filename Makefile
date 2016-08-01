.PHONY: deps compile test

REBAR=./rebar

deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

test: compile
	$(REBAR) eunit skip_deps=true
