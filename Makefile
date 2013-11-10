.PHONY: test

REBAR=rebar
ETEST=./deps/etest/bin/etest-runner

all: compile

compile:
	$(REBAR) compile

test: compile
	$(ETEST)
