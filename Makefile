REBAR?=rebar

all: deps build

clean:
	$(REBAR) clean
	rm -rf logs
	rm -f *.beam **/*.beam

deps: 
	$(REBAR) get-deps

build: 
	$(REBAR) compile

test:
	$(REBAR) eunit skip_deps=true

.PHONY: all clean deps build test
