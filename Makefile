REBAR?=rebar

all: build

clean:
	$(REBAR) clean
	rm -rf logs
	rm -f *.beam **/*.beam

build: 
	$(REBAR) compile

test:
	$(REBAR) eunit

.PHONY: all clean build test
