REBAR?=rebar

all: build

clean:
	$(REBAR) clean
	rm -rf logs
	rm -f test/*.beam

build: 
	$(REBAR) compile

tap: test/etap.beam 
	prove -v test

test: build tap 

%.beam: %.erl
	erlc -o test/ $<

.PHONY: all clean build tap test
