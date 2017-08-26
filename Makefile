PROJECT = feeder
include erlang.mk

publish:
	mix hex.publish

.PHONY: publish
