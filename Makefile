PROJECT = feeder
PLT_APPS = xmerl

include erlang.mk

publish:
	mix hex.publish

.PHONY: publish
