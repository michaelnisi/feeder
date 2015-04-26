PROJECT = feeder
include erlang.mk

publish:
		MIX_EXS=package.exs mix hex.publish

.PHONY: publish
