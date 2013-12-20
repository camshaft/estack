PROJECT = estack

default: all

## TODO don't use rebar to run eunit tests
test:
	rebar skip_deps=true eunit

## TODO add horse perf testing

include erlang.mk

.PHONY: default test
