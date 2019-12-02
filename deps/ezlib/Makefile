all: src

src:
	rebar compile xref

clean:
	rebar clean

test: all
	rebar -v skip_deps=true eunit

.PHONY: clean src test all
