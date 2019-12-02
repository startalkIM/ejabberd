PROJECT = snappyer
PROJECT_DESCRIPTION = Snappy as nif for Erlang
PROJECT_VERSION = 1.2.1

SP = 2

CPPFLAGS = -Wno-sign-compare -Wno-unused-function
LDFLAGS += -shared -lstdc++

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
CXXFLAGS = -O3 -arch x86_64
LDFLAGS += -arch x86_64 -flat_namespace -undefined suppress
endif

include erlang.mk

vsn-check:
	$(verbose) ./vsn-check.sh $(PROJECT_VERSION)

hex-publish: distclean
	rebar3 hex publish
