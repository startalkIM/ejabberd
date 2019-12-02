# Fast Stringprep implementation for Erlang / Elixir

[![Build Status](https://travis-ci.org/processone/stringprep.svg?branch=master)](https://travis-ci.org/processone/stringprep) [![Coverage Status](https://coveralls.io/repos/processone/stringprep/badge.svg?branch=master&service=github)](https://coveralls.io/github/processone/stringprep?branch=master) [![Hex version](https://img.shields.io/hexpm/v/stringprep.svg "Hex version")](https://hex.pm/packages/stringprep)

Stringprep is a framework for preparing Unicode test strings in order
to increase the likelihood that string input and string comparison
work.

The principle are defined in [RFC-3454: Preparation of
Internationalized Strings ](http://tools.ietf.org/html/rfc3454).

This library is leverage Erlang native NIF mechanism to provide
extremely fast and efficient processing.

The library includes support for several Stringprep profiles used in
XMPP protocole like:

* Nodeprep
* Nameprep
* Resourceprep

For those profiles, the rules are applied according to
[RFC6122](http://xmpp.org/rfcs/rfc6122.html#security-stringprep). The
various functions perform check on the allowed / forbidden chars for a
given profile or prevent combining left-to-right and right-to-left
chars.

It the binary string passed to a function of the API is valid, it will
return its normalized version according to Stringprep
profile. Otherwise, if the binary string is invalid, for example
because it contains invalid chars, the function will return error.

The library is heavily used in XMPP string processing. However, the
library is more generally useful in code that need to manipulate and
compare Unicode strings.

## Building

Fast Stringprep processing tool can be build as follow:

    ./configure && make

Configure script recognizes one flag - pass `--enable-gcov` to enable gcov
coverage reporting.

It is a rebar-compatible OTP application. Alternatively, you can build
it with rebar:

    rebar get-deps compile

## Usage

You can start the application with the command:

```
$ erl -pa ebin/
Erlang/OTP 18 [erts-7.1] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V7.1  (abort with ^G)
1> application:start(p1_stringprep).
```

You can then call any of the stringprep function to apply a profile:

```
stringprep:nodeprep(<<>>).
stringprep:nameprep(<<>>).
stringprep:resourceprep(<<>>).
stringprep:tolower(<<>>)).
```

## Development

### Test

#### Unit test

You can run eunit test with the command:

    $ make test
