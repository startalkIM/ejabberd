# Snappy as nif for Erlang

## Acknowledgements

Snappy source code forked from: https://github.com/google/snappy

Erlang nif support copied from: https://github.com/fdmanana/snappy-erlang-nif

## Usage examples

```
1> {ok, Bin} = snappyer:compress("abc").
{ok,<<3,8,97,98,99>>}
2> {ok, <<"abc">>} = snappyer:decompress(Bin).
{ok,<<"abc">>}

```

## Tests

```
make eunit

```

## Versioning

| Snappyer Version | Google/Snappy Version |
| ---------------- | --------------------- |
| 1.1.3-1.0.x      | 1.1.3                 |
| 1.2.x            | 1.1.3                 |

