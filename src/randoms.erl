-module(randoms).

-author('alexey@process-one.net').

-export([get_string/0]).

-export([start/0, init/0]).

start() ->
    register(random_generator, spawn(randoms, init, [])).

init() ->
    {A1, A2, A3} = os:timestamp(), random:seed(A1, A2, A3), loop().

loop() ->
    receive
      {From, get_random, N} ->
          From ! {random, random:uniform(N)}, loop();
      _ -> loop()
    end.

get_string() ->
    random_generator ! {self(), get_random, 65536*65536*65536*100},
    receive
      {random, R} -> jlib:integer_to_binary(R)
    end.
