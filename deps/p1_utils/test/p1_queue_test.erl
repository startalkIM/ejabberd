%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2017-2019 Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created :  9 Mar 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(p1_queue_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("p1_queue.hrl").

queue_dir() ->
    {ok, Cwd} = file:get_cwd(),
    filename:join(Cwd, "p1_queue").

eacces_dir() ->
    {ok, Cwd} = file:get_cwd(),
    filename:join(Cwd, "eacces_queue").

mk_list() ->
    mk_list(1, 10).

mk_list(From, To) ->
    lists:seq(From, To).

start_test() ->
    ?assertEqual(ok, p1_queue:start(queue_dir())).

double_start_test() ->
    ?assertEqual(ok, p1_queue:start(queue_dir())).

new_ram_test() ->
    p1_queue:new().
new_file_test() ->
    Q = p1_queue:new(file),
    ?assertEqual(ok, p1_file_queue:close(Q)).

double_close_test() ->
    Q = p1_queue:new(file),
    ?assertEqual(ok, p1_file_queue:close(Q)),
    ?assertEqual(ok, p1_file_queue:close(Q)).

close_test() ->
    Q1 = p1_queue:new(file),
    Q2 = p1_queue:new(file),
    ?assertEqual(ok, p1_file_queue:close(Q1)),
    ?assertEqual(ok, p1_file_queue:close(Q2)).

type_ram_test() ->
    Q = p1_queue:new(ram),
    ?assertEqual(ram, p1_queue:type(Q)).
type_file_test() ->
    Q = p1_queue:new(file),
    ?assertMatch({file, _}, p1_queue:type(Q)),
    ?assertEqual(ok, p1_file_queue:close(Q)).

is_queue_ram_test() ->
    Q = p1_queue:new(ram),
    ?assertEqual(true, p1_queue:is_queue(Q)).
is_queue_file_test() ->
    Q = p1_queue:new(file),
    ?assertEqual(true, p1_queue:is_queue(Q)),
    ?assertEqual(ok, p1_file_queue:close(Q)).

is_queue_not_queue_test() ->
    ?assertEqual(false, p1_queue:is_queue(some)).

from_list_ram_test() ->
    L = mk_list(),
    Q = p1_queue:from_list(L),
    ?assertEqual(ram, p1_queue:type(Q)).
from_list_file_test() ->
    L = mk_list(),
    Q = p1_queue:from_list(L, file),
    ?assertMatch({file, _}, p1_queue:type(Q)),
    ?assertEqual(ok, p1_file_queue:close(Q)).

to_list_ram_test() ->
    L = mk_list(),
    Q = p1_queue:from_list(L, ram),
    ?assertEqual(L, p1_queue:to_list(Q)).
to_list_file_test() ->
    L = mk_list(),
    Q = p1_queue:from_list(L, file),
    ?assertEqual(L, p1_queue:to_list(Q)),
    ?assertEqual(ok, p1_file_queue:close(Q)).

len_ram_test() ->
    L = mk_list(),
    Q = p1_queue:from_list(L, ram),
    ?assertEqual(10, p1_queue:len(Q)).
len_file_test() ->
    L = mk_list(),
    Q = p1_queue:from_list(L, file),
    ?assertEqual(10, p1_queue:len(Q)),
    ?assertEqual(ok, p1_file_queue:close(Q)).

len_macro_ram_test() ->
    L = mk_list(),
    Q = p1_queue:from_list(L, ram),
    ?assertMatch(X when ?qlen(X) == 10, Q).
len_macro_file_test() ->
    L = mk_list(),
    Q = p1_queue:from_list(L, file),
    ?assertMatch(X when ?qlen(X) == 10, Q),
    ?assertEqual(ok, p1_file_queue:close(Q)).

is_empty_ram_test() ->
    Q = p1_queue:new(ram),
    ?assertEqual(true, p1_queue:is_empty(Q)).
is_empty_file_test() ->
    Q = p1_queue:new(file),
    ?assertEqual(true, p1_queue:is_empty(Q)),
    ?assertEqual(ok, p1_file_queue:close(Q)).

clear_ram_test() ->
    L = mk_list(),
    Q = p1_queue:from_list(L, ram),
    Q1 = p1_queue:clear(Q),
    ?assertEqual(true, p1_queue:is_empty(Q1)).
clear_file_test() ->
    L = mk_list(),
    Q = p1_queue:from_list(L, file),
    Q1 = p1_queue:clear(Q),
    ?assertEqual(true, p1_queue:is_empty(Q1)),
    ?assertEqual(ok, p1_file_queue:close(Q1)).

in_ram_test() ->
    Q = p1_queue:new(ram),
    Q1 = p1_queue:in(1, Q),
    ?assertEqual([1], p1_queue:to_list(Q1)).
in_file_test() ->
    Q = p1_queue:new(file),
    Q1 = p1_queue:in(1, Q),
    ?assertEqual([1], p1_queue:to_list(Q1)),
    ?assertEqual(ok, p1_file_queue:close(Q1)).

out_ram_test() ->
    Q = p1_queue:new(ram),
    Q1 = p1_queue:in(1, Q),
    ?assertMatch({{value, 1}, Q}, p1_queue:out(Q1)).
out_file_test() ->
    Q = p1_queue:new(file),
    Q1 = p1_queue:in(1, Q),
    ?assertMatch({{value, 1}, Q2} when ?qlen(Q2) == 0, p1_queue:out(Q1)),
    ?assertEqual(ok, p1_file_queue:close(Q1)).

out_empty_ram_test() ->
    Q = p1_queue:new(ram),
    ?assertMatch({empty, Q}, p1_queue:out(Q)).
out_empty_file_test() ->
    Q = p1_queue:new(file),
    ?assertMatch({empty, Q}, p1_queue:out(Q)),
    ?assertEqual(ok, p1_file_queue:close(Q)).

clear_in_test() ->
    Q = p1_queue:from_list([1], file),
    Q1 = p1_queue:drop(Q),
    Q2 = p1_queue:in(2, Q1),
    ?assertEqual([2], p1_queue:to_list(Q2)),
    ?assertEqual(ok, p1_file_queue:close(Q2)).

get_limit_ram_test() ->
    Q = p1_queue:from_list(mk_list(), ram, 10),
    ?assertEqual(10, p1_queue:get_limit(Q)),
    ?assertError(full, p1_queue:in(11, Q)).
get_limit_file_test() ->
    Q = p1_queue:from_list(mk_list(), file, 10),
    ?assertEqual(10, p1_queue:get_limit(Q)),
    ?assertError(full, p1_queue:in(11, Q)),
    ?assertEqual(ok, p1_file_queue:close(Q)).

set_limit_ram_test() ->
    Q = p1_queue:new(ram),
    ?assertEqual(unlimited, p1_queue:get_limit(Q)),
    Q1 = p1_queue:set_limit(Q, 10),
    ?assertEqual(10, p1_queue:get_limit(Q1)).
set_limit_file_test() ->
    Q = p1_queue:new(file),
    ?assertEqual(unlimited, p1_queue:get_limit(Q)),
    Q1 = p1_queue:set_limit(Q, 10),
    ?assertEqual(10, p1_queue:get_limit(Q1)),
    ?assertEqual(ok, p1_file_queue:close(Q)).

from_list_limit_ram_test() ->
    ?assertError(full, p1_queue:from_list(mk_list(), ram, 9)).
from_list_limit_file_test() ->
    ?assertError(full, p1_queue:from_list(mk_list(), file, 9)).

peek_ram_test() ->
    Q = p1_queue:from_list([1], ram),
    ?assertEqual({value, 1}, p1_queue:peek(Q)).
peek_file_test() ->
    Q = p1_queue:from_list([1], file),
    ?assertEqual({value, 1}, p1_queue:peek(Q)),
    ?assertEqual(ok, p1_file_queue:close(Q)).

peek_empty_ram_test() ->
    Q = p1_queue:new(ram),
    ?assertEqual(empty, p1_queue:peek(Q)).
peek_empty_file_test() ->
    Q = p1_queue:new(file),
    ?assertEqual(empty, p1_queue:peek(Q)),
    ?assertEqual(ok, p1_file_queue:close(Q)).

drop_ram_test() ->
    Q = p1_queue:new(ram),
    Q1 = p1_queue:in(1, Q),
    ?assertEqual(Q, p1_queue:drop(Q1)).
drop_file_test() ->
    Q = p1_queue:new(file),
    Q1 = p1_queue:in(1, Q),
    ?assertMatch(Q2 when ?qlen(Q2) == 0, p1_queue:drop(Q1)),
    ?assertEqual(ok, p1_file_queue:close(Q1)).

drop_empty_ram_test() ->
    Q = p1_queue:new(ram),
    ?assertError(empty, p1_queue:drop(Q)).
drop_empty_file_test() ->
    Q = p1_queue:new(file),
    ?assertError(empty, p1_queue:drop(Q)),
    ?assertEqual(ok, p1_file_queue:close(Q)).

foreach_ram_test() ->
    L = mk_list(),
    Q = p1_queue:from_list(L, ram),
    put(p1_queue, []),
    F = fun(X) -> put(p1_queue, get(p1_queue) ++ [X]) end,
    ?assertEqual(ok, p1_queue:foreach(F, Q)),
    ?assertEqual(L, get(p1_queue)).
foreach_file_test() ->
    L = mk_list(),
    Q = p1_queue:from_list(L, file),
    put(p1_queue, []),
    F = fun(X) -> put(p1_queue, get(p1_queue) ++ [X]) end,
    ?assertEqual(ok, p1_queue:foreach(F, Q)),
    ?assertEqual(L, get(p1_queue)),
    ?assertEqual(ok, p1_file_queue:close(Q)).

foldl_ram_test() ->
    L = mk_list(),
    Q = p1_queue:from_list(L, ram),
    F = fun(X, Acc) -> Acc ++ [X] end,
    ?assertEqual(L, p1_queue:foldl(F, [], Q)).
foldl_file_test() ->
    L = mk_list(),
    Q = p1_queue:from_list(L, file),
    F = fun(X, Acc) -> Acc ++ [X] end,
    ?assertEqual(L, p1_queue:foldl(F, [], Q)),
    ?assertEqual(ok, p1_file_queue:close(Q)).

dropwhile_ram_test() ->
    L = mk_list(),
    Q = p1_queue:from_list(L, ram),
    F = fun(X) -> X < 6 end,
    Q1 = p1_queue:dropwhile(F, Q),
    ?assertEqual([6,7,8,9,10], p1_queue:to_list(Q1)).
dropwhile_file_test() ->
    L = mk_list(),
    Q = p1_queue:from_list(L, file),
    F = fun(X) -> X < 6 end,
    Q1 = p1_queue:dropwhile(F, Q),
    ?assertEqual([6,7,8,9,10], p1_queue:to_list(Q1)),
    ?assertEqual(ok, p1_file_queue:close(Q1)).

drop_until_empty_ram_test() ->
    L = mk_list(),
    Q = p1_queue:from_list(L, ram),
    Q1 = p1_queue:dropwhile(fun(_) -> true end, Q),
    ?assertEqual(true, p1_queue:is_empty(Q1)).
drop_until_empty_file_test() ->
    L = mk_list(),
    Q = p1_queue:from_list(L, file),
    Q1 = p1_queue:dropwhile(fun(_) -> true end, Q),
    ?assertEqual(true, p1_queue:is_empty(Q1)),
    ?assertEqual(ok, p1_file_queue:close(Q)).

ram_to_file_test() ->
    L = mk_list(),
    RQ = p1_queue:from_list(L, ram),
    FQ = p1_queue:ram_to_file(RQ),
    ?assertEqual(L, p1_file_queue:to_list(FQ)),
    ?assertEqual(FQ, p1_queue:ram_to_file(FQ)),
    ?assertEqual(ok, p1_file_queue:close(FQ)).

file_to_ram_test() ->
    L = mk_list(),
    FQ = p1_queue:from_list(L, file),
    RQ = p1_queue:file_to_ram(FQ),
    ?assertEqual(L, p1_queue:to_list(RQ)),
    ?assertEqual(RQ, p1_queue:file_to_ram(RQ)),
    ?assertEqual(ok, p1_file_queue:close(FQ)).

not_owner_test() ->
    Pid = self(),
    Owner = spawn_link(
	      fun() ->
		      Q = p1_queue:from_list(mk_list(), file),
		      Pid ! {Q, p1_file_queue:path(Q)},
		      receive stop -> Pid ! stopped end
	      end),
    {Q, Path} = receive M -> M end,
    ?assertError({bad_queue, {not_owner, Path}}, p1_queue:in(11, Q)),
    ?assertError({bad_queue, {not_owner, Path}}, p1_queue:out(Q)),
    ?assertError({bad_queue, {not_owner, Path}}, p1_queue:peek(Q)),
    ?assertError({bad_queue, {not_owner, Path}}, p1_queue:drop(Q)),
    ?assertError({bad_queue, {not_owner, Path}}, p1_queue:to_list(Q)),
    ?assertError({bad_queue, {not_owner, Path}}, p1_queue:clear(Q)),
    ?assertError({bad_queue, {not_owner, Path}},
		 p1_queue:foreach(fun(_) -> ok end, Q)),
    ?assertError({bad_queue, {not_owner, Path}},
		 p1_queue:dropwhile(fun(_) -> true end, Q)),
    ?assertError({bad_queue, {not_owner, Path}},
		 p1_queue:foldl(fun(_, X) -> X end, ok, Q)),
    Owner ! stop,
    receive stopped -> ok end.

format_error_test() ->
    Path = "/path/to/queue",
    PathBin = list_to_binary(Path),
    ?assertEqual("foo1234 (" ++ Path ++ ")",
		 p1_queue:format_error({foo1234, PathBin})),
    ?assertNotEqual("not_owner (" ++ Path ++ ")",
		    p1_queue:format_error({not_owner, PathBin})),
    ?assertNotEqual("corrupted (" ++ Path ++ ")",
		    p1_queue:format_error({corrupted, PathBin})).

bad_size_test() ->
    #file_q{fd = Fd, path = Path} = Q = p1_queue:from_list([1], file),
    ?assertMatch({ok, _}, file:position(Fd, 0)),
    ?assertEqual(ok, file:truncate(Fd)),
    ?assertEqual(ok, file:pwrite(Fd, 0, <<1>>)),
    ?assertError({bad_queue, {corrupted, Path}}, p1_queue:out(Q)),
    ?assertError({bad_queue, {corrupted, Path}}, p1_queue:peek(Q)),
    ?assertError({bad_queue, {corrupted, Path}}, p1_queue:drop(Q)),
    ?assertError({bad_queue, {corrupted, Path}}, p1_queue:to_list(Q)),
    ?assertError({bad_queue, {corrupted, Path}}, p1_queue:dropwhile(fun(_) -> true end, Q)),
    ?assertError({bad_queue, {corrupted, Path}}, p1_queue:foreach(fun(_) -> ok end, Q)),
    ?assertError({bad_queue, {corrupted, Path}}, p1_queue:foldl(fun(_, _) -> ok end, ok, Q)),
    ?assertEqual(ok, p1_file_queue:close(Q)).

eof_test() ->
    #file_q{fd = Fd, path = Path} = Q = p1_queue:from_list([1], file),
    ?assertMatch({ok, _}, file:position(Fd, 0)),
    ?assertEqual(ok, file:truncate(Fd)),
    ?assertEqual(ok, file:pwrite(Fd, 0, <<1:32>>)),
    ?assertError({bad_queue, {corrupted, Path}}, p1_queue:out(Q)),
    ?assertError({bad_queue, {corrupted, Path}}, p1_queue:peek(Q)),
    ?assertError({bad_queue, {corrupted, Path}}, p1_queue:to_list(Q)),
    ?assertError({bad_queue, {corrupted, Path}}, p1_queue:dropwhile(fun(_) -> true end, Q)),
    ?assertError({bad_queue, {corrupted, Path}}, p1_queue:foreach(fun(_) -> ok end, Q)),
    ?assertError({bad_queue, {corrupted, Path}}, p1_queue:foldl(fun(_, _) -> ok end, ok, Q)),
    ?assertEqual(ok, p1_file_queue:close(Q)).

bad_term_test() ->
    #file_q{fd = Fd, path = Path} = Q = p1_queue:from_list([1], file),
    ?assertMatch({ok, _}, file:position(Fd, 0)),
    ?assertEqual(ok, file:truncate(Fd)),
    ?assertEqual(ok, file:pwrite(Fd, 0, <<5:32, 1>>)),
    ?assertError({bad_queue, {corrupted, Path}}, p1_queue:out(Q)),
    ?assertError({bad_queue, {corrupted, Path}}, p1_queue:peek(Q)),
    ?assertError({bad_queue, {corrupted, Path}}, p1_queue:to_list(Q)),
    ?assertError({bad_queue, {corrupted, Path}}, p1_queue:dropwhile(fun(_) -> true end, Q)),
    ?assertError({bad_queue, {corrupted, Path}}, p1_queue:foreach(fun(_) -> ok end, Q)),
    ?assertError({bad_queue, {corrupted, Path}}, p1_queue:foldl(fun(_, _) -> ok end, ok, Q)),
    ?assertEqual(ok, p1_file_queue:close(Q)).

closed_test() ->
    #file_q{path = Path} = Q = p1_queue:from_list([1], file),
    ?assertEqual(ok, p1_file_queue:close(Q)),
    ?assertError({bad_queue, {einval, Path}}, p1_queue:in(2, Q)),
    ?assertError({bad_queue, {einval, Path}}, p1_queue:out(Q)),
    ?assertError({bad_queue, {einval, Path}}, p1_queue:peek(Q)),
    ?assertError({bad_queue, {einval, Path}}, p1_queue:drop(Q)),
    ?assertError({bad_queue, {einval, Path}}, p1_queue:to_list(Q)),
    ?assertError({bad_queue, {einval, Path}}, p1_queue:dropwhile(fun(_) -> true end, Q)),
    ?assertError({bad_queue, {einval, Path}}, p1_queue:foreach(fun(_) -> ok end, Q)),
    ?assertError({bad_queue, {einval, Path}}, p1_queue:foldl(fun(_, _) -> ok end, ok, Q)),
    ?assertError({bad_queue, {einval, Path}}, p1_file_queue:clear(Q)),
    ?assertEqual(ok, p1_file_queue:close(Q)).

write_fail_test() ->
    #file_q{fd = Fd, path = Path} = Q = p1_queue:new(file),
    ?assertEqual(ok, file:close(Fd)),
    %% Open file in read-only mode, so write operations fail
    {ok, NewFd} = file:open(Path, [read, binary, raw]),
    Q1 = Q#file_q{fd = NewFd},
    ?assertError({bad_queue, {ebadf, Path}}, p1_queue:in(1, Q1)),
    ?assertError({bad_queue, {einval, Path}}, p1_file_queue:clear(Q1)),
    ?assertEqual(ok, p1_file_queue:close(Q1)).

gc_test() ->
    Q = p1_queue:from_list(lists:seq(1, 1001), file),
    Q1 = p1_queue:dropwhile(fun(X) -> X =< 1000 end, Q),
    ?assertMatch(#file_q{head = 1000, tail = 1}, Q1),
    %% GC should be called here
    Q2 = p1_queue:in(1002, Q1),
    ?assertMatch(#file_q{head = 0, tail = 2}, Q2),
    ?assertEqual(ok, p1_file_queue:close(Q2)).

destruction_test() ->
    %% Check if drop/1 and out/1 don't destruct original queue
    Q = p1_queue:from_list([1], file),
    p1_queue:drop(Q),
    ?assertMatch({_, _}, p1_queue:out(Q)),
    ?assertEqual(true, p1_queue:is_queue(Q)),
    ?assertEqual(1, p1_queue:len(Q)),
    ?assertEqual(false, p1_queue:is_empty(Q)),
    ?assertEqual({value, 1}, p1_queue:peek(Q)),
    ?assertEqual([1], p1_queue:to_list(Q)),
    ?assertEqual(Q, p1_queue:dropwhile(fun(_) -> false end, Q)),
    ?assertEqual([1], p1_queue:foldl(fun(X, Acc) -> [X|Acc] end, [], Q)),
    ?assertEqual(ok, p1_queue:foreach(fun(_) -> ok end, Q)),
    ?assertEqual(ok, p1_file_queue:close(Q)).

emfile_test() ->
    _ = [p1_queue:new(file) || _ <- lists:seq(1, 10)],
    ?assertError(emfile, p1_queue:new(file)).

stop_test() ->
    ?assertMatch({ok, [_|_]}, file:list_dir(queue_dir())),
    ?assertEqual(ok, p1_queue:stop()),
    ?assertEqual({ok, []}, file:list_dir(queue_dir())).

start_fail_test() ->
    Dir = eacces_dir(),
    QDir = filename:join(Dir, "p1_queue"),
    ?assertEqual(ok, filelib:ensure_dir(QDir)),
    ?assertEqual(ok, file:change_mode(Dir, 8#00000)),
    ?assertMatch({error, _}, p1_queue:start(QDir)).

start_eacces_test() ->
    ?assertMatch(ok, p1_queue:start(eacces_dir())).

new_eacces_test() ->
    ?assertError({bad_queue, {eacces, _}}, p1_queue:new(file)).

from_list_eacces_test() ->
    L = mk_list(),
    ?assertError({bad_queue, {eacces, _}}, p1_queue:from_list(L, file)).

stop_eaccess_test() ->
    ?assertEqual(ok, p1_queue:stop()).
