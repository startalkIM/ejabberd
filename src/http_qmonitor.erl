%% Feel free to use, reuse and abuse the code in this file.

-module(http_qmonitor).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-export([get_monitor_info/0]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("ejabberd_config.hrl").


-define(MB, (1024 * 1024)).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req1} = cowboy_req:method(Req),
    case Method of 
        <<"GET">> ->
            {ok, Req2} = get_echo(Method,Req1),
            {ok, Req2, State};
        _ ->
            {ok,Req2} = echo(undefined, Req1),
            {ok, Req2, State}
    end.

get_echo(<<"GET">>,Req) ->
    Res = get_monitor_info(),
    cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], Res, Req);
get_echo(_,Req) -> cowboy_req:reply(405, Req).

echo(undefined, Req) -> cowboy_req:reply(400, [], <<"Missing parameter.">>, Req);
echo(Echo, Req) -> cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], Echo, Req).

terminate(_Reason, _Req, _State) -> ok.

get_monitor_info() ->
    ServiceStat = mod_static:get_static1(),
    PortStat = port_stat(),
    MsgQueueStat = msg_queue_stat(),
    MemoryStat = memory_stat(),
    ClientStat = client_platform_stat(),
    Result = gen_result(lists:flatten([MemoryStat, MsgQueueStat, PortStat, ServiceStat, ClientStat])),
    Result.

client_platform_stat() ->
    N = length(nodes()) + 1,
    case catch ets:lookup(local_config,{global,db_hosts}) of
        [DHost] when is_record(DHost,local_config) ->
            V = DHost#local_config.value,
            lists:map(fun(H) ->
                {<<"user_login_value_", H/binary>>, integer_to_list(ejabberd_sm:get_vh_session_number(H) div N)}
            end, V);
	_ -> []
    end.

port_stat() ->
    TcpNum = length(recon:tcp()),
    FileNum = length(recon:files()),
    PortNum = erlang:system_info(port_count),
    ProcessNum = erlang:system_info(process_count),

    [{<<"tcp_num">>, TcpNum}, {<<"file_num">>, FileNum}, {<<"port_num">>, PortNum}, {<<"process_count">>, ProcessNum}].

msg_queue_stat() ->
    [{Pid, MsgNum, Attr}] = recon:proc_count(message_queue_len, 1),

    ?DEBUG("the process(~p) message_queue num is ~p, and the attr is ~p, the messages is ~p~n", [Pid, MsgNum, Attr, erlang:process_info(Pid, messages)]),
    ?DEBUG("the process(~p) current_stacktrace is ~p~n", [Pid, erlang:process_info(Pid, current_stacktrace)]),
    [{<<"max_msg_queue">>, MsgNum}].

memory_stat() ->
    UsedNum = recon_alloc:memory(used) div ?MB,
    AllocatedNum = recon_alloc:memory(allocated) div ?MB,

    [{<<"memory_used">>, UsedNum}, {<<"memory_allocated">>, AllocatedNum}].

gen_result(StatList) ->
    lists:foldl(fun({Key, Value}, Acc) ->
			NewValue = to_binary(Value),
			<<Key/binary, "=", NewValue/binary, "\n", Acc/binary>>
		end, <<"">>, StatList).

to_binary(Value) when is_integer(Value) ->
    integer_to_binary(Value);
to_binary(Value) when is_float(Value) ->
    float_to_binary(Value);
to_binary(Value) when is_list(Value) ->
    list_to_binary(Value);
to_binary(Value) when is_binary(Value) ->
    Value.
