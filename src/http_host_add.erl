-module(http_host_add).

-export([handle/1]).
-include("ejabberd.hrl").
-include("logger.hrl").

handle(Req) ->
    {Method, _} = cowboy_req:method(Req),
    case Method of
        <<"POST">> ->
            do_handle(Req);
        _ ->
            http_utils:cowboy_req_reply_json(http_utils:gen_fail_result(1, <<Method/binary, " is not disable">>), Req)
    end.

do_handle(Req)->
    {ok, Body, Req1} = cowboy_req:body(Req),
    case rfc4627:decode(Body) of
    {ok,{obj,Args},[]}  ->
        Res = add_host(Args),
        http_utils:cowboy_req_reply_json(Res, Req1);
    _ ->
        http_utils:cowboy_req_reply_json(http_utils:gen_fail_result(1,<<"Json format error.">>), Req1)
    end.

add_host(Args) ->
    Host = proplists:get_value("host",Args,<<"">>),
    ?INFO_MSG("ARgs ~p ~n",[Args]),
    case catch ejabberd_sql:sql_query(<<"ejabhost1">>,[<<"select id from host_info where host = '">>,Host,<<"'; ">>]) of
    {selected,[<<"id">>],[[_]]} ->
        R = lists:foldl(fun(N, Acc) -> [rpc:cast(N, ejabberd_reload_hosts, add_hosts, [Host])|Acc] end, [], [node()|nodes()]),
        case catch lists:all(fun(T) -> T end, R) of
        true -> http_utils:gen_result(true, <<"0">>,<<"">>,<<"sucess">>);
         _ -> http_utils:gen_result(false, <<"1">>,<<"">>,<<"failed">>)
        end;
    _ ->
        http_utils:gen_result(false, <<"1">>,<<"">>,<<"failed">>)
    end.

