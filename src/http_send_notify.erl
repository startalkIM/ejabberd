-module(http_send_notify).

-export([handle/1]).
-include("ejabberd.hrl").
-include("logger.hrl").

handle(Req) ->
    {Method, Req1} = cowboy_req:method(Req),
    case Method of 
        <<"POST">> -> send_notify(Req1);
        _ -> http_utils:cowboy_req_reply_json(http_utils:gen_fail_result(1, <<Method/binary, " is not disable">>), Req1)
    end.

send_notify(Req)->
    {ok, Body, Req1} = cowboy_req:body(Req),
    case rfc4627:decode(Body) of
        {ok, {obj,Args},[]} ->
            From = proplists:get_value("from",Args),
            To = proplists:get_value("to",Args),
            Catagory = proplists:get_value("category",Args),
            Data = proplists:get_value("data",Args),
            do_send_notify(From, To, Catagory, Data),
            http_utils:cowboy_req_reply_json(http_utils:gen_success_result(), Req1);
        _ ->
            http_utils:cowboy_req_reply_json(http_utils:gen_fail_result(1, <<"Josn parse error">>), Req1)
    end.

do_send_notify(From, To, Catagory, Data) when is_list(To) ->
    do_send_notify1(From, To, Catagory, Data);
do_send_notify(From, To, Catagory, Data) ->
    do_send_notify1(From, [To], Catagory, Data).

do_send_notify1(_, [], _, _) -> ok;
do_send_notify1(From, [To|Rest], Catagory, Data) ->
    catch ejabberd_rpc_presence:send_notify_presence(From, To, Catagory, Data),
    do_send_notify1(From, Rest, Catagory, Data).
