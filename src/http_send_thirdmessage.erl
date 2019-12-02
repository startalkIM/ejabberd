-module(http_send_thirdmessage).

-export([handle/1]).
-include("ejabberd.hrl").
-include("logger.hrl").

handle(Req) ->
    {Method, Req1} = cowboy_req:method(Req),
    case Method of
        <<"POST">> -> send_message(Req1);
        _ -> http_utils:cowboy_req_reply_json(http_utils:gen_fail_result(1, <<Method/binary, " is not disable">>), Req1)
    end.

send_message(Req)->
    {ok, Body, Req1} = cowboy_req:body(Req),
    case rfc4627:decode(Body) of
        {ok, {obj,Args},[]} ->
            do_send_messages([{obj, Args}]),
            http_utils:cowboy_req_reply_json(http_utils:gen_success_result(), Req1);
        {ok, Msgs, []} when is_list(Msgs) ->
            do_send_messages(Msgs),
            http_utils:cowboy_req_reply_json(http_utils:gen_success_result(), Req1);
        _ ->
            http_utils:cowboy_req_reply_json(http_utils:gen_fail_result(1, <<"Josn parse error">>), Req1)
    end.

do_send_messages([]) -> ok;
do_send_messages([{obj, Args}|Rest]) ->
    From = proplists:get_value("from",Args),
    To = proplists:get_value("to",Args),
    Message = proplists:get_value("message",Args),
    JFrom = jlib:string_to_jid(From),
    JTo = jlib:string_to_jid(To),
    Packet = fxml_stream:parse_element(Message),
    catch mod_static:add_record(<<"rpc_thirdparty_chat_message">>,1),
    catch qtalk_c2s:carbon_message(JFrom, JTo, Packet),
    ejabberd_router:route(JFrom,JTo,Packet),
    do_send_messages(Rest).
