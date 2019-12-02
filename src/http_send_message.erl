-module(http_send_message).

-export([handle/1]).
-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

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
            Res = http_send_message(Args),
            http_utils:cowboy_req_reply_json(Res, Req1);
        _ ->
            http_utils:cowboy_req_reply_json(http_utils:gen_fail_result(1, <<"Josn parse error">>), Req1)
    end.

http_send_message(Json) ->
    Servers = ejabberd_config:get_myhosts(),
    Server = lists:nth(1,Servers),
    http_send_message(Server,Json).

http_send_message(Server,Args)->
    From = proplists:get_value("From",Args),
    To = proplists:get_value("To",Args),
    Body = proplists:get_value("Body",Args),
    Type = proplists:get_value("Type",Args,<<"chat">>),
    Msg_Type  = proplists:get_value("Msg_Type",Args),
    Host = proplists:get_value("Host",Args,Server),
    Domain = proplists:get_value("Domain",Args),
    Extend_Info = proplists:get_value("Extend_Info",Args,<<"">>),
    case Type of
        <<"groupchat">> -> send_muc_msg(Server,From,To,Host,Domain,Body,Extend_Info,Msg_Type);
        _ -> send_chat_msg(Server,From,To,Host,Body,Msg_Type,Extend_Info,Type)
    end.

send_chat_msg(Server,From,To,Host,Body,Msg_Type,Extend_Info,Type) ->
    case Body of
        undefined -> http_utils:gen_result(false, 1, <<"Message Body is Null">>);
        _ ->
            JFrom = jlib:make_jid(From,Server,<<"">>),
            case JFrom of
                error -> http_utils:gen_result(false, 1, <<"From make jid error">>);
                _ ->
                    lists:foreach(fun({obj,[{"User",ToU}]}) ->
                        case jlib:make_jid(ToU,Host,<<"">>) of
                            error -> ?ERROR_MSG("the to is invalid ~p~n", [ToU]);
                            JTo ->
                                Bid = list_to_binary("http" ++ binary_to_list(randoms:get_string()) ++ integer_to_list(qtalk_public:get_exact_timestamp())),
                                Packet = make_message_packet(Type,Body,Extend_Info,Msg_Type, Bid),
                                ejabberd_router:route(JFrom,JTo,Packet),
                                Packet1 = make_carbon_packet(JFrom, JTo, Packet),
                                ejabberd_router:route(JTo,JFrom,Packet1)
                        end
                    end,To),

                    http_utils:gen_result(true, 0, <<"success">>)
            end
    end.

send_muc_msg(_Server,User,Room,Host,Domain,Body,Extend_Info,Msg_Type) ->
    case Room of
        [{obj,[{"User",Muc}]}] ->
            case Body of
                undefined -> http_utils:gen_result(false, 1, <<"Message Body is Null">>);
                _ ->
                    case catch qtalk_public:check_user_reg_muc_local(Muc,Domain,User,Host) of
                    true ->
                        case  jlib:make_jid(User,Host,<<"">>) of
                            error -> http_utils:gen_result(false, 1, <<"From make jid error">>);
                            JFrom ->
                                case jlib:make_jid(Muc,Domain,<<"">>) of
                                error -> http_utils:gen_result(false, 1, <<"Muc make jid error">>);
                                JTo ->
                                    Packet = qtalk_public:make_message_packet(<<"groupchat">>,Body,Extend_Info,Msg_Type),
                                    ejabberd_router:route(JFrom,JTo,Packet),
                                    http_utils:gen_result(true, 0, <<"success">>)
                                end
                        end;
                    _ -> http_utils:gen_result(false, 1, <<"User not in Room">>)
                    end
            end;
        _ -> http_utils:gen_result(false, 1, <<"To not suit">>)
    end.

make_message_packet(Type,Msg,Extend_Info,undefined, Bid) ->
    Now = qtalk_public:get_exact_timestamp(),
    fxml:to_xmlel(
            {xmlel  ,<<"message">>, [{<<"type">>,Type}, {<<"msec_times">>, integer_to_binary(Now)}],
                [{xmlel,<<"body">>,[{<<"id">>,Bid},{<<"msgType">>,<<"1">>},{<<"extendInfo">>,Extend_Info}],[{xmlcdata, Msg}]}]});
make_message_packet(Type,Msg,Extend_Info,Msg_Type, Bid) ->
    Now = qtalk_public:get_exact_timestamp(),
    fxml:to_xmlel(
            {xmlel  ,<<"message">>, [{<<"type">>,Type}, {<<"msec_times">>, integer_to_binary(Now)}],
                [{xmlel,<<"body">>,[{<<"id">>,Bid},{<<"msgType">>,Msg_Type},{<<"extendInfo">>,Extend_Info}],[{xmlcdata, Msg}]}]}).

make_carbon_packet(From, To, Packet) ->
    #xmlel{name = Name, attrs = Attrs, children = Els} = Packet,
    Attrs1 = jlib:replace_from_to_attrs(jlib:jid_to_string(To),jlib:jid_to_string(From), Attrs),
    Attrs2 = lists:append([Attrs1,[{<<"carbon_message">>,<<"true">>}]]),
    #xmlel{name = Name, attrs = Attrs2, children = Els }.
