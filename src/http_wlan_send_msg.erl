-module(http_wlan_send_msg).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-export([http_send_message/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

init(_Transport, Req, []) -> {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req1} = cowboy_req:method(Req),
    case Method of 
        <<"POST">> ->
            HasBody = cowboy_req:has_body(Req1),
            {ok, Req2} = post_echo(Method, HasBody, Req1),
            {ok, Req2, State};
        _ ->
            {ok,Req2} = echo(Req1),
            {ok, Req2, State}
    end.
    	
post_echo(<<"POST">>, true, Req) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    case rfc4627:decode(Body) of
        {ok, {obj,Args},[]} -> 
            Res = http_send_message(Args),
            cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], Res, Req1);
        _ -> cowboy_req:reply(200, [{<<"content-type">>, <<"text/json; charset=utf-8">>}], <<"Josn parse error">>, Req1)
    end;
post_echo(_, _, Req) -> cowboy_req:reply(405, Req).

echo(Req) -> cowboy_req:reply(400, [], <<"fail">>, Req).

terminate(_Reason, _Req, _State) -> ok.
	
http_send_message(Json) ->
    Servers = ejabberd_config:get_myhosts(),
    Server = lists:nth(1,Servers),
    http_send_message(Server,Json).

http_send_message(Server,Args)->
    From = proplists:get_value("from",Args),
    To = proplists:get_value("to",Args),
    Body  = proplists:get_value("body",Args),
    Type  = proplists:get_value("type",Args),
    Msg_Type  = proplists:get_value("msg_type",Args,<<"1">>),
    Host = proplists:get_value("host",Args,Server),
    Domain  = proplists:get_value("domain",Args),
    Extend_Info  =	proplists:get_value("extend_info",Args,<<"">>),
    case Type of 
        <<"groupchat">> -> send_muc_msg(Server,From,To,Host,Domain,Body,Extend_Info,Msg_Type);
        _ -> send_chat_msg(Server,From,To,Host,Body,Msg_Type,Extend_Info)
    end.

send_chat_msg(_Server,_From,undefined,_Host,_Body,_Msg_Type,_Extend_Info) -> http_utils:gen_result(false, 1,<<"To no suit">>,<<"">>);
send_chat_msg(_Server,_From,_To,_Host,undefined,_Msg_Type,_Extend_Info) -> http_utils:gen_result(false, 1,<<"Body is null">>,<<"">>);
send_chat_msg(Server,From,To,Host,Body,Msg_Type,Extend_Info) ->
   JFrom =  case catch str:str(From,<<"@">>) of
       0 -> jlib:make_jid(From,Server,<<"">>);
        _ -> jlib:string_to_jid(From)
   end,
   case JFrom of 
       error -> http_utils:gen_result(false, 1,<<"Make from jid error">>,<<"">>);
       _ ->
           SendRes = lists:flatmap(fun({obj,[{"user",ToU}]}) ->
               case jlib:make_jid(ToU,Host,<<"">>) of 
                   error -> [<<",">>,ToU];
                   JTo ->
                        Bid = list_to_binary("http" ++ binary_to_list(randoms:get_string()) ++ integer_to_list(qtalk_public:get_exact_timestamp())),
                        Packet = make_message_packet(<<"chat">>,Body,Extend_Info,Msg_Type, Bid),
                        ejabberd_router:route(JFrom,JTo,Packet),
                        Packet1 = make_carbon_packet(JFrom, JTo, Packet),
                        ejabberd_router:route(JTo,JFrom,Packet1),
                        []
               end
           end,To),
           case length(SendRes) of 
               0 -> http_utils:gen_result(true, 0,<<"Send msg sucess">>,<<"">>);
               _ ->
                   [_|Res] = SendRes,
                   http_utils:gen_result(false, 1,<<"Send msg error">>,list_to_binary(Res))
           end
   end.

send_muc_msg(_Server,_User,_Room,_Host,_Domain,undefined,_Extend_Info,_Msg_Type) ->
	http_utils:gen_result(false, 1,<<"Body is null">>,<<"">>);
send_muc_msg(Server,User,Room,_Host,Domain,Body,Extend_Info,Msg_Type) ->
    case Room of
        [{obj,[{"user",Muc}]}] ->
            JFrom = case catch str:str(User,<<"@">>) of
                0 -> jlib:make_jid(User,Server,<<"">>);
                _ -> jlib:string_to_jid(User)
            end,
            case JFrom of
                error -> http_utils:gen_result(false, 1,<<"Make from jid error">>,<<"">>);
                _  ->
                    case jlib:make_jid(Muc,Domain,<<"">>) of 
                        error -> http_utils:gen_result(false, 1,<<"Make Muc jid error">>,<<"">>);
                        JTo ->
                            Packet = qtalk_public:make_message_packet(<<"groupchat">>,Body,Extend_Info,Msg_Type),
                            ejabberd_router:route(JFrom,JTo,Packet),
                            http_utils:gen_result(true, 0,<<"Send msg sucess">>,<<"">>)
                    end
            end;
        _ -> http_utils:gen_result(false, 1,<<"To no suit">>,<<"">>)
    end.

make_carbon_packet(From, To, Packet) ->
    #xmlel{name = Name, attrs = Attrs, children = Els} = Packet,
    Attrs1 = jlib:replace_from_to_attrs(jlib:jid_to_string(To),jlib:jid_to_string(From), Attrs),
    Attrs2 = lists:append([Attrs1,[{<<"carbon_message">>,<<"true">>}]]),
    #xmlel{name = Name, attrs = Attrs2, children = Els }.

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
