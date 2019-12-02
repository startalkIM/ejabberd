-module(http_muc_add_user).

-export([handle/1]).
-include("ejabberd.hrl").
-include("logger.hrl").

handle(Req) ->
    {Method, Req1} = cowboy_req:method(Req),
    case Method of 
        <<"POST">> -> do_handle(Req1);
        _ -> http_utils:cowboy_req_reply_json(http_utils:gen_fail_result(1, <<Method/binary, " is not disable">>), Req1)
    end.

do_handle(Req) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    case rfc4627:decode(Body) of
    {ok,{obj,Args},[]}  ->
        Res = add_muc_users(Args),
        http_utils:cowboy_req_reply_json(Res, Req1);
    _ ->
        http_utils:cowboy_req_reply_json(http_utils:gen_fail_result(1,<<"Json format error.">>), Req1)
    end.

add_muc_users(Args) ->
    Servers = ejabberd_config:get_myhosts(),
    LServer = lists:nth(1,Servers),
    add_muc_users(LServer,Args).

add_muc_users(Server,Args) ->
    Muc_id = http_muc_session:get_value("muc_id",Args,<<"">>),
    Muc_owner = http_muc_session:get_value("muc_owner",Args,<<"">>),
    Host = http_muc_session:get_value("muc_owner_host",Args,Server),
    Muc_member = http_muc_session:get_value("muc_member",Args,<<"">>),
    Domain = http_muc_session:get_value("muc_domain",Args,<<"">>),
    Muc_Towner = http_muc_session:get_value("muc_true_owner",Args,Muc_owner),
    case http_muc_session:check_muc_exist(Server,Muc_id) of
    true ->
        Muc_jid = jlib:make_jid(Muc_id,Domain,<<"">>),
        Invite_Jid = jlib:make_jid(Muc_Towner,Host,<<"">>),
        case Muc_member of 
        <<"">> -> ok;
        _ -> 
            IQ_Packet = http_muc_session:make_invite_iq(Muc_member, qtalk_public:get_default_host()),
            catch ejabberd_router:route(Invite_Jid,Muc_jid,IQ_Packet)
        end,
        http_utils:gen_result(true, <<"0">>,<<"">>,<<"sucess">>);
    _ ->
        http_utils:gen_result(true, <<"1">>,<<"">>,<<"failed">>)
    end.
