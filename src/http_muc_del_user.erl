-module(http_muc_del_user).

-export([handle/1]).
-include("ejabberd.hrl").
-include("logger.hrl").

handle(Req) ->
    {Method, Req1} = cowboy_req:method(Req),
    case Method of 
        <<"POST">> -> do_handle(Req1);
        _ -> http_utils:cowboy_req_reply_json(http_utils:gen_fail_result(1, <<Method/binary, " is not disable">>), Req1)
    end.

do_handle(Req)->
    {ok, Body, Req1} = cowboy_req:body(Req),
    case rfc4627:decode(Body) of
    {ok,{obj,Args},[]}  ->
        Res = del_muc_users(Args),
        http_utils:cowboy_req_reply_json(Res, Req1);
    _ ->
        http_utils:cowboy_req_reply_json(http_utils:gen_fail_result(1,<<"Json format error.">>), Req1)
    end.

del_muc_users(Args) ->
    Servers = ejabberd_config:get_myhosts(),
    LServer = lists:nth(1,Servers),
    do_del_muc_users(LServer,Args).

do_del_muc_users(Server,Args) ->
    Muc_id = http_muc_session:get_value("muc_id",Args,<<"">>),
    Muc_owner =	http_muc_session:get_value("muc_owner",Args,<<"">>),
    Host = http_muc_session:get_value("muc_owner_host",Args,Server),
    Muc_member = http_muc_session:get_value("muc_member",Args,<<"">>),
    Domain = http_muc_session:get_value("muc_domain",Args,<<"">>),
    Muc_Towner = http_muc_session:get_value("muc_true_owner",Args,Muc_owner),
    Owner = jlib:jid_to_string({Muc_Towner,Host,<<"">>}),
    case mod_muc:check_muc_owner(Server,Muc_id,Owner) of
    true ->
        case jlib:make_jid(Muc_id,Domain,<<"">>) of
        error -> ok;
        To ->
            Packet = http_muc_session:make_del_register_muc_iq(),
            lists:foreach(fun(U) ->
                From = jlib:make_jid(U,Server,<<"">>),
                catch ejabberd_router:route(From,To,Packet) end ,Muc_member)
       	end,
        http_utils:gen_result(true, <<"0">>,<<"">>,<<"sucess">>);
    _ ->
        http_utils:gen_result(false, <<"1">>,<<"owner error">>,<<"failed">>)
    end.
