-module(http_muc_create).

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
        Res = create_muc(Args),
        http_utils:cowboy_req_reply_json(Res, Req1);
    _ ->
        http_utils:cowboy_req_reply_json(http_utils:gen_fail_result(1,<<"Json format error.">>), Req1)
    end.

create_muc(Args) ->
    Servers = ejabberd_config:get_myhosts(),
    LServer = lists:nth(1,Servers),
    create_muc(LServer,Args).

create_muc(Server,Args) ->
    Muc_name = http_muc_session:get_value("muc_name",Args,<<"">>),
    Muc_id = http_muc_session:get_value("muc_id",Args,<<"">>),
    Muc_owner = http_muc_session:get_value("muc_owner",Args,<<"">>),
    Host = http_muc_session:get_value("muc_owner_host",Args,Server),
    Domain = http_muc_session:get_value("muc_domain",Args,<<"">>),
    Desc = http_muc_session:get_value("muc_desc",Args,<<"">>),
    Muc_Towner = http_muc_session:get_value("muc_true_owner",Args,Muc_owner),

    case http_muc_session:check_muc_exist(Server,Muc_id) of
        false ->
        Packet = http_muc_session:make_create_muc_iq(),
        case jlib:make_jid(Muc_id,Domain,<<"">>) of
	    error -> http_utils:gen_result(false, <<"-1">>,<<"">>,<<"error">>);
            To_owner ->
                Owner = jlib:make_jid(Muc_Towner,Host,<<"">>),
                catch ejabberd_router:route(Owner,To_owner, Packet),
    	        qtalk_sql:insert_muc_vcard_info(Server,qtalk_public:concat(Muc_id,<<"@">>,Domain),Muc_name,<<"">>,Desc,<<"">>,<<"1">>),
                Persistent_packet = http_muc_session:make_muc_persistent(),
                http_muc_vcard_presence:send_update_vcard_presence(Muc_id),
                catch ejabberd_router:route(jlib:make_jid(Muc_Towner,Host,<<"">>),jlib:jid_replace_resource(To_owner,<<"">>), Persistent_packet)	
    	end,
        http_utils:gen_result(true, <<"0">>,<<"">>,<<"sucess">>);
    _ ->
        http_utils:gen_result(true, <<"3">>,<<"">>,<<"failed">>)
    end.
