-module(http_muc_users).

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
    {ok, Body, _} = cowboy_req:body(Req),
    case rfc4627:decode(Body) of
    {ok,{obj,Args},[]}  ->
        Res = get_muc_users(Args),
        http_utils:cowboy_req_reply_json(Res, Req);
    _ ->
        http_utils:cowboy_req_reply_json(http_utils:gen_fail_result(1,<<"Json format error.">>), Req)
    end.

get_muc_users(Args) ->
    Servers = ejabberd_config:get_myhosts(),
    LServer = lists:nth(1,Servers),
    create_muc(LServer,Args).

create_muc(Server,Args) ->
    MucId = http_muc_session:get_value("muc_id",Args,<<"">>),
    _Domain = http_muc_session:get_value("muc_domain",Args,<<"">>),

    Infos = case catch ejabberd_sql:sql_query(Server, [<<"SELECT q.username, q.host, case when b.user_name is null then q.username else b.user_name end FROM muc_room_users q LEFT JOIN host_info a ON q.host = a.host LEFT JOIN host_users b ON q.username = b.user_id WHERE muc_name = '">>,MucId,<<"'">>]) of
        {selected,_,Res} when is_list(Res)  ->
            lists:map(fun([U, H, N]) ->
                {obj,[{"U",U},{"H", H}, {"N",N}]}
            end, Res);
        _ -> []
    end,
    http_utils:gen_result(true, 0,<<"">>, Infos).
