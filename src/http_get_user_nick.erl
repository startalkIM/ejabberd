-module(http_get_user_nick).

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
        Res = get_user_nick(Args),
        http_utils:cowboy_req_reply_json(Res, Req1);
    _ ->
        http_utils:cowboy_req_reply_json(http_utils:gen_fail_result(1,<<"Json format error.">>), Req1)
    end.

get_user_nick(Args) ->
    User = proplists:get_value("ID",Args),
    Host = proplists:get_value("Domain",Args),

    case catch ejabberd_sql:sql_query(Host, [<<"select host_users.user_id, host_users.user_name, vcard_version.url from host_users left join host_info on host_users.host_id = host_info.id left join vcard_version on host_users.user_id=vcard_version.username and host_info.host=vcard_version.host where host_users.user_id='">>, User, <<"' and host_info.host='">>, Host, <<"';">>]) of
        {selected, _, [[User, Name, Url]]} -> 
            http_utils:gen_result(true, 0,<<"">>, {obj, [{"Nick", Name}, {"Vcard", Url}]});
        _ -> http_utils:gen_fail_result(1,<<"get info fail">>)
    end.

