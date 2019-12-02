-module(http_management_get_muc_opts).

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
        Res = change_muc_opts(Args),
        http_utils:cowboy_req_reply_json(Res, Req1);
    _ ->
        http_utils:cowboy_req_reply_json(http_utils:gen_fail_result(1,<<"Json format error.">>), Req1)
    end.

change_muc_opts(Args) ->
    MucName = proplists:get_value("muc", Args),
    ConferenceServer = proplists:get_value("domain", Args),
    [_, Server] = binary:split(ConferenceServer, <<".">>, []),
    SName = ejabberd_sql:escape(MucName),
    SHost = ejabberd_sql:escape(ConferenceServer),

    MucOpts = case catch qtalk_sql:get_muc_opts(Server, SName, SHost) of
        {selected,_, [[Opts]]} -> mod_muc:opts_to_binary(ejabberd_sql:decode_term(Opts));
        _ -> error
    end,

    case MucOpts of
        error -> http_utils:gen_result(false, <<"-1">>, <<"Muc not exit or Muc is temp Muc">>);
        _ ->
           MaxUsers = proplists:get_value(max_users, MucOpts),
           AffiliationsOpts = case proplists:get_value(affiliations, MucOpts) of
               undefined -> [];
               V -> V
           end,
           Affiliations = lists:foldl(fun({{U, S, _R}, {Aff,_J}}, Acc) ->
              case Aff of
                  owner -> [{obj, [{"user", U}, {"host", S}]}|Acc];
                  _ -> Acc
              end
           end, [], AffiliationsOpts),

           MucShowName = case ejabberd_sql:sql_query(Server, <<"select show_name from muc_vcard_info where muc_name='", MucName/binary, "@", ConferenceServer/binary, "';">>) of
               {selected, _, [[Name]]} -> Name;
               _ -> <<"">>
           end,

           http_utils:gen_result(true, <<"0">>,<<"">>, {obj, [{"mucid", <<MucName/binary, "@", ConferenceServer/binary>>}, {"maxusers", MaxUsers}, {"showname", MucShowName}, {"owner", Affiliations}]})
    end.
