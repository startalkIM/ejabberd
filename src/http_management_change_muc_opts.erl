-module(http_management_change_muc_opts).

-export([handle/1]).
-include("ejabberd.hrl").
-include("logger.hrl").

-record(muc_online_room, {name_host = {<<"">>, <<"">>} :: {binary(), binary()} | '$1' | {'_', binary()} | '_',
                            pid = self() :: pid() | '$2' | '_' | '$1'}).

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
    ?DEBUG("the opts is ~p~n", [MucOpts]),

    case MucOpts of
        error -> http_utils:gen_result(false, <<"-1">>, <<"Muc not exit or Muc is temp Muc">>);
        _ ->
            NewOpts = do_update_muc_opts(Server, MucOpts, Args),
            mod_muc:store_room(Server,ConferenceServer,MucName,NewOpts),
            case mod_muc_redis:get_muc_room_pid(MucName,ConferenceServer) of
                [] -> http_utils:gen_result(true, <<"0">>, <<"Muc set opt ok,muc not start">>);
                [M] ->
                    Pid = M#muc_online_room.pid,
                    Pid ! shutdown,
                    http_utils:gen_result(true, <<"0">>, <<"Muc set opt ok">>)
            end
    end.

do_update_muc_opts(Server, Opts, Args) ->
    KeyList = ["public","persistent","max_users"],
    NewOpts = lists:foldl(fun(Opt,Acc) ->
            case proplists:get_value(Opt,Args) of
            undefined -> Acc;
            <<>> -> Acc;
            V ->
                NewAcc = proplists:delete(list_to_atom(Opt),Acc),
                case Opt of
                    "max_users" -> lists:append(NewAcc,[{list_to_atom(Opt),binary_to_integer(V)}]);
                    _ -> lists:append(NewAcc,[{list_to_atom(Opt),list_to_atom(binary_to_list(V))}])
                end
            end
    end, Opts, KeyList),

    case proplists:get_value( "affiliations",Args) of
        undefined -> NewOpts;
        <<>> -> NewOpts;
        V ->
            AffOpts = case proplists:get_value(affiliations,NewOpts) of
                undefined -> [{{<<"none">>,Server,<<>>},{owner,<<>>}}];
                Vo -> Vo
            end,
            OptsMvAff = proplists:delete(affiliations,NewOpts),
            NewAffOpts = lists:flatmap(fun({{U,S,R},{Aff,J}}) -> 
                case Aff of
                    owner -> [{{V,S,R},{Aff,J}}];
                    _ -> [{{U,S,R},{Aff,J}}]
                end
            end, AffOpts),
            lists:append(OptsMvAff,[{affiliations,NewAffOpts}])
    end.
