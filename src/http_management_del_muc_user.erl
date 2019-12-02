-module(http_management_del_muc_user).

-export([handle/1]).
-include("ejabberd.hrl").
-include("logger.hrl").

-record(muc_online_room,
           {name_host = {<<"">>, <<"">>} :: {binary(), binary()} | '$1' |{'_', binary()} | '_', pid = self() :: pid() | '$2' | '_' | '$1'}).
%%------------------------------------------
%% curl -ivvv -d '{"muc":"67e0587b3b1241438172ac895336bc63", "domain":"conference.ejabhost1", "max_users":"250", "affiliations":"lffan.liuxx"}' 'http://127.0.0.1:10050/qtalk/management/change_muc_opts'
%%------------------------------------------

handle(Req) ->
    {Method, _} = cowboy_req:method(Req),
    case Method of 
        <<"POST">> ->
            do_handle(Req);
        _ ->
            http_utils:cowboy_req_reply_json(http_utils:gen_fail_result(1, <<Method/binary, " is not disable">>), Req)
    end.

do_handle(Req)->
    {ok, Body, Req1} = cowboy_req:body(Req),
    case rfc4627:decode(Body) of
    {ok,{obj,Args},[]}  ->
        Res = del_muc_user(Args),
        http_utils:cowboy_req_reply_json(Res, Req1);
    _ ->
        http_utils:cowboy_req_reply_json(http_utils:gen_fail_result(1,<<"Json format error.">>), Req)
    end.

del_muc_user(Args) ->
    MucName = proplists:get_value("muc", Args),
    User = proplists:get_value("user", Args),
    Host = proplists:get_value("host", Args),
    ConferenceServer = proplists:get_value("domain", Args),
    case get_room_affiliation(MucName) of
        {User, Host} ->
            ?ERROR_MSG("~p is muc owner~n", [{MucName, User, Host, ConferenceServer}]),
            reset_muc_owner(MucName, ConferenceServer, User, Host);
        _ -> ok
    end,
    do_del_user(MucName, ConferenceServer, User, Host),
    http_utils:gen_result(true, <<"0">>, <<"Muc set ok">>).

get_room_affiliation(Room) ->
    case catch ejabberd_sql:sql_query([<<"select opts from muc_room ">>,<<"where name='">>, Room, <<"';">>]) of
        {selected, [<<"opts">>], [[Opts]]} ->
            get_affction_opts(ejabberd_sql:decode_term(Opts));
        _ -> undefined
    end.

get_affction_opts(Opts) ->
    Affs = proplists:get_value(affiliations, Opts),
    lists:foldl(fun({{U, S, _R}, {owner,<<"">>}}, _) ->
        {iolist_to_binary(U), iolist_to_binary(S)};
    (_, Acc) -> Acc
    end, undefined, Affs).

do_del_user(Muc, ConferenceServer, User, Host) ->
    case mod_muc_redis:get_muc_room_pid(Muc, ConferenceServer) of
        [] ->
            catch ejabberd_sql:sql_query([<<"delete from muc_room_users where username = '">>,User, <<"' and host = '">>, Host, <<"' and muc_name = '">>,Muc, <<"' and domain = '">>, ConferenceServer, <<"';">>]),
            catch ejabberd_sql:sql_query([<<"update user_register_mucs set registed_flag = '0', created_at = (now())::timestamp(3) where username = '">>, User, <<"' and muc_name = '">>, Muc, <<"' and domain = '">>, ConferenceServer, <<"' and host = '">>,Host,<<"';">>]);
        [M] ->
            JID = jlib:make_jid(User, Host, <<"">>),
            M#muc_online_room.pid ! {http_del_user,JID}
    end.

reset_muc_owner(Muc, ConferenceServer, User, Host) ->
    case catch ejabberd_sql:sql_query([<<"select username, host from muc_room_users where NOT (username = '">>,User, <<"' and host = '">>, Host, <<"') and muc_name = '">>,Muc, <<"' and domain = '">>, ConferenceServer, <<"' order by id limit 1;">>]) of
        {selected, _, [[U, _H]|_]} -> change_muc_opts([{"muc", Muc}, {"domain", ConferenceServer}, {"affiliations", U}]);
        _ -> ok
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
        error -> ?ERROR_MSG("Muc not exit or Muc is temp Muc~n", []);
        _ ->
            NewOpts = do_update_muc_opts(Server, MucOpts, Args),
            mod_muc:store_room(Server,ConferenceServer,MucName,NewOpts),
            case mod_muc_redis:get_muc_room_pid(MucName,ConferenceServer) of
                [] -> ?ERROR_MSG("muc not start~n", []);
                [M] ->
                    Pid = M#muc_online_room.pid,
                    gen_fsm:send_all_state_event(Pid,update_state)
            end
    end.

do_update_muc_opts(Server, Opts, Args) ->
    V = proplists:get_value("affiliations",Args),
    Aff_opts = case proplists:get_value(affiliations,Opts) of
        undefined -> [];
        Vo -> Vo
    end,
    Opts_mv_aff = proplists:delete(affiliations,Opts),
    New_aff_opts = lists:flatmap(fun({{U, _, _}, {_, _}}) when U =:= V -> [];
                             ({{_, _, _}, {owner, _}}) -> [];
                             ({{U,S,R},{Aff,J}}) -> [{{U,S,R},{Aff,J}}]
            end, Aff_opts),
    lists:append(Opts_mv_aff,[{affiliations,[{{V, Server, <<>>}, {owner, <<>>}}|New_aff_opts]}]).

