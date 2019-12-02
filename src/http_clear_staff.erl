-module(http_clear_staff).

-export([handle/1]).
-include("ejabberd.hrl").
-include("logger.hrl").

-record(muc_online_room,
                   {name_host = {<<"">>, <<"">>} :: {binary(), binary()} | '$1' |{'_', binary()} | '_', pid = self() :: pid() | '$2' | '_' | '$1'}).

handle(Req) ->
    {Method, Req1} = cowboy_req:method(Req),
    case Method of 
        <<"POST">> -> do_handle(Req1);
        _ -> http_utils:cowboy_req_reply_json(http_utils:gen_fail_result(1, <<Method/binary, " is not disable">>), Req1)
    end.

do_handle(Req)->
    {ok, Body, Req1} = cowboy_req:body(Req),
    case rfc4627:decode(Body) of
        {ok, {obj, Args},[]} ->
            Host = proplists:get_value("host",Args),
            Users = proplists:get_value("users",Args),
            clear_staff(Host, Users),
            ?INFO_MSG("the params is ~p~n", [{Host, Users}]),
            http_utils:cowboy_req_reply_json(http_utils:gen_success_result(), Req1);
        _ ->
            http_utils:cowboy_req_reply_json(http_utils:gen_fail_result(1, <<"Josn parse error">>), Req1)
    end.

clear_staff(_, []) -> ok;
clear_staff(Host, [User|Users]) ->
    case catch ejabberd_sql:sql_query([<<"select username,muc_name from muc_room_users where username = '", User/binary, "';">>]) of 
        {selected,[<<"username">>,<<"muc_name">>],Res} when is_list(Res) ->
            ?INFO_MSG("the mucs is ~p~n", [Res]),
            lists:foreach(fun([U,M]) ->
                case jlib:make_jid(U, Host, <<"">>) of
                    error -> ?INFO_MSG("Make User Jid Error ~p ~n",[U]);
                    JID ->
                        ServerHost =  str:concat(<<"conference.">>, Host),
                        case mod_muc_redis:get_muc_room_pid(M,ServerHost) of
                            [] ->
                                ?INFO_MSG("the JID is ~p~n", [{JID, ServerHost,U, M}]),
                                qtalk_public:clear_ets_muc_room_users(M, U, Host),
                                ejabberd_sql:sql_query([<<"delete from muc_room_users where username = '">>,U,<<"' and muc_name = '">>, M, <<"' and domain = '">>, ServerHost, <<"';">>]),
                                ejabberd_sql:sql_query(Host, [<<"delete from user_register_mucs where username = '">>,U,<<"' and muc_name = '">>,M,<<"';">>]);
                            [Muc] ->
                                ?INFO_MSG("Remove dimission User ~p ,Muc ~p ~n",[U,M]),
                                Muc#muc_online_room.pid ! {http_del_user,JID}
                        end
                end
            end, Res);
        O -> ?ERROR_MSG("the fail res is ~p~n", [O]), ok
    end,

    clear_staff(Host, Users).
