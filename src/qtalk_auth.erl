%%%----------------------------------------------------------------------
%%%%%% File    : ejabberd_auth_sql.erl
%%%%%% Purpose : auth qtalk key
%%%%%%----------------------------------------------------------------------
%%%

-module(qtalk_auth).

-export([check_user_password/3]).

-include("ejabberd.hrl").
-include("logger.hrl").

check_user_password(Host, User, Password) ->
    case catch rsa:dec(base64:decode(Password)) of
        Json when is_binary(Json) ->
            R = case qtalk_sql:get_password_salt_by_host(Host, User) of
                {selected,_, [[Password1, Salt]]} ->
                    {ok,{obj,L},[]} = rfc4627:decode( Json),
                    Pass = proplists:get_value("p",L),
                    Key = proplists:get_value("mk",L),
                    case Key of
                        undefined -> ok;
                    _->
                        NewKey = qtalk_public:concat(User,<<"@">>,Host),
                        catch set_user_mac_key(Host,NewKey,Key)
                    end,
		    ?INFO_MSG("the password is ~p~n", [{User, Host, Password1,Pass, Salt}]),
                    do_check_host_user(Password1,Pass, Salt);
                _ -> false
           end,
           case R of
               false -> ?ERROR_MSG("auth fail for ~p~n", [{User, Host, Json}]), false;
               true -> true
           end;
        _ ->
           ?INFO_MSG("the password is ~p~n", [{User, Host, Password}]),
           case do_check_host_user_auth(Host, User, user_type(Password)) of
               true -> true;
               R ->
                   ?ERROR_MSG("auth fail for ~p~n", [{User, Host, Password, R}]),
		   R
           end
    end.

do_check_host_user_auth(Host, User, {nauth, L}) ->
    Pass = proplists:get_value("p",L),
    Key = proplists:get_value("mk",L),

    Url = ejabberd_config:get_option(auth_url,fun(Url)-> binary_to_list(Url) end,"http://127.0.0.1:8081/im_http_service/corp/newapi/auth/checktoken.qunar"),
    Header = [],
    Type = "application/json",
    HTTPOptions = [],
    Options = [],
    Body = rfc4627:encode({obj, [{"u", User}, {"h", Host}, {"t", Pass}]}),
    case catch http_client:http_post(Url,Header,Type,Body,HTTPOptions,Options) of
    {ok, {_Status,_Headers, Res}} ->
        case rfc4627:decode(Res) of
            {ok,{obj,Args},_} ->
                case proplists:get_value("ret", Args) of
                    R when R =:= true; R =:= "true" ->
                        case Key of
                            undefined -> ok;
                            _ ->
                                NewKey = qtalk_public:concat(User,<<"@">>,Host),
                                catch set_user_mac_key(Host,NewKey,Key)
                        end,
                        true;
                    _ -> ?ERROR_MSG("auth password fail for ~p~n", [Res]), {false, <<"nauth-expired">>}
                end;
             _ -> ?ERROR_MSG("auth password fail for ~p~n", [Res]), {false, <<"nauth-http-response-error">>}
        end;
      R -> ?ERROR_MSG("auth password fail for ~p~n", [R]), {false, <<"nauth-http-fail">>}
    end;
do_check_host_user_auth(_Host, User, {anony, [Plat, UUID, Token, Password]}) ->
    ?DEBUG("check_password the user type is client and anonymous auth, the user is ~p, token is ~p~n", [User, [Plat, UUID, Token]]),
    case mod_redis:str_get(1, <<Plat/binary, User/binary>>) of
        {ok, Password} -> mod_redis:expire_time(1, <<Plat/binary, User/binary>>, 86400*7), true;
        _ ->
            ?ERROR_MSG("check anonymous unvalid fail for client ~p:~p~n", [User, {Plat, UUID, Token, Password}]),
            catch monitor_util:monitor_count(<<"login_fail_client_anonymous_token_unvalid">>, 1),
            {false, "out_of_date"}
    end;
do_check_host_user_auth(_Host, User, {'no-surport', Password}) ->
    ?ERROR_MSG("got a no sourport login password: ~p~n", [{User, Password}]),
    {false, "no_surport"}.

user_type(Password) ->
    case rfc4627:decode(Password) of
        {ok, {obj, [{"anony", {obj, List}}]}, []} ->
            Plat = proplists:get_value("plat", List),
            Token = proplists:get_value("token", List),
            UUID = proplists:get_value("uuid", List),
            {anony, [Plat, UUID, Token, Password]};
	{ok, {obj, [{"nauth", {obj, List}}]}, []} -> {nauth, List};
        _ ->
            {'no-surport', Password}
    end.

do_check_host_user(<<"CRY:", _>>, _, null) -> false;
do_check_host_user(<<"CRY:", Password/binary>>,Pass, Salt) ->
    P1 = do_md5(Pass),
    P2 = do_md5(<<P1/binary, Salt/binary>>),
    do_md5(P2) =:= Password;
do_check_host_user(Password, Password, _) -> true;
do_check_host_user(_, _, _) -> false.


do_md5(S) ->
     p1_sha:to_hexlist(erlang:md5(S)).

set_user_mac_key(_Server,User,Key) ->
    UTkey = str:concat(User,<<"_tkey">>),
    case mod_redis:redis_cmd(2,["HKEYS",UTkey]) of
    {ok,L} when is_list(L) ->
        case lists:member(Key,L) of
        true ->
            lists:foreach(fun(K) ->
                    catch mod_redis:hash_del(2,UTkey,K) end,L -- [Key]);
        _ ->
            lists:foreach(fun(K) ->
                    catch mod_redis:hash_del(2,UTkey,K) end,L -- [Key]),
            catch mod_redis:hash_set(2,UTkey,Key,qtalk_public:get_timestamp())
        end;
    _ ->
        catch mod_redis:hash_set(2,UTkey,Key,qtalk_public:get_timestamp())
    end.
