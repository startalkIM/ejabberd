-module(user_relation).

-export([get_user_friend_opts/3,get_user_friends/3, del_friend/6, get_user_friend_opts/4, get_user_friends/4, add_user_friend_opts/7, add_user_friend/4]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-record(friend_opts,{user,rec_msg_flag,vld_friend_flag,validate_quetion,validate_answer,version}).

get_user_friend_opts(Server, Username, Userhost, _Key) ->
    get_user_friend_opts(Server, Username, Userhost).

get_user_friend_opts(Server,Username, Userhost) ->
    Sql = [<<"select rec_msg_opt,vld_friend_opt,validate_quetion,validate_answer,vesion from user_relation_opts where username = '">>,Username, <<"' and userhost = '">>, Userhost, <<"';">>],
    case catch ejabberd_sql:sql_query(Server, Sql) of
        {selected, _ , [[R,V,Q,A,Ver]]} ->
            Friend_opts = #friend_opts{user = Username ,rec_msg_flag = R,vld_friend_flag = V,validate_quetion = Q,validate_answer = A,version = Ver},
            Friend_opts;
        _ -> []
    end.

get_user_friends(Server, User, Userhost, Key) ->
    Rslt = get_user_friends(Server,User, Userhost),

    case Key of
        size -> length(Rslt);
        _ -> Rslt
    end.

get_user_friends(Server,User, Userhost) ->
    case catch ejabberd_sql:sql_query(Server, [<<"select friend,host,relationship,version from user_friends where username = '">>,User,<<"' and userhost = '">>, Userhost, <<"';">>]) of
        {selected,[<<"friend">>,<<"host">>,<<"relationship">>,<<"version">>],[]} ->
            [];
        {selected, _ , SRes} when is_list(SRes) ->
            lists:flatmap(fun([F,H,R,V]) ->    
                case R of
                    <<"1">> -> [{{F,H},V}];
                    _ -> [] 
                end
            end, SRes);
        _ -> []
    end.

del_friend(Server, From, FromHost, To, Domain, _Version) ->
    case catch ejabberd_sql:sql_query(Server, [<<"update user_friends set relationship = 0 where username = '">>,From,<<"' and userhost = '">>, FromHost, <<"' and friend = '">>,To, <<"' and host = '">>, Domain, <<"';">>]) of
         {updated,1} -> true;
         _ -> true
    end.

add_user_friend_opts(Server,User,Userhost,Mode,Question,Answer,Version) ->
    Sql = <<"insert into user_relation_opts(username,userhost,vld_friend_opt,validate_quetion,validate_answer,vesion) values ('">>,
    case catch ejabberd_sql:sql_query(Server, [Sql, User,<<"',">>, Userhost, <<"','">>,Mode,<<",'">>,Question,<<"','">>,Answer,<<"',">>,Version,<<");">>]) of
        {updated,1} -> true;
        _ ->
            case Mode of 
                <<"2">> ->
			case catch ejabberd_sql:sql_query(Server, [<<"update user_relation_opts set vld_friend_opt = 2, validate_quetion = '">>,Question,
                                                                    <<"', validate_answer = '">>,Answer,<<"' where username = '">>,User, <<"' and userhost = '">>, Userhost, <<"';">>]) of
                            {updated,1} -> true;
                            _ -> false
                        end;
                _ ->
                    case catch ejabberd_sql:sql_query(Server, [<<"update user_relation_opts set vld_friend_opt = ">>,Mode,<<", validate_quetion = '">>,Question,
                                                                <<"', validate_answer = '">>,Answer,<<"' where username = '">>,User,<<"' and userhost = '">>, Userhost, <<"';">>]) of
                        {updated,1} -> true;
                        _ -> false
                    end
            end
    end.

add_user_friend(Server,From,To,Version) ->
    case catch ejabberd_sql:sql_query(Server, [<<"insert into user_friends(username,userhost,friend,host,relationship,version) values ('">>,From#jid.luser,<<"','">>, From#jid.lserver,<<"','">>, To#jid.luser,<<"','">>,To#jid.lserver,<<"',1,">>,Version,<<");">>]) of
        {updated,1} -> true;
        _ ->
            case catch ejabberd_sql:sql_query(Server, [<<"update user_friends set relationship = 1,version = version +1  where username = '">>,From#jid.luser, <<"' and userhost = '">>, From#jid.lserver, <<"' and friend = '">>,To#jid.luser, <<"' and host = '">>, To#jid.lserver, <<"';">>]) of
                {updated,1} -> true;
                _ -> false
            end
    end.
