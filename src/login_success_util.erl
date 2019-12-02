-module(login_success_util).

-export([do_process/1,
         close_stat/5
                ]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

do_process(User) ->
    ?DEBUG("the login success user is ~p~n", [User]).

close_stat(User, Server, Resource, {IP, _}, {{MegaSecs, Secs, _}, _}) when User =/= [], Resource =/= <<"">> ->
    Timestamp = qtalk_public:format_time(MegaSecs * 1000000 + Secs),
    {NMSec, NSec, _} = erlang:now(),
    Now = qtalk_public:format_time(NMSec * 1000000 + NSec),
    Ip = inet_parse:ntoa(IP),
    Type = "server",
    Platform = get_platform(Resource),

    catch ejabberd_sql:sql_query([<<"insert into login_data (username, host, resource, platform, ip, login_time, logout_at, record_type) values ('">>, User,<<"','">>, Server, <<"','">>,
                                Resource, <<"', '">>, Platform, <<"', '">>, Ip, <<"', '">>, Timestamp, <<"', '">>, Now, <<"', '">>, Type, <<"');">>]);
close_stat(User, Server, Resource, Ip, Time) ->
    ?DEBUG("the login success user is ~p~n", [{User, Server, Resource, Ip, Time}]),
    ok.

get_platform(Resource) ->
    get_platform(Resource, [{<<"_P[Android">>, <<"Android">>},
                            {<<"_P[iOS">>, <<"iOS">>},
                            {<<"_P[PC">>, <<"PC">>},
                            {<<"_P[Mac">>, <<"Mac">>},
                            {<<"_P[LINUX">>, <<"LINUX">>}]).

get_platform(_Resource, []) -> <<"unknown">>;
get_platform(Resource, [{Pattern, Plat}|Rest]) ->
    case binary:match(Resource, Pattern) of
        nomatch -> get_platform(Resource, Rest);
        _ -> Plat
    end.
