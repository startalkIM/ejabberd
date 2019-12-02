%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2015-2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 11 Mar 2015 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(ejabberd_sm_redis).

-behaviour(ejabberd_config).

-behaviour(ejabberd_sm).

-export([init/0, set_session/1, delete_session/4,
	 get_sessions/0, get_sessions/1, get_sessions/2,
	 get_sessions/3, opt_type/1]).

-export([get_vh_session_number/1]).

-define(SESSION_REDIS_TABLE,7).

-include("ejabberd.hrl").
-include("ejabberd_sm.hrl").
-include("logger.hrl").
-include("jlib.hrl").


%%%===================================================================
%%% API
%%%===================================================================
-spec init() -> ok | {error, any()}.
init() ->
    clean_table().

-spec set_session(#session{}) -> ok.
set_session(Session) ->
    T = term_to_binary(Session),
    {U, S, R} = Session#session.usr,
    Show = Session#session.show,
    USKey = us_to_key(Session#session.us),
    USOKey = us_to_okey(Session#session.us),
    SIDKey = sid_to_key(Session#session.sid),
    ServKey = server_to_key(element(2, Session#session.us)),
    ServOKey = server_to_okey(element(2, Session#session.us)),
    USSIDKey = us_sid_to_key(Session#session.us, Session#session.sid),
    Node = list_to_binary(atom_to_list(node())),
    PSession = rfc4627:encode({obj, [{"r", R}, {"n", Node}, {"f", Show}]}),
    SPSession = rfc4627:encode({obj, [{"u", U}, {"s", S}, {"r", R}, {"n", Node}, {"f", Show}]}),
    case mod_redis:qp(?SESSION_REDIS_TABLE,[["HSET", USKey, SIDKey, T],
			    ["HSET", ServKey, USSIDKey, T], ["HSET", USOKey, SIDKey, PSession], ["HSET", ServOKey, USSIDKey, SPSession]]) of
	[{ok, _}, {ok, _}, {ok, _}, {ok, _}] ->
	    ok;
	Err ->
	    ?ERROR_MSG("failed to set session for redis: ~p", [Err])
    end.

-spec delete_session(binary(), binary(), binary(), sid()) ->
			    {ok, #session{}} | {error, notfound}.
delete_session(LUser, LServer, _LResource, SID) ->
    USKey = us_to_key({LUser, LServer}),
    case mod_redis:q(?SESSION_REDIS_TABLE,["HGETALL", USKey]) of
	{ok, Vals} ->
	    Ss = decode_session_list(Vals),
	    case lists:keyfind(SID, #session.sid, Ss) of
		false ->
		    {error, notfound};
		Session ->
                    USOKey = us_to_okey({LUser, LServer}),
		    SIDKey = sid_to_key(SID),
		    ServKey = server_to_key(element(2, Session#session.us)),
		    ServOKey = server_to_okey(element(2, Session#session.us)),
		    USSIDKey = us_sid_to_key(Session#session.us, SID),
		    mod_redis:qp(?SESSION_REDIS_TABLE,[["HDEL", USKey, SIDKey],["HDEL", USOKey, SIDKey],
				       ["HDEL", ServKey, USSIDKey], ["HDEL", ServOKey, USSIDKey]]),
		    {ok, Session}
	    end;
	Err ->
	    ?ERROR_MSG("failed to delete session from redis: ~p", [Err]),
	    {error, notfound}
    end.

-spec get_sessions() -> [#session{}].
get_sessions() ->
    lists:flatmap(
      fun(LServer) ->
	      get_sessions(LServer)
      end, ejabberd_sm:get_vh_by_backend(?MODULE)).

-spec get_sessions(binary()) -> [#session{}].
get_sessions(LServer) ->
    ServKey = server_to_key(LServer),
    case mod_redis:q(?SESSION_REDIS_TABLE,["HGETALL", ServKey]) of
	{ok, Vals} ->
	    decode_session_list(Vals);
	Err ->
	    ?ERROR_MSG("failed to get sessions from redis: ~p", [Err]),
	    []
    end.

-spec get_sessions(binary(), binary()) -> [#session{}].
get_sessions(LUser, LServer) ->
    USKey = us_to_key({LUser, LServer}),
    case mod_redis:q(?SESSION_REDIS_TABLE,["HGETALL", USKey]) of
	{ok, Vals} when is_list(Vals) ->
	    decode_session_list(Vals);
	Err ->
	    ?ERROR_MSG("failed to get sessions from redis: ~p", [Err]),
	    []
    end.

-spec get_sessions(binary(), binary(), binary()) ->
    [#session{}].
get_sessions(LUser, LServer, LResource) ->
    USKey = us_to_key({LUser, LServer}),
    case mod_redis:q(?SESSION_REDIS_TABLE,["HGETALL", USKey]) of
	{ok, Vals} when is_list(Vals) ->
	    [S || S <- decode_session_list(Vals),
		  element(3, S#session.usr) == LResource];
	Err ->
	    ?ERROR_MSG("failed to get sessions from redis: ~p", [Err]),
	    []
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
iolist_to_list(IOList) ->
    binary_to_list(iolist_to_binary(IOList)).

us_to_key({LUser, LServer}) ->
    <<"ejabberd:sm:", LUser/binary, "@", LServer/binary>>.

us_to_okey({LUser, LServer}) ->
    <<"ejabberd:sm:other:", LUser/binary, "@", LServer/binary>>.

server_to_key(LServer) ->
    <<"ejabberd:sm:", LServer/binary>>.

server_to_okey(LServer) ->
    <<"ejabberd:sm:other:", LServer/binary>>.

us_sid_to_key(US, SID) ->
    term_to_binary({US, SID}).

sid_to_key(SID) ->
    term_to_binary(SID).

decode_session_list([_, Val|T]) ->
    [binary_to_term(Val)|decode_session_list(T)];
decode_session_list([]) ->
    [].

clean_table() ->
    ?INFO_MSG("Cleaning Redis SM table... ~p", [?MYHOSTS ++ ejabberd_reload_hosts:get_hosts()]),
    lists:foreach(
      fun(LServer) ->
	      ServKey = server_to_key(LServer),
              ServOKey = server_to_okey(LServer),
	      case mod_redis:q(?SESSION_REDIS_TABLE,["HKEYS", ServKey]) of
		  {ok, []} ->
		      ok;
		  {ok, Vals} ->
		      Vals1 = lists:filter(
				fun(USSIDKey) ->
					{_, SID} = binary_to_term(USSIDKey),
					node(element(2, SID)) == node()
				end, Vals),
                      Q1 = case Vals1 of
					[] -> [];
					_ -> ["HDEL", ServKey | Vals1]
				end,
                      Q2 = case Vals1 of
					[] -> [];
					_ -> ["HDEL", ServOKey | Vals1]
				end,
		      Q3 = lists:map(
			     fun(USSIDKey) ->
				     {US, SID} = binary_to_term(USSIDKey),
				     USKey = us_to_key(US),
				     SIDKey = sid_to_key(SID),
				     ["HDEL", USKey, SIDKey]
			     end, Vals1),

		      Q4 = lists:map(
			     fun(USSIDKey) ->
				     {US, SID} = binary_to_term(USSIDKey),
				     USOKey = us_to_okey(US),
				     SIDKey = sid_to_key(SID),
				     ["HDEL", USOKey, SIDKey]
			     end, Vals1),

                      Res = mod_redis:qp(?SESSION_REDIS_TABLE,lists:delete([], [Q1|Q3])),
                      case lists:filter(
                             fun({ok, _}) -> false;
                                (_) -> true
                             end, Res) of
                          [] ->
                              ok;
                          Errs ->
                              ?ERROR_MSG("failed to clean redis table for "
                                         "server ~s: ~p", [LServer, Errs])
                      end,
                      Res1 = mod_redis:qp(?SESSION_REDIS_TABLE,lists:delete([], [Q2|Q4])),
                      case lists:filter(
                             fun({ok, _}) -> false;
                                (_) -> true
                             end, Res1) of
                          [] ->
                              ok;
                          Errs1 ->
                              ?ERROR_MSG("failed to clean redis table for "
                                         "server ~s: ~p", [LServer, Errs1])
                      end;
		  Err ->
		      ?ERROR_MSG("failed to clean redis table for "
				 "server ~s: ~p", [LServer, Err])
	      end
      end, ?MYHOSTS ++ ejabberd_reload_hosts:get_hosts()).

opt_type(redis_connect_timeout) ->
    fun (I) when is_integer(I), I > 0 -> I end;
opt_type(redis_db) ->
    fun (I) when is_integer(I), I >= 0 -> I end;
opt_type(redis_password) -> fun iolist_to_list/1;
opt_type(redis_port) ->
    fun (P) when is_integer(P), P > 0, P < 65536 -> P end;
opt_type(redis_reconnect_timeout) ->
    fun (I) when is_integer(I), I > 0 -> I end;
opt_type(redis_server) -> fun iolist_to_list/1;
opt_type(_) ->
    [redis_connect_timeout, redis_db, redis_password,
     redis_port, redis_reconnect_timeout, redis_server].

get_vh_session_number(LServer) ->
    ServKey = server_to_key(LServer),
    case catch  mod_redis:q( ?SESSION_REDIS_TABLE,["HLEN", ServKey]) of
    {ok, Val} when is_binary(Val) ->
        binary_to_integer(Val);
    Err ->
        ?ERROR_MSG("failed to get sessions from redis: ~p", [Err]),
        0
    end.
