%%%----------------------------------------------------------------------
%%% File    : ejabberd_sm.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Session manager
%%% Created : 24 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2016   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_sm).

-behaviour(ejabberd_config).

-author('alexey@process-one.net').

-behaviour(gen_server).

%% API
-export([start/0,
	 start_link/0,
	 route/3,
	 process_iq/3,
	 open_session/5,
	 open_session/6,
	 close_session/4,
	 check_in_subscription/6,
	 bounce_offline_message/3,
	 disconnect_removed_user/2,
	 get_user_resources/2,
	 get_user_present_resources/2,
	 set_presence/7,
	 unset_presence/6,
	 close_session_unset_presence/5,
	 set_offline_info/5,
	 get_offline_info/4,
	 dirty_get_sessions_list/0,
	 dirty_get_my_sessions_list/0,
	 get_vh_session_list/1,
	 get_vh_session_number/1,
	 get_vh_by_backend/1,
	 register_iq_handler/4,
	 register_iq_handler/5,
	 unregister_iq_handler/2,
	 force_update_presence/1,
	 connected_users/0,
	 connected_users_number/0,
	 user_resources/2,
	 kick_user/2,
	 get_session_pid/3,
	 get_user_info/3,
	 get_user_ip/3,
	 get_max_user_sessions/2,
	 get_all_pids/0,
	 is_existing_resource/3,
	 get_commands_spec/0,
	 make_sid/0
	]).

-export([
	 insert_chat_msg/9,
	 record_show/4,
	 get_user_away_rescources/2,
	 get_user_session/2,
	 add_datetime_to_packet/3,
	 add_msectime_to_packet/4,
         delete_presence_spool/3,
	 online/1,
	 get_sm_backend/1,
     send_push_message/7,
         get_user_present_resources_and_pid/2
 	]).

-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3, opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-include("ejabberd_commands.hrl").
-include("mod_privacy.hrl").
-include("ejabberd_sm.hrl").
-include("mod_muc.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-callback init() -> ok | {error, any()}.
-callback set_session(#session{}) -> ok.
-callback delete_session(binary(), binary(), binary(), sid()) ->
    {ok, #session{}} | {error, notfound}.
-callback get_sessions() -> [#session{}].
-callback get_sessions(binary()) -> [#session{}].
-callback get_sessions(binary(), binary()) -> [#session{}].
-callback get_sessions(binary(), binary(), binary()) -> [#session{}].

-record(state, {}).

%% default value for the maximum number of user connections
-define(MAX_USER_SESSIONS, infinity).
-define(DIRECTION, <<"recv">>).
-define(CN, <<"qchat">>).
-define(USRTYPE, <<"common">>).

%%====================================================================
%% API
%%====================================================================
-export_type([sid/0]).

start() ->
    ChildSpec = {?MODULE, {?MODULE, start_link, []},
		 transient, 1000, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec route(jid(), jid(), xmlel() | broadcast()) -> ok.

route(From, To, Packet) ->
    case catch do_route(From, To, Packet) of
      {'EXIT', Reason} ->
	  ?ERROR_MSG("~p~nwhen processing: ~p",
		     [Reason, {From, To, Packet}]);
      _ -> ok
    end.

-spec open_session(sid(), binary(), binary(), binary(), prio(), info()) -> ok.

open_session(SID, User, Server, Resource, Priority, Info) ->
    catch mod_static:add_record(<<"user_login_Count">>, 1),
    set_session(SID, User, Server, Resource, Priority, Info),
    check_for_sessions_to_replace(User, Server, Resource),
    JID = jid:make(User, Server, Resource),
    ejabberd_hooks:run(sm_register_connection_hook,
		       JID#jid.lserver, [SID, JID, Info]).

-spec open_session(sid(), binary(), binary(), binary(), info()) -> ok.

open_session(SID, User, Server, Resource, Info) ->
    open_session(SID, User, Server, Resource, undefined, Info).

-spec close_session(sid(), binary(), binary(), binary()) -> ok.

close_session(SID, User, Server, Resource) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LResource = jid:resourceprep(Resource),
    Mod = get_sm_backend(LServer),
    Info = case Mod:delete_session(LUser, LServer, LResource, SID) of
	       {ok, #session{info = I}} -> I;
	       {error, notfound} -> []
	   end,
    JID = jid:make(User, Server, Resource),
    ejabberd_rpc_presence:send_login_presence(JID),
    ejabberd_hooks:run(sm_remove_connection_hook,
		       JID#jid.lserver, [SID, JID, Info]).

-spec check_in_subscription(any(), binary(), binary(),
                            any(), any(), any()) -> any().

check_in_subscription(Acc, User, Server, _JID, _Type, _Reason) ->
    case ejabberd_auth:is_user_exists(User, Server) of
      true -> Acc;
      false -> {stop, false}
    end.

-spec bounce_offline_message(jid(), jid(), xmlel()) -> stop.

bounce_offline_message(From, To, Packet) ->
    Lang = fxml:get_tag_attr_s(<<"xml:lang">>, Packet),
    Txt = <<"User session not found">>,
    Err = jlib:make_error_reply(
	    Packet, ?ERRT_SERVICE_UNAVAILABLE(Lang, Txt)),
    ejabberd_router:route(To, From, Err),
    stop.

-spec disconnect_removed_user(binary(), binary()) -> ok.

disconnect_removed_user(User, Server) ->
    ejabberd_sm:route(jid:make(<<"">>, <<"">>, <<"">>),
		      jid:make(User, Server, <<"">>),
                      {broadcast, {exit, <<"User removed">>}}).

get_user_resources(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = get_sm_backend(LServer),
    Ss = online(Mod:get_sessions(LUser, LServer)),
    [element(3, S#session.usr) || S <- clean_session_list(Ss)].

-spec get_user_present_resources(binary(), binary()) -> [tuple()].

get_user_present_resources(LUser, LServer) ->
    Mod = get_sm_backend(LServer),
    Ss = online(Mod:get_sessions(LUser, LServer)),

    [{S#session.priority, element(3, S#session.usr)}
     || S <- clean_session_list(Ss), is_integer(S#session.priority)].

-spec get_user_ip(binary(), binary(), binary()) -> ip().

get_user_ip(User, Server, Resource) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LResource = jid:resourceprep(Resource),
    Mod = get_sm_backend(LServer),
    case online(Mod:get_sessions(LUser, LServer, LResource)) of
	[] ->
	    undefined;
	Ss ->
	    Session = lists:max(Ss),
	    proplists:get_value(ip, Session#session.info)
    end.

-spec get_user_info(binary(), binary(), binary()) -> info() | offline.

get_user_info(User, Server, Resource) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LResource = jid:resourceprep(Resource),
    Mod = get_sm_backend(LServer),
    case online(Mod:get_sessions(LUser, LServer, LResource)) of
	[] ->
	    offline;
	Ss ->
	    Session = lists:max(Ss),
	    Node = node(element(2, Session#session.sid)),
	    Conn = proplists:get_value(conn, Session#session.info),
	    IP = proplists:get_value(ip, Session#session.info),
	    [{node, Node}, {conn, Conn}, {ip, IP}]
    end.

-spec set_presence(sid(), binary(), binary(), binary(),
                   prio(), xmlel(), info()) -> ok.

set_presence(SID, User, Server, Resource, Priority,
	     Presence, Info) ->
    set_session(SID, User, Server, Resource, Priority,
		Info),
    catch ejabberd_rpc_presence:send_login_presence(jid:make(User, Server,<<"">>)),
    ejabberd_hooks:run(set_presence_hook,
		       jid:nameprep(Server),
		       [User, Server, Resource, Presence]).

-spec unset_presence(sid(), binary(), binary(),
                     binary(), binary(), info()) -> ok.

unset_presence(SID, User, Server, Resource, Status,
	       Info) ->
    set_session(SID, User, Server, Resource, undefined,
		Info),
    ejabberd_hooks:run(unset_presence_hook,
		       jid:nameprep(Server),
		       [User, Server, Resource, Status]).

-spec close_session_unset_presence(sid(), binary(), binary(),
                                   binary(), binary()) -> ok.

close_session_unset_presence(SID, User, Server,
			     Resource, Status) ->
    close_session(SID, User, Server, Resource),
    ejabberd_hooks:run(unset_presence_hook,
		       jid:nameprep(Server),
		       [User, Server, Resource, Status]).

-spec get_session_pid(binary(), binary(), binary()) -> none | pid().

get_session_pid(User, Server, Resource) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LResource = jid:resourceprep(Resource),
    Mod = get_sm_backend(LServer),
    case online(Mod:get_sessions(LUser, LServer, LResource)) of
	[#session{sid = {_, Pid}}] -> Pid;
	_ -> none
    end.

-spec set_offline_info(sid(), binary(), binary(), binary(), info()) -> ok.

set_offline_info(SID, User, Server, Resource, Info) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LResource = jid:resourceprep(Resource),
    set_session(SID, LUser, LServer, LResource, undefined, [offline | Info]).

-spec get_offline_info(erlang:timestamp(), binary(), binary(),
                       binary()) -> none | info().

get_offline_info(Time, User, Server, Resource) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LResource = jid:resourceprep(Resource),
    Mod = get_sm_backend(LServer),
    case Mod:get_sessions(LUser, LServer, LResource) of
	[#session{sid = {Time, _}, info = Info}] ->
	    case proplists:get_bool(offline, Info) of
		true ->
		    Info;
		false ->
		    none
	    end;
	_ ->
	    none
    end.

-spec dirty_get_sessions_list() -> [ljid()].

dirty_get_sessions_list() ->
    lists:flatmap(
      fun(Mod) ->
	      [S#session.usr || S <- online(Mod:get_sessions())]
      end, get_sm_backends()).

-spec dirty_get_my_sessions_list() -> [#session{}].

dirty_get_my_sessions_list() ->
    lists:flatmap(
      fun(Mod) ->
	      [S || S <- online(Mod:get_sessions()),
		    node(element(2, S#session.sid)) == node()]
      end, get_sm_backends()).

-spec get_vh_session_list(binary()) -> [ljid()].

get_vh_session_list(Server) ->
    LServer = jid:nameprep(Server),
    Mod = get_sm_backend(LServer),
    [S#session.usr || S <- online(Mod:get_sessions(LServer))].

-spec get_all_pids() -> [pid()].

get_all_pids() ->
    lists:flatmap(
      fun(Mod) ->
	      [element(2, S#session.sid) || S <- online(Mod:get_sessions())]
      end, get_sm_backends()).

-spec get_vh_session_number(binary()) -> non_neg_integer().

%%get_vh_session_number(Server) ->
%%    LServer = jid:nameprep(Server),
%%    Mod = get_sm_backend(LServer),
%%    length(online(Mod:get_sessions(LServer))).
get_vh_session_number(Server) ->
    LServer = jid:nameprep(Server),
    Mod = get_sm_backend(LServer),
    Mod:get_vh_session_number(LServer).

register_iq_handler(Host, XMLNS, Module, Fun) ->
    ejabberd_sm ! {register_iq_handler, Host, XMLNS, Module, Fun}.

-spec register_iq_handler(binary(), binary(), atom(), atom(), list()) -> any().

register_iq_handler(Host, XMLNS, Module, Fun, Opts) ->
    ejabberd_sm ! {register_iq_handler, Host, XMLNS, Module, Fun, Opts}.

-spec unregister_iq_handler(binary(), binary()) -> any().

unregister_iq_handler(Host, XMLNS) ->
    ejabberd_sm ! {unregister_iq_handler, Host, XMLNS}.


%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    lists:foreach(fun(Mod) -> Mod:init() end, get_sm_backends()),
    ets:new(sm_iqtable, [named_table]),
    lists:foreach(
      fun(Host) ->
	      ejabberd_hooks:add(roster_in_subscription, Host,
				 ejabberd_sm, check_in_subscription, 20),
	      ejabberd_hooks:add(offline_message_hook, Host,
				 ejabberd_sm, bounce_offline_message, 100),
	      ejabberd_hooks:add(remove_user, Host,
				 ejabberd_sm, disconnect_removed_user, 100)
      end, ?MYHOSTS),
    ejabberd_commands:register_commands(get_commands_spec()),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok, {reply, Reply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({route, From, To, Packet}, State) ->
    case catch do_route(From, To, Packet) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p~nwhen processing: ~p",
		       [Reason, {From, To, Packet}]);
	_ ->
	    ok
    end,
    {noreply, State};
handle_info({register_iq_handler, _Host, XMLNS, Module, Function}, State) ->
    ets:insert(sm_iqtable, {XMLNS, Module, Function}),
    {noreply, State};
handle_info({register_iq_handler, _Host, XMLNS, Module,
	     Function, Opts},
	    State) ->
    ets:insert(sm_iqtable,
	       {XMLNS, Module, Function, Opts}),
    {noreply, State};
handle_info({unregister_iq_handler, _Host, XMLNS},
	    State) ->
    case ets:lookup(sm_iqtable, XMLNS) of
      [{_, Module, Function, Opts}] ->
	  gen_iq_handler:stop_iq_handler(Module, Function, Opts);
      _ -> ok
    end,
    ets:delete(sm_iqtable, XMLNS),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) ->
    catch ets:delete_all_objects(node_pid_prefix),
    ejabberd_commands:unregister_commands(get_commands_spec()),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

-spec set_session(sid(), binary(), binary(), binary(),
                  prio(), info()) -> ok.

set_session(SID, User, Server, Resource, Priority, Info) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LResource = jid:resourceprep(Resource),
    US = {LUser, LServer},
    USR = {LUser, LServer, LResource},
    Mod = get_sm_backend(LServer),
    Mod:set_session(#session{sid = SID, usr = USR, us = US,
			     priority = Priority, info = Info, show = <<"normal">>}).

-spec online([#session{}]) -> [#session{}].

online(Sessions) ->
    lists:filter(fun is_online/1, Sessions).

-spec is_online(#session{}) -> boolean().

is_online(#session{info = Info}) ->
    not proplists:get_bool(offline, Info).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_route(From, To, {broadcast, _} = Packet) ->
    case To#jid.lresource of
        <<"">> ->
            lists:foreach(fun(R) ->
                                  do_route(From,
                                           jid:replace_resource(To, R),
                                           Packet)
                          end,
                          get_user_resources(To#jid.user, To#jid.server));
        _ ->
            {U, S, R} = jid:tolower(To),
	    Mod = get_sm_backend(S),
	    case online(Mod:get_sessions(U, S, R)) of
                [] ->
                    ?DEBUG("packet dropped~n", []);
                Ss ->
                    Session = lists:max(Ss),
                    Pid = element(2, Session#session.sid),
                    Pid ! {route, From, To, Packet}
            end
    end;
do_route(From, To, #xmlel{} = Packet10) ->
    ?DEBUG("session manager~n\tfrom ~p~n\tto ~p~n\tpacket "
	   "~P~n",
	   [From, To, Packet10, 8]),
    #jid{user = User, server = Server,
	 luser = LUser, lserver = LServer, lresource = LResource} = To,
    Packet = case fxml:get_tag_attr_s(<<"chatid">>, Packet10) of
            undefined ->
                fxml:replace_tag_attr(<<"chatid">>, <<"1">>, Packet10);
            _ ->
                Packet10
            end,

    #xmlel{name = Name, attrs = Attrs, children = Els} = Packet,
    Lang = fxml:get_attr_s(<<"xml:lang">>, Attrs),
    case LResource of
      <<"">> ->
	  case Name of
	    <<"presence">> ->
		{Pass, _Subsc} = case fxml:get_attr_s(<<"type">>, Attrs)
				     of
				   <<"subscribe">> ->
				       Reason = fxml:get_path_s(Packet,
							       [{elem,
								 <<"status">>},
								cdata]),
				       {is_privacy_allow(From, To, Packet)
					  andalso
					  ejabberd_hooks:run_fold(roster_in_subscription,
								  LServer,
								  false,
								  [User, Server,
								   From,
								   subscribe,
								   Reason]),
					true};
				   <<"subscribed">> ->
				       {is_privacy_allow(From, To, Packet)
					  andalso
					  ejabberd_hooks:run_fold(roster_in_subscription,
								  LServer,
								  false,
								  [User, Server,
								   From,
								   subscribed,
								   <<"">>]),
					true};
				   <<"unsubscribe">> ->
				       {is_privacy_allow(From, To, Packet)
					  andalso
					  ejabberd_hooks:run_fold(roster_in_subscription,
								  LServer,
								  false,
								  [User, Server,
								   From,
								   unsubscribe,
								   <<"">>]),
					true};
				   <<"unsubscribed">> ->
				       {is_privacy_allow(From, To, Packet)
					  andalso
					  ejabberd_hooks:run_fold(roster_in_subscription,
								  LServer,
								  false,
								  [User, Server,
								   From,
								   unsubscribed,
								   <<"">>]),
					true};
				   _ -> {true, false}
				 end,
		if Pass ->
			handle_presence(LServer,From,To,Packet,Attrs);
		   true -> ok
		end;
	    <<"message">> ->
		case fxml:get_attr_s(<<"type">>, Attrs) of
		  <<"chat">> -> route_message(From, To, Packet, chat);
		  <<"headline">> -> route_message(From, To, Packet, headline);
		  <<"error">> -> ok;
		  <<"groupchat">> -> route_message(From, To, Packet, groupchat);
          Type ->
            route_message(From, To, Packet, Type)
		end;
	    <<"iq">> -> process_iq(From, To, Packet);
	    _ -> ok
	  end;
      _ ->
	Mod = get_sm_backend(LServer),
	case online(Mod:get_sessions(LUser, LServer, LResource)) of
	    [] ->
		case Name of
		  <<"message">> ->
		      case fxml:get_attr_s(<<"type">>, Attrs) of
			<<"chat">> -> route_message(From, To, Packet, chat);
			<<"headline">> -> ok;
			<<"error">> -> ok;
			<<"groupchat">> -> ok;
			<<"normal">> -> route_message(From, To, Packet, normal);
		        Type -> route_message(From, To, Packet, Type)
		      end;
		  <<"iq">> ->
		      case fxml:get_attr_s(<<"type">>, Attrs) of
			<<"error">> -> ok;
			<<"result">> -> update_iq_muc_info(From,To,Packet), ok;
			_ ->
			    ErrTxt = <<"User session not found">>,
			    Err = jlib:make_error_reply(
				    Packet,
				    ?ERRT_SERVICE_UNAVAILABLE(Lang, ErrTxt)),
			    ejabberd_router:route(To, From, Err)
		      end;
		  _ -> ?DEBUG("packet dropped~n", [])
		end;
	    Ss ->
                Session = lists:max(Ss),
                Pid = element(2, Session#session.sid),
                NewPacket  = case Name of
                    <<"message">> -> make_new_packet(From,To,Packet,Name,Attrs,Els);	
                    <<"iq">> ->
                        update_iq_muc_info(From,To,Packet),
                        Packet;
                    _ -> Packet
                end,
		Pid ! {route, From, To, NewPacket},
        ?DEBUG("Pid send  newPacket ~p ~n",[Pid])
	  end
    end.

%% The default list applies to the user as a whole,
%% and is processed if there is no active list set
%% for the target session/resource to which a stanza is addressed,
%% or if there are no current sessions for the user.
is_privacy_allow(From, To, Packet) ->
    User = To#jid.user,
    Server = To#jid.server,
    PrivacyList =
	ejabberd_hooks:run_fold(privacy_get_user_list, Server,
				#userlist{}, [User, Server]),
    is_privacy_allow(From, To, Packet, PrivacyList).

%% Check if privacy rules allow this delivery
%% Function copied from ejabberd_c2s.erl
is_privacy_allow(From, To, Packet, PrivacyList) ->
    User = To#jid.user,
    Server = To#jid.server,
    allow ==
      ejabberd_hooks:run_fold(privacy_check_packet, Server,
			      allow,
			      [User, Server, PrivacyList, {From, To, Packet},
			       in]).

route_message(From, To, Packet, Type) ->
    LUser = To#jid.luser,
    LServer = To#jid.lserver,
    PrioRes = get_user_present_resources(LUser, LServer),
    #xmlel{name = Name, attrs = Attrs, children = Els} = Packet,
    NewPacket = make_new_packet(From,To,Packet,Name,Attrs,Els),

    case Type of
         <<"readmark">> -> readmark:readmark_message(From,To,NewPacket);
         <<"revoke">> -> revoke:revoke_message(From,To,NewPacket);
         _ -> ok
    end,

    case catch lists:max(PrioRes) of
      {Priority, _R}
	  when is_integer(Priority), Priority >= 0 ->
              catch send_max_priority_msg(LUser,LServer,Priority,From,To,NewPacket,PrioRes);
      _ -> ok
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clean_session_list(Ss) ->
    clean_session_list(lists:keysort(#session.usr, Ss), []).

clean_session_list([], Res) -> Res;
clean_session_list([S], Res) -> [S | Res];
clean_session_list([S1, S2 | Rest], Res) ->
    if S1#session.usr == S2#session.usr ->
	   if S1#session.sid > S2#session.sid ->
		  clean_session_list([S1 | Rest], Res);
	      true -> clean_session_list([S2 | Rest], Res)
	   end;
       true -> clean_session_list([S2 | Rest], [S1 | Res])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% On new session, check if some existing connections need to be replace
check_for_sessions_to_replace(User, Server, Resource) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LResource = jid:resourceprep(Resource),
    check_existing_resources(LUser, LServer, LResource),
    check_max_sessions(LUser, LServer).

check_existing_resources(LUser, LServer, LResource) ->
    Mod = get_sm_backend(LServer),
    Ss = Mod:get_sessions(LUser, LServer, LResource),
    {OnlineSs, OfflineSs} = lists:partition(fun is_online/1, Ss),
    lists:foreach(fun(#session{sid = S}) ->
			  Mod:delete_session(LUser, LServer, LResource, S)
		  end, OfflineSs),
    if OnlineSs == [] -> ok;
       true ->
	   SIDs = [SID || #session{sid = SID} <- OnlineSs],
	   MaxSID = lists:max(SIDs),
	   lists:foreach(fun ({_, Pid} = S) when S /= MaxSID ->
				 Pid ! replaced;
			     (_) -> ok
			 end,
			 SIDs)
    end.

-spec is_existing_resource(binary(), binary(), binary()) -> boolean().

is_existing_resource(LUser, LServer, LResource) ->
    [] /= get_resource_sessions(LUser, LServer, LResource).

get_resource_sessions(User, Server, Resource) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LResource = jid:resourceprep(Resource),
    Mod = get_sm_backend(LServer),
    [S#session.sid || S <- online(Mod:get_sessions(LUser, LServer, LResource))].

check_max_sessions(LUser, LServer) ->
    Mod = get_sm_backend(LServer),
    SIDs = [S#session.sid || S <- online(Mod:get_sessions(LUser, LServer))],
    MaxSessions = get_max_user_sessions(LUser, LServer),
    if length(SIDs) =< MaxSessions -> ok;
       true -> {_, Pid} = lists:min(SIDs), Pid ! replaced
    end.

%% Get the user_max_session setting
%% This option defines the max number of time a given users are allowed to
%% log in
%% Defaults to infinity
get_max_user_sessions(LUser, Host) ->
    case acl:match_rule(Host, max_user_sessions,
			jid:make(LUser, Host, <<"">>))
	of
      Max when is_integer(Max) -> Max;
      infinity -> infinity;
      _ -> ?MAX_USER_SESSIONS
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_iq(From, To, Packet) ->
    IQ = jlib:iq_query_info(Packet),
    case IQ of
      #iq{xmlns = XMLNS, lang = Lang} ->
	  Host = To#jid.lserver,
	  case ets:lookup(sm_iqtable, XMLNS) of
	    [{_, Module, Function}] ->
		ResIQ = Module:Function(From, To, IQ),
		if ResIQ /= ignore ->
		       ejabberd_router:route(To, From, jlib:iq_to_xml(ResIQ));
		   true -> ok
		end;
	    [{_, Module, Function, Opts}] ->
		gen_iq_handler:handle(Host, Module, Function, Opts,
				      From, To, IQ);
	    [] ->
		Txt = <<"No module is handling this query">>,
		Err = jlib:make_error_reply(
			Packet,
			?ERRT_SERVICE_UNAVAILABLE(Lang, Txt)),
		ejabberd_router:route(To, From, Err)
	  end;
      reply -> update_iq_muc_info(From,To,Packet),ok;
      _ ->
	  Err = jlib:make_error_reply(Packet, ?ERR_BAD_REQUEST),
	  ejabberd_router:route(To, From, Err),
	  ok
    end.

-spec force_update_presence({binary(), binary()}) -> any().

force_update_presence({LUser, LServer}) ->
    Mod = get_sm_backend(LServer),
    Ss = online(Mod:get_sessions(LUser, LServer)),
    lists:foreach(fun (#session{sid = {_, Pid}}) ->
			  Pid ! {force_update_presence, LUser, LServer}
		  end,
		  Ss).

-spec get_sm_backend(binary()) -> module().

get_sm_backend(Host) ->
    DBType = ejabberd_config:get_option(
	       {sm_db_type, Host},
	       fun(T) -> ejabberd_config:v_db(?MODULE, T) end,
	       mnesia),
    list_to_atom("ejabberd_sm_" ++ atom_to_list(DBType)).

-spec get_sm_backends() -> [module()].

get_sm_backends() ->
    lists:usort([get_sm_backend(Host) || Host <- ?MYHOSTS]).

-spec get_vh_by_backend(module()) -> [binary()].

get_vh_by_backend(Mod) ->
    lists:filter(
      fun(Host) ->
	      get_sm_backend(Host) == Mod
      end, ?MYHOSTS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ejabberd commands

get_commands_spec() ->
    [#ejabberd_commands{name = connected_users,
			tags = [session],
			desc = "List all established sessions",
                        policy = admin,
			module = ?MODULE, function = connected_users, args = [],
			result = {connected_users, {list, {sessions, string}}}},
     #ejabberd_commands{name = connected_users_number,
			tags = [session, stats],
			desc = "Get the number of established sessions",
                        policy = admin,
			module = ?MODULE, function = connected_users_number,
			args = [], result = {num_sessions, integer}},
     #ejabberd_commands{name = user_resources,
			tags = [session],
			desc = "List user's connected resources",
                        policy = user,
			module = ?MODULE, function = user_resources,
			args = [],
			result = {resources, {list, {resource, string}}}},
     #ejabberd_commands{name = kick_user,
			tags = [session],
			desc = "Disconnect user's active sessions",
			module = ?MODULE, function = kick_user,
			args = [{user, binary}, {host, binary}],
			result = {num_resources, integer}}].

-spec connected_users() -> [binary()].

connected_users() ->
    USRs = dirty_get_sessions_list(),
    SUSRs = lists:sort(USRs),
    lists:map(fun ({U, S, R}) -> <<U/binary, $@, S/binary, $/, R/binary>> end,
	      SUSRs).

connected_users_number() ->
    length(dirty_get_sessions_list()).

user_resources(User, Server) ->
    Resources = get_user_resources(User, Server),
    lists:sort(Resources).

kick_user(User, Server) ->
    Resources = get_user_resources(User, Server),
    lists:foreach(
	fun(Resource) ->
		PID = get_session_pid(User, Server, Resource),
                ?ERROR_MSG("kick user ~p~n", [{User, Server, Resource}]),
		PID ! kick
	end, Resources),
    length(Resources).

make_sid() ->
    {p1_time_compat:unique_timestamp(), self()}.

opt_type(sm_db_type) -> fun(T) -> ejabberd_config:v_db(?MODULE, T) end;
opt_type(_) -> [sm_db_type].

get_user_present_resources_and_pid(LUser, LServer) ->
    Mod = get_sm_backend(LServer),
    Ss = online(Mod:get_sessions(LUser, LServer)),
    [{S#session.priority, element(3, S#session.usr),element(2, S#session.sid)}
        || S <- clean_session_list(Ss), is_integer(S#session.priority)].

get_user_session(LUser, LServer) ->
    Mod = get_sm_backend(LServer),
    Ss = online(Mod:get_sessions(LUser, LServer)),
    [S  || S <- clean_session_list(Ss), is_integer(S#session.priority)].

insert_chat_msg(_Server,From, To,FromHost,ToHost, Msg,_Body,ID,InsertTime) ->
    case jlib:nodeprep(From) of
    error -> {error, invalid_jid};
    LUser ->
        Packet = #xmlel{attrs = Attrs} = fxml_stream:parse_element(Msg),
        Type = fxml:get_attr_s(<<"type">>, Attrs),
        Realfrom = fxml:get_attr_s(<<"realfrom">>, Attrs),
        Realto = fxml:get_attr_s(<<"realto">>, Attrs),
        LFrom = ejabberd_sql:escape(LUser),
        LTo = ejabberd_sql:escape(To),
        LBody = ejabberd_sql:escape(Msg),
        LID = ejabberd_sql:escape(ID),
        LServer = get_server(FromHost,ToHost),
        Time = qtalk_public:pg2timestamp(InsertTime),

	catch spawn(?MODULE, send_push_message, [From, To, FromHost, ToHost, Msg, ID, InsertTime]),
        insert_msg2db(LServer, LFrom,LTo,FromHost,ToHost,LID,LBody,Time, Realfrom,Realto,Type)
    end.

insert_msg2db(LServer, LFrom,LTo,From_host,To_host,LID,LBody,Time) ->
    case catch qtalk_sql:insert_msg_v2(LServer, LFrom,LTo,From_host,To_host,LBody,LID,Time) of
        {updated, 1} -> {atomic, ok};
        A -> ?INFO_MSG("Insert Msg error Body: ~p ,~p ~n",[A,LBody]), {atomic, exists}
    end.


insert_msg2db(LServer, LFrom,LTo,From_host,To_host,LID,LBody,Time, Realfrom, Realto, Type) ->
    case catch qtalk_sql:insert_msg_v3(LServer, LFrom,LTo,From_host,To_host,LBody,LID,Time, Realfrom, Realto, Type) of
        {updated, 1} -> {atomic, ok};
        A -> ?INFO_MSG("Insert Msg error Body: ~p ,~p ~n",[A,LBody]), {atomic, exists}
    end.

record_show(User,Server,Resource,Show) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    LResource = jlib:resourceprep(Resource),
    US = {LUser, LServer},
    USR = {LUser, LServer, LResource},
    
    Mod = get_sm_backend(LServer),
    case online(Mod:get_sessions(LUser, LServer, LResource)) of
    [] ->
    	ok;
    Ss ->
        Session = lists:max(Ss),
        catch Mod:set_session(Session#session{usr = USR,us = US, show = Show})
    end.

%%判读特殊情况，聊天室用户非正常退出，导致存在于聊天室中的#state.users表中，
%%多域情况下给to用户会重复发送消息
judge_to_user_available(_FServer,<<"">>,_R)->
    true;
judge_to_user_available(FServer,Rescource,R) ->
    case str:str(FServer,<<"conference">>) of 
        0 -> true;
	_ ->
            if Rescource == R -> true;
            true -> false
            end
    end.

%%TODO
send_muc_room_remove_unavailable_user(From,To) ->
    case mod_muc_redis:get_muc_room_pid(From#jid.user, From#jid.server) of
        [] -> ok;
        [Muc] -> Muc#muc_online_room.pid ! {delete_unavailable_user,To}
    end.

get_user_away_rescources(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    Mod = get_sm_backend(LServer),
    case online(Mod:get_sessions(LUser, LServer)) of
        [] -> [<<"none">>];
        Ss ->
            lists:flatmap(fun(S) ->
                case S#session.show of 
                    <<"away">> -> [element(3, S#session.usr)];
                    _ -> []
                end
            end,clean_session_list(Ss))
    end.

try_insert_msg(From,To,Packet,Mbody,Now) ->
    #xmlel{attrs = Attrs, children = Els} = Packet,
    LServer = To#jid.lserver,
    insert_user_chat_msg(From,To,Packet,Attrs,Els,Mbody,LServer,Now).

insert_user_chat_msg(From,To,Packet,Attrs,Els,Mbody,LServer,Now) ->
    catch mod_static:add_record(<<"chat_message">>,1),
    Carbon = fxml:get_attr_s(<<"carbon_message">>, Attrs),

    case Carbon =/= <<"true">> andalso (not mod_muc_room:is_invitation(Els)) of
        true  ->
            Msg_id = fxml:get_tag_attr_s(<<"id">>, fxml:get_subtag(Packet,<<"body">>)),
            %%catch qtalk_public:send_recv_repley(Msg_id,From,Packet),
            case catch fxml:element_to_binary(Packet) of
            BPacket when is_binary(BPacket)  ->
            catch     insert_chat_msg(LServer,
		                        From#jid.user,
                			    To#jid.user,
			                    From#jid.lserver,
                			    To#jid.lserver,
			                    BPacket,
                			    Mbody,
                			    Msg_id,
			                    Now);
            Err ->
                ?INFO_MSG("insert_user_chat_msg ~p ,~p ~n",[Err,Packet])
            end;
        _ -> ok
    end.

send_max_priority_msg(LUser,LServer,Priority,From,To,NewPacket,PrioRes) ->
    lists:flatmap(fun ({P, R}) when P == Priority ->
            LResource = jlib:resourceprep(R),
            Mod = get_sm_backend(LServer),
            case online(Mod:get_sessions(LUser, LServer, LResource)) of
                [] -> []; % Race condition
                Ss ->
                    case judge_to_user_available(From#jid.server,To#jid.lresource,LResource) of
                        true ->
          	                Session = lists:max(Ss),
          	                Pid = element(2, Session#session.sid),
          	                ?DEBUG("sending to process ~p~n", [Pid]),
          	                Pid ! {route, From, To, NewPacket},
                            [{Session#session.show,LResource}];
                _ ->
                    ?INFO_MSG("Rescoure not match ~p : ~p ~n",[R,To#jid.lresource]), 
                    send_muc_room_remove_unavailable_user(From,To),
                    []
            end
        end;
        %% Ignore other priority:
        ({_Prio, _Res}) -> []
    end,PrioRes).

make_new_packet(From,To,Packet,Name,Attrs,Els) ->
    NewPacket = case fxml:get_attr_s(<<"msec_times">>, Attrs)  of
        <<"">> -> add_datetime_to_packet(Name,Attrs,Els);
        _ -> Packet
    end,

    InsertTime = binary_to_integer(fxml:get_tag_attr_s(<<"msec_times">>, NewPacket)),
    Mtype = fxml:get_attr_s(<<"type">>, Attrs),
    case  Mtype == <<"normal">> orelse Mtype == <<"chat">> orelse Mtype == <<"consult">> orelse Mtype == <<"collection">> of
        true ->
            Reply = fxml:get_attr_s(<<"auto_reply">>, Attrs),
            Mbody = fxml:get_subtag_cdata(NewPacket, <<"body">>),
            Delay = fxml:get_subtag_cdata(Packet,<<"delay">>),

            case Delay =/= <<"Offline Storage">> andalso
                 Reply =/= <<"true">> of
                true ->
                    try_insert_msg(From,To,NewPacket,Mbody,InsertTime),
                    NewPacket;
                _ -> NewPacket
            end;
        _ -> NewPacket
    end.

add_datetime_to_packet(Name,Attrs,Els) ->
    Now = qtalk_public:get_exact_timestamp(),
    add_msectime_to_packet(Name,Attrs,Els,Now).

add_msectime_to_packet(Name,Attrs,Els,Time) ->
    #xmlel{name = Name,
           attrs =  [{<<"msec_times">>,integer_to_binary(Time)}] ++ Attrs,
           children =  Els}.

make_verify_friend_packet(Num, Rslt, _From, _To, Packet, Attrs) ->
    Num1  = case proplists:get_value(<<"num">>,Rslt) of
        undefined -> 0;
        V  -> V
    end,
    case Num < 300 andalso Num1 < 300 of
        true ->
            case proplists:get_value(<<"mode">>,Rslt) of
                <<"0">> -> do_make_verify_friend_packet(?NS_VER_FRI,<<"refused">>,<<"all_refuse">>,2);
                <<"1">> -> {Packet,1};
                <<"2">> ->
                    case fxml:get_attr_s(<<"answer">>, Attrs) == proplists:get_value(<<"answer">>,Rslt) of
		        true -> do_make_verify_friend_packet(?NS_VER_FRI,<<"success">>,<<"answer_right">>,2) ;
                        _ -> do_make_verify_friend_packet(?NS_VER_FRI,<<"refused">>,<<"answer_errror">>,2)
                    end;
                <<"3">> -> do_make_verify_friend_packet(?NS_VER_FRI,<<"success">>,<<"all_accpet">>,2);
                _ -> do_make_verify_friend_packet(?NS_VER_FRI,<<"success">>,<<"default_config">>,2)
            end;
        false -> do_make_verify_friend_packet(?NS_VER_FRI,<<"refused">>,<<"out of max friend num">>,2)
    end.

do_make_verify_friend_packet(XMLNS,Rslt,Reason,Two_ways) ->
    {#xmlel{name = <<"presence">>,
            attrs = [{<<"xmlns">>,XMLNS},{<<"type">>,<<"handle_friend_result">>},{<<"result">>,Rslt},{<<"reason">>,Reason}],
            children = []},Two_ways}.

send_presence_packet(From,To,Packet) ->
    PResources = get_user_present_resources(To#jid.luser, To#jid.lserver),
    lists:foreach(fun ({_, R}) ->
        do_route(From, jlib:jid_replace_resource(To, R), Packet)
    end, PResources).

send_presence_packet1(From,To,Packet,Attrs) ->
    PResources = get_user_present_resources(To#jid.luser, To#jid.lserver),
    case PResources of 
        [] ->
            Body = fxml:get_attr_s(<<"body">>, Attrs),
            catch insert_presence_spool(From#jid.server,From#jid.luser,To#jid.luser,To#jid.lserver,Body);	
        _ ->
            lists:foreach(fun ({_, R}) ->
                do_route(From, jlib:jid_replace_resource(To, R), Packet)
            end, PResources)
    end.

insert_presence_spool(Server,From,To,IServer,Body) ->
    Timestamp  = integer_to_binary(qtalk_public:get_timestamp()),
    case catch ejabberd_sql:sql_query(Server, [<<"udpate invite_spool set timestamp = ">>,Timestamp,<<",Body = '">>,Body,<<"' where username = '">>,To,
			<<"' and inviter = '">>, From,<<"' and host = '">>,Server,<<"' and ihost = '">>,IServer,<<"';">>]) of
        {updated,1} -> ok;
    _ ->
       case catch ejabberd_sql:sql_query(Server, [<<"insert into invite_spool(username,host,inviter,ihost,body,timestamp) values ('">>, To,<<"','">>,
			IServer,<<"','">>,From,<<"','">>,Server,<<"','">>,ejabberd_sql:escape(Body),<<"',">>,Timestamp,<<")">>]) of
           {updated,1} -> ok;
           _ -> ok
       end
    end.

delete_presence_spool(Server,From,To) ->
    case catch ejabberd_sql:sql_query(Server, [<<"delete from invite_spool where username = '">>,From,<<"' and inviter = '">>,To,<<"';">>]) of
        {updated,1} -> ok;
        _ -> ok
    end.
	
handle_presence(LServer,From,To,Packet,Attrs) ->
    User = To#jid.luser,
    Type = fxml:get_attr_s(<<"type">>, Attrs),
    Result = fxml:get_attr_s(<<"result">>, Attrs),
              
    case {Type,Result} of
     {<<"verify_friend">>,_} ->
            Rslt = mod_user_relation:do_verify_friend(LServer,User, To#jid.lserver),
            Num = case fxml:get_attr_s(<<"friend_num">>,Attrs) of
                <<>> -> 0;
		N when is_binary(N) -> binary_to_integer(N);
		_ -> 0
            end,
            {NewPacket,Two_way} = make_verify_friend_packet(Num,Rslt,From,To,Packet,Attrs),
            case Two_way of
                1 -> send_presence_packet1(From,To,NewPacket,Attrs);
                _ ->
                    ejabberd_router:route(From,jlib:jid_replace_resource(To,<<"">>),NewPacket),
                    ejabberd_router:route(To,jlib:jid_replace_resource(From,<<"">>),fxml:replace_tag_attr(<<"direction">>, <<"2">>, NewPacket))
            end;
       {<<"manual_authentication_confirm">>,<<"allow">>}  ->
            Num1 =  mod_user_relation:get_users_friend_num(LServer,User, To#jid.lserver),
            Num = binary_to_integer(fxml:get_attr_s(<<"friend_num">>,Attrs)),
            case Num1 < 300 andalso Num < 300 of
            true ->
                {NewPacket,_} = do_make_verify_friend_packet(?NS_VER_FRI,<<"success">>,<<"manual_authentication_confirm_success">>,1),
                 ejabberd_router:route(From,jlib:jid_replace_resource(To,<<"">>),NewPacket),
                 ejabberd_router:route(To,jlib:jid_replace_resource(From,<<"">>),fxml:replace_tag_attr(<<"direction">>, <<"2">>, NewPacket));
            _ ->
                {NewPacket,_} = do_make_verify_friend_packet(?NS_VER_FRI,<<"refused">>,<<"out of max friend num">>,1),
                ejabberd_router:route(From,jlib:jid_replace_resource(To,<<"">>),NewPacket),
                ejabberd_router:route(To,jlib:jid_replace_resource(From,<<"">>),fxml:replace_tag_attr(<<"direction">>, <<"2">>, NewPacket))
            end;
        {<<"handle_friend_result">>,<<"success">>} ->
	    user_relation:add_user_friend(LServer,To,From,<<"1">>),
            send_presence_packet(From,To,Packet);
        {<<"handle_friend_result">>,_} ->
            send_presence_packet(From,To,Packet);
        {<<"two_way_del">>,<<"success">> } ->
            Del_user = fxml:get_attr_s(<<"jid">>, Attrs),
            Domain = fxml:get_attr_s(<<"domain">>, Attrs),
	     catch  user_relation:del_friend(To#jid.lserver,To#jid.luser,To#jid.lserver,Del_user,Domain,<<"1">>),
            send_presence_packet(From,To,Packet);
        _ -> send_presence_packet(From,To,Packet) 
    end.

update_iq_muc_info(From,To, #xmlel{attrs = PAttrs,children = Els}) ->
    Type = fxml:get_attr_s(<<"type">>, PAttrs),
    if Type =/= <<"error">> ->
        case Els of
            [#xmlel{name = <<"del_user">>,attrs = Attrs}] ->
                case fxml:get_attr_s(<<"xmlns">>, Attrs) of
                ?NS_MUC_DEL_USER  -> catch qtalk_sql:update_register_mucs(To#jid.lserver,To#jid.luser,To#jid.lserver,From#jid.luser,From#jid.lserver,<<"0">>);
                _ -> ok
                end;
            [#xmlel{name = <<"add_user">>,attrs = Attrs}] ->
                case fxml:get_attr_s(<<"xmlns">>, Attrs) of
                ?NS_MUC_ADD_USER -> catch qtalk_sql:insert_user_register_mucs(To#jid.lserver,To#jid.luser,To#jid.lserver,From#jid.luser,From#jid.lserver);
                _ -> ok
                end;
            [#xmlel{name = <<"query">>,attrs = Attrs,children = [{xmlel,<<"set_register">>,[],[]}]}] ->
                case fxml:get_attr_s(<<"xmlns">>, Attrs) of
                    ?NS_MUC_REGISTER ->
                        case Type of
                            <<"result">> -> catch qtalk_sql:insert_user_register_mucs(To#jid.lserver,
                                                                                     To#jid.luser,
										     To#jid.lserver,
                                                                                     From#jid.luser,
                                                                                     From#jid.lserver);
                            _ -> ok
                        end;
                    _ -> ok
               end;
         _ -> ok
        end;
    true ->
        ok
    end.
	
get_server(From_host,To_host) ->
    if From_host =:= To_host -> From_host;
    true -> lists:nth(1,ejabberd_config:get_myhosts())
    end.

%% 将所有消息通过接口发送个第三方服务
send_push_message(From, To, FromHost, ToHost, Msg, ID, InsertTime) ->
    PushUrls = ejabberd_config:get_option(push_url, fun(Url)-> Url end, undefined),
    case PushUrls of
        undefined -> ok;
        _ -> do_send_push_message(From, To, FromHost, ToHost, Msg, ID, InsertTime, PushUrls)
    end.

do_send_push_message(From, To, FromHost, ToHost, Msg, ID, InsertTime, []) ->
    ok;
do_send_push_message(From, To, FromHost, ToHost, Msg, ID, InsertTime, [PushUrl|Rest]) ->
    case jlib:nodeprep(From) of
    error -> {error, invalid_jid};
    LUser ->
        Packet = #xmlel{attrs = Attrs} = fxml_stream:parse_element(Msg),
        Type = fxml:get_attr_s(<<"type">>, Attrs),
        Realfrom = fxml:get_attr_s(<<"realfrom">>, Attrs),
        Realto = fxml:get_attr_s(<<"realto">>, Attrs),
        {ThirdDirection, CN, UsrType} = case fxml:get_tag_attr_s(<<"channelid">>, Packet) of
            <<"">> -> {?DIRECTION, ?CN, ?USRTYPE};
            ChannelId ->
                {ok, {obj, ChannelIdJson}, []} = rfc4627:decode(ChannelId),
                {proplists:get_value("d", ChannelIdJson, ?DIRECTION),
                 proplists:get_value("cn", ChannelIdJson, ?CN),
                 proplists:get_value("usrType", ChannelIdJson, ?USRTYPE)}
        end,
    
        MsgContent = rfc4627:encode({obj, [{"topic", <<"chat">>},
                                           {"m_from", LUser},
                                           {"from_host", FromHost},
                                           {"m_to", To},
                                           {"to_host", ToHost},
                                           {"m_body", Msg},
                                           {"type", Type},
                                           {"realfrom", Realfrom},
                                           {"realto", Realto},
                                           {"create_time", InsertTime},
                                           {"cn", CN},
                                           {"d", ThirdDirection},
                                           {"usrType", UsrType},
                                           {"msg_id", ID}]}),

        case catch http_client:http_post(binary_to_list(PushUrl), [{"connection", "close"}], "application/json", MsgContent, [], []) of
            Res -> ?DEBUG("the res is ~p~n", [Res])
        end
    end,
    do_send_push_message(From, To, FromHost, ToHost, Msg, ID, InsertTime, Rest).
