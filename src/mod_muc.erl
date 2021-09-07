%%%----------------------------------------------------------------------
%%% File    : mod_muc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : MUC support (XEP-0045)
%%% Created : 19 Mar 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(mod_muc).

-author('alexey@process-one.net').

-protocol({xep, 45, '1.25'}).

-behaviour(gen_server).

-behaviour(gen_mod).

%% API
-export([start_link/2,
	 start/2,
	 stop/1,
	 room_destroyed/4,
	 store_room/4,
	 restore_room/3,
	 forget_room/3,
	 create_room/5,
	 shutdown_rooms/1,
	 process_iq_disco_items/4,
	 broadcast_service_message/2,
	 export/1,
	 import/1,
	 import/3,
     route/3,
	 opts_to_binary/1,
	 can_use_nick/4]).

-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3,
	 mod_opt_type/1, depends/2]).

-export([check_muc_owner/3,handle_recreate_muc/7,recreate_muc_room/6,recreate_muc_room/7,handle_recreate_muc/6]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").
-include("mod_muc.hrl").

-record(state,
	{host = <<"">> :: binary(),
         server_host = <<"">> :: binary(),
         access = {none, none, none, none} :: {atom(), atom(), atom(), atom()},
         history_size = 20 :: non_neg_integer(),
         default_room_opts = [] :: list(),
         room_shaper = none :: shaper:shaper()}).

-define(PROCNAME, ejabberd_mod_muc).

-define(MAX_ROOMS_DISCOITEMS, 100).

-type muc_room_opts() :: [{atom(), any()}].
-callback init(binary(), gen_mod:opts()) -> any().
-callback import(binary(), #muc_room{} | #muc_registered{}) -> ok | pass.
-callback store_room(binary(), binary(), binary(), list()) -> {atomic, any()}.
-callback restore_room(binary(), binary(), binary()) -> muc_room_opts() | error.
-callback forget_room(binary(), binary(), binary()) -> {atomic, any()}.
-callback can_use_nick(binary(), binary(), jid(), binary()) -> boolean().
-callback get_rooms(binary(), binary()) -> [#muc_room{}].
-callback get_nick(binary(), binary(), jid()) -> binary() | error.
-callback set_nick(binary(), binary(), jid(), binary()) -> {atomic, ok | false}.

%%====================================================================
%% API
%%====================================================================
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE,
			  [Host, Opts], []).

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec = {Proc, {?MODULE, start_link, [Host, Opts]},
		 transient, 1000, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    MucHost = str:concat(<<"conference.">>,Host),
    Rooms = shutdown_rooms(MucHost),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:delete_child(ejabberd_sup, Proc),
    {wait, Rooms}.

depends(_Host, _Opts) ->
    [{mod_mam, soft}].

shutdown_rooms(Host) ->
    mod_muc_redis:shutdown_local_rooms(Host).



%% This function is called by a room in three situations:
%% A) The owner of the room destroyed it
%% B) The only participant of a temporary room leaves it
%% C) mod_muc:stop was called, and each room is being terminated
%%    In this case, the mod_muc process died before the room processes
%%    So the message sending must be catched
room_destroyed(Host, Room, Pid, ServerHost) ->
    catch gen_mod:get_module_proc(ServerHost, ?PROCNAME) !
	    {room_destroyed, {Room, Host}, Pid},
    ok.

%% @doc Create a room.
%% If Opts = default, the default room options are used.
%% Else use the passed options as defined in mod_muc_room.
create_room(Host, Name, From, Nick, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, {create, Name, From, Nick, Opts}).

store_room(ServerHost, Host, Name, Opts) ->
    LServer = jid:nameprep(ServerHost),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:store_room(LServer, Host, Name, Opts).

restore_room(ServerHost, Host, Name) ->
    LServer = jid:nameprep(ServerHost),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:restore_room(LServer, Host, Name).

forget_room(ServerHost, Host, Name) ->
    LServer = jid:nameprep(ServerHost),
    ejabberd_hooks:run(remove_room, LServer, [LServer, Name, Host]),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:forget_room(LServer, Host, Name).

process_iq_disco_items(_Host, From, To, IQ) ->
    Res = IQ#iq{type = result,
		sub_el =
		    [#xmlel{name = <<"query">>,
			    attrs = [{<<"xmlns">>, ?NS_DISCO_ITEMS}],
		         children = []}]},
    ejabberd_router:route(To, From, jlib:iq_to_xml(Res)).

can_use_nick(_ServerHost, _Host, _JID, <<"">>) -> false;
can_use_nick(ServerHost, Host, JID, Nick) ->
    LServer = jid:nameprep(ServerHost),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:can_use_nick(LServer, Host, JID, Nick).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Host, Opts]) ->
	?DEBUG("Opts ~p ~n",[Opts]),
    MyHost = gen_mod:get_opt_host(Host, Opts,
				  <<"conference.@HOST@">>),
    Mod = gen_mod:db_mod(Host, Opts, ?MODULE),
    Mod:init(Host, [{host, MyHost}|Opts]),
    catch ets:new(muc_online_users, [bag, named_table, public, {keypos, 2}]),
    catch ets:new(muc_opts,[set,named_table,public,{keypos,1},{write_concurrency, true}, {read_concurrency, true}]),

    mnesia:subscribe(system),
    %%mod_muc_redis:clear_all_muc(MyHost),
    mod_muc_redis:test1(Host, MyHost),
    
    Access = gen_mod:get_opt(access, Opts,
                             fun acl:access_rules_validator/1, all),
    AccessCreate = gen_mod:get_opt(access_create, Opts,
                                   fun acl:access_rules_validator/1, all),
    AccessAdmin = gen_mod:get_opt(access_admin, Opts,
                                  fun acl:access_rules_validator/1,
                                  none),
    AccessPersistent = gen_mod:get_opt(access_persistent, Opts,
				       fun acl:access_rules_validator/1,
                                       all),
    HistorySize = gen_mod:get_opt(history_size, Opts,
                                  fun(I) when is_integer(I), I>=0 -> I end,
                                  20),
    DefRoomOpts1 = gen_mod:get_opt(default_room_options, Opts,
				   fun(L) when is_list(L) -> L end,
				   []),
    DefRoomOpts =
	lists:flatmap(
	  fun({Opt, Val}) ->
		  Bool = fun(B) when is_boolean(B) -> B end,
		  VFun = case Opt of
			     allow_change_subj -> Bool;
			     allow_private_messages -> Bool;
			     allow_query_users -> Bool;
			     allow_user_invites -> Bool;
			     allow_visitor_nickchange -> Bool;
			     allow_visitor_status -> Bool;
			     anonymous -> Bool;
			     captcha_protected -> Bool;
			     logging -> Bool;
			     members_by_default -> Bool;
			     members_only -> Bool;
			     moderated -> Bool;
			     password_protected -> Bool;
			     persistent -> Bool;
				 forbidden_words -> Bool;
			     public -> Bool;
			     public_list -> Bool;
			     mam -> Bool;
			     allow_subscription -> Bool;
			     password -> fun iolist_to_binary/1;
			     title -> fun iolist_to_binary/1;
			     allow_private_messages_from_visitors ->
				 fun(anyone) -> anyone;
				    (moderators) -> moderators;
				    (nobody) -> nobody
				 end;
			     max_users ->
				 fun(I) when is_integer(I), I > 0 -> I end;
                             presence_broadcast ->
                                 fun(L) ->
                                         lists:map(
                                           fun(moderator) -> moderator;
                                              (participant) -> participant;
                                              (visitor) -> visitor
                                           end, L)
                                 end;
			     _ ->
				 ?ERROR_MSG("unknown option ~p with value ~p",
					    [Opt, Val]),
				 fun(_) -> undefined end
			 end,
		  case gen_mod:get_opt(Opt, [{Opt, Val}], VFun) of
		      undefined -> [];
		      NewVal -> [{Opt, NewVal}]
		  end
	  end, DefRoomOpts1),
    RoomShaper = gen_mod:get_opt(room_shaper, Opts,
                                 fun(A) when is_atom(A) -> A end,
                                 none),

    DefOpts =  #state{host = MyHost,server_host = Host,access = {Access, AccessCreate, AccessAdmin, AccessPersistent},
           default_room_opts = DefRoomOpts,history_size = HistorySize,room_shaper = RoomShaper},
    catch ets:insert(muc_opts, {MyHost,DefOpts}),

    ejabberd_router:register_route(MyHost,Host, {apply, ?MODULE, route}),

    {ok, #state{host = MyHost,
		server_host = Host,
		access = {Access, AccessCreate, AccessAdmin, AccessPersistent},
		default_room_opts = DefRoomOpts,
		history_size = HistorySize,
		room_shaper = RoomShaper}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call({create, Room, From, Nick, Opts}, _From,
	    #state{host = Host, server_host = ServerHost,
		   access = Access, default_room_opts = DefOpts,
		   history_size = HistorySize,
		   room_shaper = RoomShaper} = State) ->
    ?DEBUG("MUC: create new room '~s'~n", [Room]),
    NewOpts = case Opts of
		default -> DefOpts;
		_ -> Opts
	      end,
    {ok, Pid} = mod_muc_room:start(
		  Host, ServerHost, Access,
		  Room, HistorySize,
		  RoomShaper, From,
		  Nick, NewOpts),
    register_room(Host, Room, Pid),
    {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({route, From, To, Packet},
	    #state{host = Host, server_host = ServerHost,
		   access = Access, default_room_opts = DefRoomOpts,
		   history_size = HistorySize,
		   room_shaper = RoomShaper} = State) ->
    case catch do_route(Host, ServerHost, Access, HistorySize, RoomShaper,
			From, To, Packet, DefRoomOpts) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p", [Reason]);
	_ ->
	    ok
    end,
    {noreply, State};
handle_info({room_destroyed, RoomHost, Pid}, State) ->
    {Room, Host} = RoomHost,
    mod_muc_redis:room_destroyed(Host,Room,Pid),
    {noreply, State};
handle_info({mnesia_system_event, {mnesia_down, Node}}, State) ->
    clean_table_from_bad_node(Node,State#state.host),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, State) ->
    ejabberd_router:unregister_route(State#state.host),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

do_route(Host, ServerHost, Access, HistorySize, RoomShaper,
	 From, To, Packet, DefRoomOpts) ->
    {AccessRoute, _AccessCreate, _AccessAdmin, _AccessPersistent} = Access,
    case acl:match_rule(ServerHost, AccessRoute, From) of
	allow ->
		?DEBUG("Access2 ~p ~n",[Access]),
	    do_route1(Host, ServerHost, Access, HistorySize, RoomShaper,
		From, To, Packet, DefRoomOpts);
	_ ->
		?DEBUG("Access ~p ~n",[Access]),
	    #xmlel{attrs = Attrs} = Packet,
	    Lang = fxml:get_attr_s(<<"xml:lang">>, Attrs),
	    ErrText = <<"Access denied by service policy">>,
	    Err = jlib:make_error_reply(Packet,
		    ?ERRT_FORBIDDEN(Lang, ErrText)),
	    ejabberd_router:route_error(To, From, Err, Packet)
    end.


do_route1(Host, ServerHost, Access, HistorySize, RoomShaper,
	  From, To, Packet, DefRoomOpts) ->
    {_AccessRoute, AccessCreate, AccessAdmin, _AccessPersistent} = Access,
    {Room, _, Nick} = jid:tolower(To),
    #xmlel{name = Name, attrs = Attrs} = Packet,
    case Room of
      <<"">> ->
	  case Nick of
	    <<"">> ->
		case Name of
		  <<"iq">> ->
		      case jlib:iq_query_info(Packet) of
			#iq{type = get, xmlns = (?NS_DISCO_INFO) = XMLNS,
			    sub_el = _SubEl, lang = Lang} =
			    IQ ->
			    Info = ejabberd_hooks:run_fold(disco_info,
							   ServerHost, [],
							   [ServerHost, ?MODULE,
							    <<"">>, <<"">>]),
			    Res = IQ#iq{type = result,
					sub_el =
					    [#xmlel{name = <<"query">>,
						    attrs =
							[{<<"xmlns">>, XMLNS}],
						    children =
							iq_disco_info(
							  ServerHost, Lang) ++
							  Info}]},
			    ejabberd_router:route(To, From,
						  jlib:iq_to_xml(Res));
			#iq{type = get, xmlns = ?NS_DISCO_ITEMS} = IQ ->
			    spawn(?MODULE, process_iq_disco_items,
				  [Host, From, To, IQ]);
			#iq{type = get, xmlns = (?NS_REGISTER) = XMLNS,
			    lang = Lang, sub_el = _SubEl} =
			    IQ ->
			    Res = IQ#iq{type = result,
					sub_el =
					    [#xmlel{name = <<"query">>,
						    attrs =
							[{<<"xmlns">>, XMLNS}],
						    children =
							iq_get_register_info(ServerHost,
									     Host,
									     From,
									     Lang)}]},
			    ejabberd_router:route(To, From,
						  jlib:iq_to_xml(Res));
			#iq{type = set, xmlns = (?NS_REGISTER) = XMLNS,
			    lang = Lang, sub_el = SubEl} =
			    IQ ->
			    case process_iq_register_set(ServerHost, Host, From,
							 SubEl, Lang)
				of
			      {result, IQRes} ->
				  Res = IQ#iq{type = result,
					      sub_el =
						  [#xmlel{name = <<"query">>,
							  attrs =
							      [{<<"xmlns">>,
								XMLNS}],
							  children = IQRes}]},
				  ejabberd_router:route(To, From,
							jlib:iq_to_xml(Res));
			      {error, Error} ->
				  Err = jlib:make_error_reply(Packet, Error),
				  ejabberd_router:route(To, From, Err)
			    end;
			#iq{type = get, xmlns = (?NS_VCARD) = XMLNS,
			    lang = Lang, sub_el = _SubEl} =
			    IQ ->
			    Res = IQ#iq{type = result,
					sub_el =
					    [#xmlel{name = <<"vCard">>,
						    attrs =
							[{<<"xmlns">>, XMLNS}],
						    children =
							iq_get_vcard(Lang)}]},
			    ejabberd_router:route(To, From,
						  jlib:iq_to_xml(Res));
			#iq{type = get, xmlns = ?NS_MUCSUB,
			    sub_el = #xmlel{name = <<"subscriptions">>} = SubEl} = IQ ->
			      RoomJIDs = get_subscribed_rooms(ServerHost, Host, From),
			      Subs = lists:map(
				       fun(J) ->
					       #xmlel{name = <<"subscription">>,
						      attrs = [{<<"jid">>,
								jid:to_string(J)}]}
				       end, RoomJIDs),
			      Res = IQ#iq{type = result,
					  sub_el = [SubEl#xmlel{children = Subs}]},
			      ejabberd_router:route(To, From, jlib:iq_to_xml(Res));
			#iq{type = get, xmlns = ?NS_MUC_UNIQUE} = IQ ->
			    Res = IQ#iq{type = result,
					sub_el =
					    [#xmlel{name = <<"unique">>,
						    attrs =
							[{<<"xmlns">>,
							  ?NS_MUC_UNIQUE}],
						    children =
							[iq_get_unique(From)]}]},
			    ejabberd_router:route(To, From,
						  jlib:iq_to_xml(Res));
                        #iq{type = get, xmlns = (?NS_USER_MUCS) = XMLNS, lang = Lang, sub_el = _SubEl} = IQ ->
                            Res = IQ#iq{type = result,
                                        sub_el = [#xmlel{name = <<"query">>,
                                                         attrs = [{<<"xmlns">>, XMLNS}],
                                                         children = iq_get_user_mucs(ServerHost, Host, From, Lang)}]},
                            ejabberd_router:route(To, From, jlib:iq_to_xml(Res));
			#iq{} ->
			    Err = jlib:make_error_reply(Packet,
							?ERR_FEATURE_NOT_IMPLEMENTED),
			    ejabberd_router:route(To, From, Err);
			_ -> ok
		      end;
		  <<"message">> ->
		      case fxml:get_attr_s(<<"type">>, Attrs) of
			<<"error">> -> ok;
                        <<"readmark">> -> readmark:readmark_message(From,To,Packet);
			_ ->
			    case acl:match_rule(ServerHost, AccessAdmin, From)
				of
			      allow ->
				  Msg = fxml:get_path_s(Packet,
						       [{elem, <<"body">>},
							cdata]),
				  broadcast_service_message(Host, Msg);
			      _ ->
				  Lang = fxml:get_attr_s(<<"xml:lang">>, Attrs),
				  ErrText =
				      <<"Only service administrators are allowed "
					"to send service messages">>,
				  Err = jlib:make_error_reply(Packet,
							      ?ERRT_FORBIDDEN(Lang,
									      ErrText)),
				  ejabberd_router:route(To, From, Err)
			    end
		      end;
		  <<"presence">> -> ok
		end;
	    _ ->
		case fxml:get_attr_s(<<"type">>, Attrs) of
		  <<"error">> -> ok;
		  <<"result">> -> ok;
		  _ ->
		      Err = jlib:make_error_reply(Packet,
						  ?ERR_ITEM_NOT_FOUND),
		      ejabberd_router:route(To, From, Err)
		end
	  end;
      _ ->
        case mod_muc_redis:get_muc_room_pid(Room, Host) of
		[] ->
		    Type = fxml:get_attr_s(<<"type">>, Attrs),    
                    case {Name, Type} of
			{<<"presence">>, <<"">>}  ->
			    case check_user_can_create_room(ServerHost,
				    AccessCreate, From, Room) and
				check_create_roomid(ServerHost, Room) of
				true ->
				    {ok, Pid} = start_new_room(Host, ServerHost, Access,
					    Room, HistorySize,
					    RoomShaper, From, Nick, DefRoomOpts),
				    register_room(Host, Room, Pid),
				    mod_muc_room:route(Pid, From, Nick, Packet),
				    ok;
				false ->
				    Lang = fxml:get_attr_s(<<"xml:lang">>, Attrs),
				    ErrText = <<"Room creation is denied by service policy">>,
				    Err = jlib:make_error_reply(
					    Packet, ?ERRT_FORBIDDEN(Lang, ErrText)),
				    ejabberd_router:route(To, From, Err)
			    end;
		        {<<"message">>,<<"groupchat">>} ->
                		recreate_muc_room(ServerHost,Host,Room,From,Nick,Packet, true);
		        {<<"message">>,<<"chat">>} ->
        		        recreate_muc_room(ServerHost,Host,Room,From,Nick,Packet, true);
		        {<<"message">>,<<"normal">>} ->
		                recreate_muc_room(ServerHost,Host,Room,From,Nick,Packet, true);
        		{<<"iq">>,_} ->
	                	catch mod_handle_muc_iq:handle_iq(ServerHost,Room,Host,Nick,From,To,Packet);
			_ ->
			    Lang = fxml:get_attr_s(<<"xml:lang">>, Attrs),
			    ErrText = <<"Conference room does not exist">>,
			    Err = jlib:make_error_reply(Packet,
				    ?ERRT_ITEM_NOT_FOUND(Lang, ErrText)),
			    ejabberd_router:route(To, From, Err)
		    end;
		[R] ->
		    Pid = R#muc_online_room.pid,
		    ?DEBUG("MUC: send to process ~p~n", [Pid]),
		    mod_muc_room:route(Pid, From, Nick, Packet),
		    ok
	    end
    end.

check_user_can_create_room(ServerHost, AccessCreate,
			   From, _RoomID) ->
    case acl:match_rule(ServerHost, AccessCreate, From) of
      allow -> true;
      _ -> false
    end.

check_create_roomid(ServerHost, RoomID) ->
    Max = gen_mod:get_module_opt(ServerHost, ?MODULE, max_room_id,
				 fun(infinity) -> infinity;
				    (I) when is_integer(I), I>0 -> I
				 end, infinity),
    Regexp = gen_mod:get_module_opt(ServerHost, ?MODULE, regexp_room_id,
				    fun iolist_to_binary/1, ""),
    (byte_size(RoomID) =< Max) and
    (re:run(RoomID, Regexp, [unicode, {capture, none}]) == match).

get_rooms(ServerHost, Host) ->
    LServer = jid:nameprep(ServerHost),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:get_rooms(LServer, Host).

start_new_room(Host, ServerHost, Access, Room,
	    HistorySize, RoomShaper, From,
	    Nick, DefRoomOpts) ->
    case restore_room(ServerHost, Host, Room) of
	error ->
	    ?INFO_MSG("MUC: open new room '~p'~n", [{ServerHost, Host, Room}]),
	    mod_muc_room:start(Host, ServerHost, Access, Room,
		HistorySize, RoomShaper,
		From, Nick, DefRoomOpts);
	Opts ->
	    ?INFO_MSG("MUC: restore room '~p'~n", [{Room, Opts}]),
	    mod_muc_room:start(Host, ServerHost, Access, Room,
		HistorySize, RoomShaper, Opts)
    end.

register_room(Host, Room, Pid) ->
    mod_muc_redis:register_room(Host, Room, Pid).


iq_disco_info(ServerHost, Lang) ->
    [#xmlel{name = <<"identity">>,
	    attrs =
		[{<<"category">>, <<"conference">>},
		 {<<"type">>, <<"text">>},
		 {<<"name">>,
		  translate:translate(Lang, <<"Chatrooms">>)}],
	    children = []},
     #xmlel{name = <<"feature">>,
	    attrs = [{<<"var">>, ?NS_DISCO_INFO}], children = []},
     #xmlel{name = <<"feature">>,
	    attrs = [{<<"var">>, ?NS_DISCO_ITEMS}], children = []},
     #xmlel{name = <<"feature">>,
	    attrs = [{<<"var">>, ?NS_MUC}], children = []},
     #xmlel{name = <<"feature">>,
	    attrs = [{<<"var">>, ?NS_MUC_UNIQUE}], children = []},
     #xmlel{name = <<"feature">>,
	    attrs = [{<<"var">>, ?NS_REGISTER}], children = []},
     #xmlel{name = <<"feature">>,
	    attrs = [{<<"var">>, ?NS_RSM}], children = []},
     #xmlel{name = <<"feature">>,
	    attrs = [{<<"var">>, ?NS_MUCSUB}], children = []},
     #xmlel{name = <<"feature">>,
	    attrs = [{<<"var">>, ?NS_VCARD}], children = []}] ++
	case gen_mod:is_loaded(ServerHost, mod_mam) of
	    true ->
		[#xmlel{name = <<"feature">>,
			attrs = [{<<"var">>, ?NS_MAM_TMP}]},
		 #xmlel{name = <<"feature">>,
			attrs = [{<<"var">>, ?NS_MAM_0}]},
		 #xmlel{name = <<"feature">>,
			attrs = [{<<"var">>, ?NS_MAM_1}]}];
	    false ->
		[]
	end.

get_subscribed_rooms(ServerHost, Host, From) ->
    Rooms = get_rooms(ServerHost, Host),
    BareFrom = jid:remove_resource(From),
    lists:flatmap(
      fun(#muc_room{name_host = {Name, _}, opts = Opts}) ->
	      Subscribers = proplists:get_value(subscribers, Opts, []),
	      case lists:keymember(BareFrom, 1, Subscribers) of
		  true -> [jid:make(Name, Host, <<>>)];
		  false -> []
	      end;
	 (_) ->
	      []
      end, Rooms).

-define(XFIELD(Type, Label, Var, Val),
	#xmlel{name = <<"field">>,
	       attrs =
		   [{<<"type">>, Type},
		    {<<"label">>, translate:translate(Lang, Label)},
		    {<<"var">>, Var}],
	       children =
		   [#xmlel{name = <<"value">>, attrs = [],
			   children = [{xmlcdata, Val}]}]}).

iq_get_unique(From) ->
    {xmlcdata,
     p1_sha:sha(term_to_binary([From, p1_time_compat:timestamp(),
			     randoms:get_string()]))}.

get_nick(ServerHost, Host, From) ->
    LServer = jid:nameprep(ServerHost),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:get_nick(LServer, Host, From).

iq_get_register_info(ServerHost, Host, From, Lang) ->
    {Nick, Registered} = case get_nick(ServerHost, Host,
				       From)
			     of
			   error -> {<<"">>, []};
			   N ->
			       {N,
				[#xmlel{name = <<"registered">>, attrs = [],
					children = []}]}
			 end,
    Registered ++
      [#xmlel{name = <<"instructions">>, attrs = [],
	      children =
		  [{xmlcdata,
		    translate:translate(Lang,
					<<"You need a client that supports x:data "
					  "to register the nickname">>)}]},
       #xmlel{name = <<"x">>,
	      attrs = [{<<"xmlns">>, ?NS_XDATA},
		       {<<"type">>, <<"form">>}],
	      children =
		  [#xmlel{name = <<"title">>, attrs = [],
			  children =
			      [{xmlcdata,
				<<(translate:translate(Lang,
						       <<"Nickname Registration at ">>))/binary,
				  Host/binary>>}]},
		   #xmlel{name = <<"instructions">>, attrs = [],
			  children =
			      [{xmlcdata,
				translate:translate(Lang,
						    <<"Enter nickname you want to register">>)}]},
		   ?XFIELD(<<"text-single">>, <<"Nickname">>, <<"nick">>,
			   Nick)]}].

set_nick(ServerHost, Host, From, Nick) ->
    LServer = jid:nameprep(ServerHost),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:set_nick(LServer, Host, From, Nick).

iq_set_register_info(ServerHost, Host, From, Nick,
		     Lang) ->
    case set_nick(ServerHost, Host, From, Nick) of
      {atomic, ok} -> {result, []};
      {atomic, false} ->
	  ErrText = <<"That nickname is registered by another "
		      "person">>,
	  {error, ?ERRT_CONFLICT(Lang, ErrText)};
      _ ->
	  Txt = <<"Database failure">>,
	  {error, ?ERRT_INTERNAL_SERVER_ERROR(Lang, Txt)}
    end.

process_iq_register_set(ServerHost, Host, From, SubEl,
			Lang) ->
    #xmlel{children = Els} = SubEl,
    case fxml:get_subtag(SubEl, <<"remove">>) of
      false ->
	  case fxml:remove_cdata(Els) of
	    [#xmlel{name = <<"x">>} = XEl] ->
		case {fxml:get_tag_attr_s(<<"xmlns">>, XEl),
		      fxml:get_tag_attr_s(<<"type">>, XEl)}
		    of
		  {?NS_XDATA, <<"cancel">>} -> {result, []};
		  {?NS_XDATA, <<"submit">>} ->
		      XData = jlib:parse_xdata_submit(XEl),
		      case XData of
			invalid ->
			      Txt = <<"Incorrect data form">>,
			      {error, ?ERRT_BAD_REQUEST(Lang, Txt)};
			_ ->
			    case lists:keysearch(<<"nick">>, 1, XData) of
			      {value, {_, [Nick]}} when Nick /= <<"">> ->
				  iq_set_register_info(ServerHost, Host, From,
						       Nick, Lang);
			      _ ->
				  ErrText =
				      <<"You must fill in field \"Nickname\" "
					"in the form">>,
				  {error, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)}
			    end
		      end;
		  _ -> {error, ?ERR_BAD_REQUEST}
		end;
	    _ -> {error, ?ERR_BAD_REQUEST}
	  end;
      _ ->
	  iq_set_register_info(ServerHost, Host, From, <<"">>,
			       Lang)
    end.

iq_get_vcard(Lang) ->
    [#xmlel{name = <<"FN">>, attrs = [],
	    children = [{xmlcdata, <<"ejabberd/mod_muc">>}]},
     #xmlel{name = <<"URL">>, attrs = [],
	    children = [{xmlcdata, ?EJABBERD_URI}]},
     #xmlel{name = <<"DESC">>, attrs = [],
	    children =
		[{xmlcdata,
		  <<(translate:translate(Lang,
					 <<"ejabberd MUC module">>))/binary,
		    "\nCopyright (c) 2003-2016 ProcessOne">>}]}].

broadcast_service_message(Host, Msg) ->
    lists:foreach(
	fun(#muc_online_room{pid = Pid}) ->
		gen_fsm:send_all_state_event(
		    Pid, {service_message, Msg})
	end, get_vh_rooms(Host)).


get_vh_rooms(Host) ->
    mod_muc_redis:get_vh_rooms(Host).


clean_table_from_bad_node(Node, Host) ->
    mod_muc_redis:clean_table_from_node(Node,Host).


opts_to_binary(Opts) ->
    lists:map(
      fun({title, Title}) ->
              {title, iolist_to_binary(Title)};
         ({description, Desc}) ->
              {description, iolist_to_binary(Desc)};
         ({password, Pass}) ->
              {password, iolist_to_binary(Pass)};
         ({subject, Subj}) ->
              {subject, iolist_to_binary(Subj)};
         ({subject_author, Author}) ->
              {subject_author, iolist_to_binary(Author)};
         ({affiliations, Affs}) ->
              {affiliations, lists:map(
                               fun({{U, S, R}, Aff}) ->
                                       NewAff =
                                           case Aff of
                                               {A, Reason} ->
                                                   {A, iolist_to_binary(Reason)};
                                               _ ->
                                                   Aff
                                           end,
                                       {{iolist_to_binary(U),
                                         iolist_to_binary(S),
                                         iolist_to_binary(R)},
                                        NewAff}
                               end, Affs)};
         ({captcha_whitelist, CWList}) ->
              {captcha_whitelist, lists:map(
                                    fun({U, S, R}) ->
                                            {iolist_to_binary(U),
                                             iolist_to_binary(S),
                                             iolist_to_binary(R)}
                                    end, CWList)};
         (Opt) ->
              Opt
      end, Opts).

export(LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:export(LServer).

import(LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:import(LServer).

import(LServer, DBType, Data) ->
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    Mod:import(LServer, Data).

mod_opt_type(access) ->
    fun acl:access_rules_validator/1;
mod_opt_type(access_admin) ->
    fun acl:access_rules_validator/1;
mod_opt_type(access_create) ->
    fun acl:access_rules_validator/1;
mod_opt_type(access_persistent) ->
    fun acl:access_rules_validator/1;
mod_opt_type(db_type) -> fun(T) -> ejabberd_config:v_db(?MODULE, T) end;
mod_opt_type(default_room_options) ->
    fun (L) when is_list(L) -> L end;
mod_opt_type(history_size) ->
    fun (I) when is_integer(I), I >= 0 -> I end;
mod_opt_type(host) -> fun iolist_to_binary/1;
mod_opt_type(max_room_desc) ->
    fun (infinity) -> infinity;
	(I) when is_integer(I), I > 0 -> I
    end;
mod_opt_type(max_room_id) ->
    fun (infinity) -> infinity;
	(I) when is_integer(I), I > 0 -> I
    end;
mod_opt_type(regexp_room_id) ->
    fun iolist_to_binary/1;
mod_opt_type(max_room_name) ->
    fun (infinity) -> infinity;
	(I) when is_integer(I), I > 0 -> I
    end;
mod_opt_type(max_user_conferences) ->
    fun (I) when is_integer(I), I > 0 -> I end;
mod_opt_type(max_users) ->
    fun (I) when is_integer(I), I > 0 -> I end;
mod_opt_type(max_users_admin_threshold) ->
    fun (I) when is_integer(I), I > 0 -> I end;
mod_opt_type(max_users_presence) ->
    fun (MUP) when is_integer(MUP) -> MUP end;
mod_opt_type(min_message_interval) ->
    fun (MMI) when is_number(MMI) -> MMI end;
mod_opt_type(min_presence_interval) ->
    fun (I) when is_number(I), I >= 0 -> I end;
mod_opt_type(room_shaper) ->
    fun (A) when is_atom(A) -> A end;
mod_opt_type(user_message_shaper) ->
    fun (A) when is_atom(A) -> A end;
mod_opt_type(forbidden_words) ->
    fun (A) when is_atom(A) -> A end;
mod_opt_type(user_presence_shaper) ->
    fun (A) when is_atom(A) -> A end;
mod_opt_type(_) ->
    [access, access_admin, access_create, access_persistent,
     db_type, default_room_options, history_size, host,
     max_room_desc, max_room_id, max_room_name, regexp_room_id,
     max_user_conferences, max_users,
     max_users_admin_threshold, max_users_presence,
     min_message_interval, min_presence_interval,
     room_shaper, user_message_shaper, user_presence_shaper].

get_affction_opts(Opts) ->
    lists:flatmap(fun({affiliations, Affs}) ->
         lists:flatmap(fun({{U, S, _R}, {NewAff,<<"">>}}) ->
             [{jlib:jid_to_string({iolist_to_binary(U), iolist_to_binary(S),<<"">>}), NewAff}];
         (_) -> []
         end, Affs);
    (_) -> []
    end, Opts).

route(From,To,Packet) ->
    ?DEBUG("From ~p ,To ~p,Packet ~p ~n",[From,To,Packet]),
    case catch ets:lookup(muc_opts, To#jid.lserver) of
        [{ _,#state{host = Host, server_host = ServerHost,
            access = Access, default_room_opts = DefRoomOpts,
            history_size = HistorySize,
            room_shaper = RoomShaper}}] ->
                case catch do_route(Host, ServerHost, Access, HistorySize, RoomShaper,
                    From, To, Packet, DefRoomOpts) of
                    {'EXIT', Reason} -> ?ERROR_MSG("~p", [Reason]), ok;
                    _ -> ok
                end;
        _ -> self() ! {From ,To ,Packet}
    end.

iq_get_user_mucs(_ServerHost, _Host, From, _Lang) ->
    Mucs = case catch qtalk_sql:get_user_register_mucs(From#jid.lserver,From#jid.luser, From#jid.lserver) of
        {selected,_,Res}  when is_list(Res) -> Res;
        _ -> []
    end,

    lists:map(fun ([Muc_Name,H]) ->
        #xmlel{name = <<"muc_rooms">>,
            attrs = [{<<"name">>,Muc_Name},{<<"host">>,H}],
            children = []} end,Mucs).

recreate_muc_room(ServerHost,Host,Room,From,Nick,Packet) ->
    do_recreate_muc_room(ServerHost,Host,Room,From,Nick,Packet,true).

recreate_muc_room(ServerHost,Host,Room,From,Nick,Packet,Flag) ->
    do_recreate_muc_room(ServerHost,Host,Room,From,Nick,Packet,Flag).

do_recreate_muc_room(ServerHost,Host,Room,From,Nick,Packet,Flag) ->
    if From#jid.lserver =:= ServerHost  ->
        case catch ejabberd_sql:sql_query(ServerHost,
            [<<"select created_at from user_register_mucs where username = '">>,From#jid.luser,<<"' and muc_name = '">>,
                    Room,<<"' and domain = '">>,Host,<<"' and registed_flag = 1;">>]) of
            {selected, _ , [[_T]]} -> handle_recreate_muc(ServerHost,Room,Host,From,Nick,Packet,Flag);
            _ -> ok
        end;
    true ->
        case catch ejabberd_sql:sql_query(ServerHost,
            [<<"select subscribe_flag from muc_room_users where username = '">>,From#jid.luser,<<"' and muc_name = '">>,
                Room,<<"' and host = '">>,From#jid.lserver,<<"';">>]) of
            {selected, _ , [[_T]]} -> handle_recreate_muc(ServerHost,Room,Host,From,Nick,Packet,Flag);
            _ -> ok
        end
    end.

handle_recreate_muc(Server, Muc, Host, From, Nick, Packet) ->
    handle_recreate_muc(Server, Muc, Host, From, Nick, Packet, true).

handle_recreate_muc(Server, Muc, Host, From, Nick, Packet, Flag) ->
    P = case catch ets:lookup(muc_opts, Host) of
    [{ _,#state{host = Host, access = Access, default_room_opts = DefRoomOpts,
        history_size = HistorySize,
        room_shaper = RoomShaper}}] ->
        {ok, Pid} =  start_new_room(Host, Server, Access, Muc, HistorySize,  RoomShaper, From,  Nick, DefRoomOpts),
        register_room(Host, Muc, Pid),
        Pid;
    _ ->
        case catch qtalk_sql:get_muc_opts(Server,Muc, Host) of
        {selected,_,[[Opts]]} ->
            Bopts = opts_to_binary(ejabberd_sql:decode_term(Opts)),
            {ok, Pid} = mod_muc_room:start(Host,Server,{all,all,none,all},Muc,20,none,Bopts),
            register_room(Host, Muc, Pid),
            Pid;
        _ -> false 
        end
    end,

    case {P, Flag} of
        {false, _} -> ok;
        {_, false} -> ok;
        {_, true} -> mod_muc_room:route(P, From, Nick, Packet)
    end.

check_muc_owner(LServer,Muc,User) ->
    Host = str:concat(<<"conference.">>,LServer),
    case catch qtalk_sql:get_muc_opts(LServer,Muc,Host) of
     {selected,_,[[Opts]]} ->
         Affction = get_affction_opts(ejabberd_sql:decode_term(Opts)),
         Aff = proplists:get_value(User,Affction,none),
         case Aff  of
         'admin' -> true;
         'owner' -> true;
         _ -> false
         end;
    _ -> false
    end.
