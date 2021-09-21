%%%----------------------------------------------------------------------
%%% File    : ejabberd_c2s.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Serve C2S connection
%%% Created : 16 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_protobuf_c2s).

-behaviour(ejabberd_config).

-author('alexey@process-one.net').

-protocol({xep, 78, '2.5'}).
-protocol({xep, 138, '2.0'}).
-protocol({xep, 198, '1.3'}).
-protocol({xep, 356, '7.1'}).

-update_info({update, 0}).

-define(GEN_FSM, p1_fsm).

-behaviour(?GEN_FSM).

%% External exports
-export([start/2,
	 stop/1,
	 start_link/2,
	 close/1,
	 send_text/2,
	 send_element/2,
	 socket_type/0,
	 get_presence/1,
	 get_last_presence/1,
	 get_aux_field/2,
	 set_aux_field/3,
	 del_aux_field/2,
	 get_subscription/2,
	 get_queued_stanzas/1,
	 get_csi_state/1,
	 set_csi_state/2,
	 get_resume_timeout/1,
	 set_resume_timeout/2,
	 send_filtered/5,
	 broadcast/4,
	 get_subscribed/1,
         transform_listen_option/2]).

-export([init/1, wait_for_stream/2, wait_for_auth/2,
	 wait_for_feature_request/2, wait_for_bind/2,
	 wait_for_sasl_response/2,
	 wait_for_resume/2, session_established/2,
	 handle_event/3, handle_sync_event/4, code_change/4,
	 handle_info/3, terminate/3, print_state/1, opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-include("mod_privacy.hrl").

-define(SETS, gb_sets).
-define(DICT, dict).

%% pres_a contains all the presence available send (either through roster mechanism or directed).
%% Directed presence unavailable remove user from pres_a.
-record(state, {socket,
		sockmod,
		socket_monitor,
		xml_socket,
		streamid,
		sasl_state,
		access,
		shaper,
		zlib = false,
		tls = false,
		tls_required = false,
		tls_enabled = false,
		tls_options = [],
		authenticated = false,
		jid,
		user = <<"">>, server = <<"">>, resource = <<"">>,
		sid,
		pres_t = ?SETS:new(),
		pres_f = ?SETS:new(),
		pres_a = ?SETS:new(),
		pres_last,
		pres_timestamp,
		privacy_list = #userlist{},
		conn = unknown,
		auth_module = unknown,
		ip,
		aux_fields = [],
		csi_state = active,
		mgmt_state,
		mgmt_xmlns,
		mgmt_queue,
		mgmt_max_queue,
		mgmt_pending_since,
		mgmt_timeout,
		mgmt_max_timeout,
		mgmt_ack_timeout,
		mgmt_ack_timer,
		mgmt_resend,
		mgmt_stanzas_in = 0,
		mgmt_stanzas_out = 0,
		mgmt_stanzas_req = 0,
		ask_offline = true,
	        key = <<"">>,
		lang = <<"">>}).

%-define(DBGFSM, true).

-ifdef(DBGFSM).

-define(FSMOPTS, [{debug, [trace]}]).

-else.

-define(FSMOPTS, []).

-endif.

%% This is the timeout to apply between event when starting a new
%% session:
-define(C2S_OPEN_TIMEOUT, 60000).

-define(C2S_HIBERNATE_TIMEOUT, ejabberd_config:get_option(c2s_hibernate, fun(X) when is_integer(X); X == hibernate-> X end, 90000)).

-define(STREAM_HEADER,
	<<"<?xml version='1.0'?><stream:stream "
	  "xmlns='jabber:client' xmlns:stream='http://et"
	  "herx.jabber.org/streams' id='~s' from='~s'~s"
	  "~s>">>).

-define(STREAM_TRAILER, <<"</stream:stream>">>).

-define(INVALID_NS_ERR, ?SERR_INVALID_NAMESPACE).

-define(INVALID_XML_ERR, ?SERR_XML_NOT_WELL_FORMED).

-define(HOST_UNKNOWN_ERR, ?SERR_HOST_UNKNOWN).

-define(POLICY_VIOLATION_ERR(Lang, Text),
	?SERRT_POLICY_VIOLATION(Lang, Text)).

-define(INVALID_FROM, ?SERR_INVALID_FROM).

%% XEP-0198:

-define(IS_STREAM_MGMT_TAG(Name),
	(Name == <<"enable">>) or
	(Name == <<"resume">>) or
	(Name == <<"a">>) or
	(Name == <<"r">>)).

-define(IS_SUPPORTED_MGMT_XMLNS(Xmlns),
	(Xmlns == ?NS_STREAM_MGMT_2) or
	(Xmlns == ?NS_STREAM_MGMT_3)).

-define(MGMT_FAILED(Condition, Attrs),
	#xmlel{name = <<"failed">>,
	       attrs = Attrs,
	       children = [#xmlel{name = Condition,
				  attrs = [{<<"xmlns">>, ?NS_STANZAS}],
				  children = []}]}).

-define(MGMT_BAD_REQUEST(Xmlns),
	?MGMT_FAILED(<<"bad-request">>, [{<<"xmlns">>, Xmlns}])).

-define(MGMT_SERVICE_UNAVAILABLE(Xmlns),
	?MGMT_FAILED(<<"service-unavailable">>, [{<<"xmlns">>, Xmlns}])).

-define(MGMT_UNEXPECTED_REQUEST(Xmlns),
	?MGMT_FAILED(<<"unexpected-request">>, [{<<"xmlns">>, Xmlns}])).

-define(MGMT_UNSUPPORTED_VERSION(Xmlns),
	?MGMT_FAILED(<<"unsupported-version">>, [{<<"xmlns">>, Xmlns}])).

-define(MGMT_ITEM_NOT_FOUND(Xmlns),
	?MGMT_FAILED(<<"item-not-found">>, [{<<"xmlns">>, Xmlns}])).

-define(MGMT_ITEM_NOT_FOUND_H(Xmlns, NumStanzasIn),
	?MGMT_FAILED(<<"item-not-found">>,
		     [{<<"xmlns">>, Xmlns},
		      {<<"h">>, jlib:integer_to_binary(NumStanzasIn)}])).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(SockData, Opts) ->
    ?GEN_FSM:start(ejabberd_protobuf_c2s,
		   [SockData, Opts],
		   fsm_limit_opts(Opts) ++ ?FSMOPTS).

start_link(SockData, Opts) ->
    (?GEN_FSM):start_link(ejabberd_protobuf_c2s,
			  [SockData, Opts],
			  fsm_limit_opts(Opts) ++ ?FSMOPTS).

socket_type() -> protobuf.

%% Return Username, Resource and presence information
get_presence(FsmRef) ->
    (?GEN_FSM):sync_send_all_state_event(FsmRef,
					 {get_presence}, 1000).
get_last_presence(FsmRef) ->
    (?GEN_FSM):sync_send_all_state_event(FsmRef,
					 {get_last_presence}, 1000).

get_aux_field(Key, #state{aux_fields = Opts}) ->
    case lists:keysearch(Key, 1, Opts) of
      {value, {_, Val}} -> {ok, Val};
      _ -> error
    end.

set_aux_field(Key, Val,
	      #state{aux_fields = Opts} = State) ->
    Opts1 = lists:keydelete(Key, 1, Opts),
    State#state{aux_fields = [{Key, Val} | Opts1]}.

del_aux_field(Key, #state{aux_fields = Opts} = State) ->
    Opts1 = lists:keydelete(Key, 1, Opts),
    State#state{aux_fields = Opts1}.

get_subscription(From = #jid{}, StateData) ->
    get_subscription(jid:tolower(From), StateData);
get_subscription(LFrom, StateData) ->
    LBFrom = setelement(3, LFrom, <<"">>),
    F = (?SETS):is_element(LFrom, StateData#state.pres_f)
	  orelse
	  (?SETS):is_element(LBFrom, StateData#state.pres_f),
    T = (?SETS):is_element(LFrom, StateData#state.pres_t)
	  orelse
	  (?SETS):is_element(LBFrom, StateData#state.pres_t),
    if F and T -> both;
       F -> from;
       T -> to;
       true -> none
    end.

get_queued_stanzas(#state{mgmt_queue = Queue} = StateData) ->
    lists:map(fun({_N, Time, El}) ->
		      add_resent_delay_info(StateData, El, Time)
	      end, queue:to_list(Queue)).

get_csi_state(#state{csi_state = CsiState}) ->
    CsiState.

set_csi_state(#state{} = StateData, CsiState) ->
    StateData#state{csi_state = CsiState};
set_csi_state(FsmRef, CsiState) ->
    FsmRef ! {set_csi_state, CsiState}.

get_resume_timeout(#state{mgmt_timeout = Timeout}) ->
    Timeout.

set_resume_timeout(#state{} = StateData, Timeout) ->
    StateData#state{mgmt_timeout = Timeout};
set_resume_timeout(FsmRef, Timeout) ->
    FsmRef ! {set_resume_timeout, Timeout}.

send_filtered(FsmRef, Feature, From, To, Packet) ->
    FsmRef ! {send_filtered, Feature, From, To, Packet}.

broadcast(FsmRef, Type, From, Packet) ->
    FsmRef ! {broadcast, Type, From, Packet}.

stop(FsmRef) -> (?GEN_FSM):send_event(FsmRef, stop).

close(FsmRef) -> (?GEN_FSM):send_event(FsmRef, closed).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------

init([{SockMod, Socket}, Opts]) ->
    Access = gen_mod:get_opt(access, Opts,
			     fun acl:access_rules_validator/1, all),
    Shaper = gen_mod:get_opt(shaper, Opts,
			     fun acl:shaper_rules_validator/1, none),
    XMLSocket = case lists:keysearch(xml_socket, 1, Opts) of
		  {value, {_, XS}} -> XS;
		  _ -> false
		end,
    Zlib = proplists:get_bool(zlib, Opts),
    StartTLS = proplists:get_bool(starttls, Opts),
    StartTLSRequired = proplists:get_bool(starttls_required, Opts),
    TLSEnabled = proplists:get_bool(tls, Opts),
    TLS = StartTLS orelse
	    StartTLSRequired orelse TLSEnabled,
    TLSOpts1 = lists:filter(fun ({certfile, _}) -> true;
				({ciphers, _}) -> true;
				({dhfile, _}) -> true;
				(_) -> false
			    end,
			    Opts),
    TLSOpts2 = case lists:keysearch(protocol_options, 1, Opts) of
                   {value, {_, O}} ->
                       [_|ProtocolOptions] = lists:foldl(
                                    fun(X, Acc) -> X ++ Acc end, [],
                                    [["|" | binary_to_list(Opt)] || Opt <- O, is_binary(Opt)]
                                   ),
                        [{protocol_options, iolist_to_binary(ProtocolOptions)} | TLSOpts1];
                   _ -> TLSOpts1
               end,
    TLSOpts3 = case proplists:get_bool(tls_compression, Opts) of
                   false -> [compression_none | TLSOpts2];
                   true -> TLSOpts2
               end,
    TLSOpts = [verify_none | TLSOpts3],
    StreamMgmtEnabled = proplists:get_value(stream_management, Opts, true),
    StreamMgmtState = if StreamMgmtEnabled -> inactive;
			 true -> disabled
		      end,
    MaxAckQueue = case proplists:get_value(max_ack_queue, Opts) of
		    Limit when is_integer(Limit), Limit > 0 -> Limit;
		    infinity -> infinity;
		    _ -> 1000
		  end,
    ResumeTimeout = case proplists:get_value(resume_timeout, Opts) of
		      RTimeo when is_integer(RTimeo), RTimeo >= 0 -> RTimeo;
		      _ -> 300
		    end,
    MaxResumeTimeout = case proplists:get_value(max_resume_timeout, Opts) of
			 Max when is_integer(Max), Max >= ResumeTimeout -> Max;
			 _ -> ResumeTimeout
		       end,
    AckTimeout = case proplists:get_value(ack_timeout, Opts) of
		   ATimeo when is_integer(ATimeo), ATimeo > 0 -> ATimeo * 1000;
		   infinity -> undefined;
		   _ -> 60000
		 end,
    ResendOnTimeout = case proplists:get_value(resend_on_timeout, Opts) of
			Resend when is_boolean(Resend) -> Resend;
			if_offline -> if_offline;
			_ -> false
		      end,
    IP = peerip(SockMod, Socket),
    Socket1 = if TLSEnabled andalso
		 SockMod /= ejabberd_frontend_socket -> SockMod:starttls('probuff', Socket, TLSOpts, <<"">>);
		 true -> Socket
	      end,
    SocketMonitor = SockMod:monitor(Socket1),
    StateData = #state{socket = Socket1, sockmod = SockMod,
		       socket_monitor = SocketMonitor,
		       xml_socket = XMLSocket, zlib = Zlib, tls = TLS,
		       tls_required = StartTLSRequired,
		       tls_enabled = TLSEnabled, tls_options = TLSOpts,
		       sid = ejabberd_sm:make_sid(), streamid = new_id(),
		       access = Access, shaper = Shaper, ip = IP,
		       mgmt_state = StreamMgmtState,
		       mgmt_max_queue = MaxAckQueue,
		       mgmt_timeout = ResumeTimeout,
		       mgmt_max_timeout = MaxResumeTimeout,
		       mgmt_ack_timeout = AckTimeout,
		       mgmt_resend = ResendOnTimeout},
    {ok, wait_for_stream, StateData, ?C2S_OPEN_TIMEOUT}.

%% Return list of all available resources of contacts,
get_subscribed(FsmRef) ->
    (?GEN_FSM):sync_send_all_state_event(FsmRef,
					 get_subscribed, 1000).

wait_for_stream({xmlstreamstart, _Name, Attrs}, StateData) ->
    case fxml:get_attr_s(<<"xmlns:stream">>, Attrs) of
	?NS_STREAM ->
	    Server =
		case StateData#state.server of
		<<"">> ->
		    jid:nameprep(fxml:get_attr_s(<<"to">>, Attrs));
		S -> S
	    end,
	    Lang = case fxml:get_attr_s(<<"xml:lang">>, Attrs) of
		Lang1 when byte_size(Lang1) =< 35 ->
		    %% As stated in BCP47, 4.4.1:
		    %% Protocols or specifications that
		    %% specify limited buffer sizes for
		    %% language tags MUST allow for
		    %% language tags of at least 35 characters.
		    Lang1;
		_ ->
		    %% Do not store long language tag to
		    %% avoid possible DoS/flood attacks
		    <<"">>
	    end,
	    StreamVersion = case fxml:get_attr_s(<<"version">>, Attrs) of
			      <<"1.0">> ->
				  <<"1.0">>;
			      _ ->
				  <<"">>
			  end,
	    IsBlacklistedIP = is_ip_blacklisted(StateData#state.ip, Lang),
	  %  case lists:member(Server, ?MYHOSTS) of
	            case catch lists:member(Server, ?MYHOSTS) orelse lists:member(Server,ejabberd_reload_hosts:get_hosts()) of
		true when IsBlacklistedIP == false ->
		    change_shaper(StateData, jid:make(<<"">>, Server, <<"">>)),
		    case StreamVersion of
			<<"1.0">> ->
			    case StateData#state.authenticated of
				false ->
				    TLS = StateData#state.tls,
				    TLSEnabled = StateData#state.tls_enabled,
				    TLSRequired = StateData#state.tls_required,
				    SASLState = cyrsasl:server_new(
					    <<"jabber">>, Server, <<"">>, [],
					    fun (U) ->
						    ejabberd_auth:get_password_with_authmodule(
							U, Server)
					    end,
					  fun(U, AuthzId, P) ->
						    ejabberd_auth:check_password_with_authmodule(
						    U, AuthzId, Server, P)
					    end,
					  fun(U, AuthzId, P, D, DG) ->
						    ejabberd_auth:check_password_with_authmodule(
						    U, AuthzId, Server, P, D, DG)
					    end),
				    Mechs =
					case TLSEnabled or not TLSRequired of
					true ->
					    Ms = lists:map(fun (S) ->
							    #xmlel{name = <<"mechanism">>,
								attrs = [],
								children = [{xmlcdata, S}]}
						    end,
                            [<<"PLAIN">>]),
						   % cyrsasl:listmech(Server)),
					    [#xmlel{name = <<"mechanisms">>,
						    attrs = [{<<"xmlns">>, ?NS_SASL}],
						    children = Ms}];
					false ->
					    []
				    end,
				    SockMod =
					(StateData#state.sockmod):get_sockmod(StateData#state.socket),
				    Zlib = StateData#state.zlib,
				    CompressFeature = case Zlib andalso
					((SockMod == gen_tcp) orelse (SockMod == fast_tls)) of
					true ->
					    [#xmlel{name = <<"compression">>,
						    attrs = [{<<"xmlns">>, ?NS_FEATURE_COMPRESS}],
						    children = [#xmlel{name = <<"method">>,
							    attrs = [],
							    children = [{xmlcdata, <<"zlib">>}]}]}];
					_ ->
					    []
				    end,
				    TLSFeature =
					case (TLS == true) andalso
					(TLSEnabled == false) andalso
					(SockMod == gen_tcp) of
					true ->
					    case TLSRequired of
						true ->
						    [#xmlel{name = <<"starttls">>,
							    attrs = [{<<"xmlns">>, ?NS_TLS}],
							    children = [#xmlel{name = <<"required">>,
								    attrs = [],
								    children = []}]}];
						_ ->
						    [#xmlel{name = <<"starttls">>,
							    attrs = [{<<"xmlns">>, ?NS_TLS}],
							    children = []}]
					    end;
					false ->
					    []
				    end,
				    StreamFeatures1 = TLSFeature ++ CompressFeature ++ Mechs,
				    ejabberd_hooks:run_fold(c2s_stream_features,
					    Server, StreamFeatures1, [Server]),
					
 	                            User = fxml:get_attr_s(<<"user">>, Attrs),
            			    Sockmod = case TLS of
	                	    true ->
        	                	    <<"TLS">>;
                	            _ ->
	                                    <<"">>
        	                    end,
                    	            send_welcome_msg(StateData,User,Server,<<"1.0">>,Sockmod),

				    fsm_next_state(wait_for_feature_request,
					StateData#state{server = Server,
					    sasl_state = SASLState,
					    lang = Lang});
				_ ->
				    case StateData#state.resource of
					<<"">> ->
					    RosterVersioningFeature =
						ejabberd_hooks:run_fold(roster_get_versioning_feature,
						    Server, [],
						    [Server]),
					    StreamManagementFeature =
						case stream_mgmt_enabled(StateData) of
						true ->
						    [#xmlel{name = <<"sm">>,
							    attrs = [{<<"xmlns">>, ?NS_STREAM_MGMT_2}],
							    children = []},
							#xmlel{name = <<"sm">>,
							    attrs = [{<<"xmlns">>, ?NS_STREAM_MGMT_3}],
							    children = []}];
						false ->
						    []
					    end,
					    SockMod =
						(StateData#state.sockmod):get_sockmod(
						  StateData#state.socket),
					    Zlib = StateData#state.zlib,
					    CompressFeature =
						case Zlib andalso
						    ((SockMod == gen_tcp) orelse (SockMod == fast_tls)) of
						    true ->
							[#xmlel{name = <<"compression">>,
								attrs = [{<<"xmlns">>, ?NS_FEATURE_COMPRESS}],
								children = [#xmlel{name = <<"method">>,
										   attrs = [],
										   children = [{xmlcdata, <<"zlib">>}]}]}];
						    _ ->
							[]
						end,
					    StreamFeatures1 = [#xmlel{name = <<"bind">>,
							attrs = [{<<"xmlns">>, ?NS_BIND}],
							children = []},
						    #xmlel{name = <<"session">>,
							attrs = [{<<"xmlns">>, ?NS_SESSION}],
							children =
                                                           [#xmlel{name = <<"optional">>}]}]
						++
						RosterVersioningFeature ++
						StreamManagementFeature ++
						CompressFeature ++
						ejabberd_hooks:run_fold(c2s_post_auth_features,
						    Server, [], [Server]),
					    ejabberd_hooks:run_fold(c2s_stream_features,
						    Server, StreamFeatures1, [Server]),
					    fsm_next_state(wait_for_bind,
						StateData#state{server = Server, lang = Lang});
					_ ->
					    fsm_next_state(session_established,
						StateData#state{server = Server, lang = Lang})
				    end
			    end;
			_ ->
                send_stream_end(StateData, 222, <<"vresion is error">>),
		        {stop, normal, StateData}
		    end;
		true ->
		    IP = StateData#state.ip,
		    {true, LogReason, _ReasonT} = IsBlacklistedIP,
		    ?INFO_MSG("Connection attempt from blacklisted IP ~s: ~s",
			[jlib:ip_to_list(IP), LogReason]),
            send_stream_end(StateData, 222, <<"Connection attempt from blacklisted IP">>),
		    {stop, normal, StateData};
		_ ->
                    send_stream_end(StateData, 222, <<"host is not surport">>),
		    {stop, normal, StateData}
	    end;
	_ ->
            send_stream_end(StateData, 222, <<"not a stream">>),
	    {stop, normal, StateData}
    end;
wait_for_stream(timeout, StateData) ->
    send_stream_end(StateData, 102, <<"wait for stream timeout">>),
    {stop, normal, StateData};
wait_for_stream({xmlstreamelement, _}, StateData) ->
    send_stream_end(StateData, 222, <<"wait for stream for xmlstreamelement">>),
    {stop, normal, StateData};
wait_for_stream({xmlstreamend, _}, StateData) ->
    send_stream_end(StateData, 222, <<"wait for stream for streamend">>),
    {stop, normal, StateData};
wait_for_stream({xmlstreamerror, _}, StateData) ->
    send_stream_end(StateData, 222, <<"wait for stream for xmlstreamerror">>),
    {stop, normal, StateData};
wait_for_stream(closed, StateData) ->
    send_stream_end(StateData, 102, <<"wait for stream for closed">>),
    {stop, normal, StateData};
wait_for_stream(stop, StateData) ->
    send_stream_end(StateData, 102, <<"wait for stream for stop">>),
    {stop, normal, StateData}.

wait_for_auth({xmlstreamelement, #xmlel{name = Name} = El}, StateData)
	when ?IS_STREAM_MGMT_TAG(Name) ->
    fsm_next_state(wait_for_auth, dispatch_stream_mgmt(El, StateData));
wait_for_auth({xmlstreamelement, El}, StateData) ->
    case is_auth_packet(El) of
	{auth, _ID, get, {U, _, _, _}} ->
	    #xmlel{name = Name, attrs = Attrs} = jlib:make_result_iq_reply(El),
	    case U of
		<<"">> -> UCdata = [];
		_ -> UCdata = [{xmlcdata, U}]
	    end,
	    Res = case
		ejabberd_auth:plain_password_required(StateData#state.server)
	    of
		false ->
		    #xmlel{name = Name, attrs = Attrs,
			children =
			[#xmlel{name = <<"query">>,
				attrs = [{<<"xmlns">>, ?NS_AUTH}],
				children =
				[#xmlel{name = <<"username">>,
					attrs = [],
					children = UCdata},
				    #xmlel{name = <<"password">>,
					attrs = [], children = []},
				    #xmlel{name = <<"digest">>,
					attrs = [], children = []},
				    #xmlel{name = <<"resource">>,
					attrs = [],
					children = []}]}]};
		true ->
		    #xmlel{name = Name, attrs = Attrs,
			children =
			[#xmlel{name = <<"query">>,
				attrs = [{<<"xmlns">>, ?NS_AUTH}],
				children =
				[#xmlel{name = <<"username">>,
					attrs = [],
					children = UCdata},
				    #xmlel{name = <<"password">>,
					attrs = [], children = []},
				    #xmlel{name = <<"resource">>,
					attrs = [],
					children = []}]}]}
	    end,
	    send_element(StateData, Res),
	    fsm_next_state(wait_for_auth, StateData);
	{auth, _ID, set, {_U, _P, _D, <<"">>}} ->
	    Lang = StateData#state.lang,
	    Txt = <<"No resource provided">>,
	    Err = jlib:make_error_reply(El, ?ERRT_NOT_ACCEPTABLE(Lang, Txt)),
	    send_element(StateData, Err),
	    fsm_next_state(wait_for_auth, StateData);
	{auth, _ID, set, {U, P, D, R}} ->
	    JID = jid:make(U, StateData#state.server, R),
	    case JID /= error andalso
		acl:access_matches(StateData#state.access,
				   #{usr => jid:split(JID), ip => StateData#state.ip},
				   StateData#state.server) == allow
	    of
		true ->
		    DGen = fun (PW) ->
			    p1_sha:sha(<<(StateData#state.streamid)/binary, PW/binary>>)
		    end,
		case ejabberd_auth:check_password_with_authmodule(U, U,
			    StateData#state.server,
			    P, D, DGen)
		    of
			{true, AuthModule} ->
			    ?INFO_MSG("(~w) Accepted legacy authentication for ~s by ~p from ~s",
				[StateData#state.socket,
				    jid:to_string(JID), AuthModule,
				    ejabberd_config:may_hide_data(jlib:ip_to_list(StateData#state.ip))]),
			    ejabberd_hooks:run(c2s_auth_result, StateData#state.server,
				[true, U, StateData#state.server,
				    StateData#state.ip]),
			    Conn = get_conn_type(StateData),
			    Info = [{ip, StateData#state.ip}, {conn, Conn},
				    {auth_module, AuthModule}],
			    Res = jlib:make_result_iq_reply(
				    El#xmlel{children = []}),
			    send_element(StateData, Res),
			    ejabberd_sm:open_session(StateData#state.sid, U,
				StateData#state.server, R,
				Info),
			    change_shaper(StateData, JID),
			    {Fs, Ts} =
				ejabberd_hooks:run_fold(roster_get_subscription_lists,
				    StateData#state.server,
				    {[], []},
				    [U, StateData#state.server]),
			    LJID = jid:tolower(jid:remove_resource(JID)),
			    Fs1 = [LJID | Fs],
			    Ts1 = [LJID | Ts],
			    PrivList = ejabberd_hooks:run_fold(privacy_get_user_list,
				    StateData#state.server,
				    #userlist{},
				    [U, StateData#state.server]),
			    NewStateData = StateData#state{user = U,
				    resource = R,
				    jid = JID,
				    conn = Conn,
				    auth_module = AuthModule,
				    pres_f = (?SETS):from_list(Fs1),
				    pres_t = (?SETS):from_list(Ts1),
				    privacy_list = PrivList},
			    fsm_next_state(session_established, NewStateData);
			_ ->
			    ?INFO_MSG("(~w) Failed legacy authentication for ~s from ~s",
				[StateData#state.socket,
				    jid:to_string(JID),
				    ejabberd_config:may_hide_data(jlib:ip_to_list(StateData#state.ip))]),
			    ejabberd_hooks:run(c2s_auth_result, StateData#state.server,
				[false, U, StateData#state.server,
				    StateData#state.ip]),
			    Lang = StateData#state.lang,
			    Txt = <<"Legacy authentication failed">>,
			    Err = jlib:make_error_reply(
				    El, ?ERRT_NOT_AUTHORIZED(Lang, Txt)),
			    send_element(StateData, Err),
			    fsm_next_state(wait_for_auth, StateData)
		    end;
		_ ->
		    if JID == error ->
			    ?INFO_MSG("(~w) Forbidden legacy authentication "
				"for username '~s' with resource '~s'",
				[StateData#state.socket, U, R]),
			    Err = jlib:make_error_reply(El, ?ERR_JID_MALFORMED),
			    send_element(StateData, Err),
			    fsm_next_state(wait_for_auth, StateData);
			true ->
			    ?INFO_MSG("(~w) Forbidden legacy authentication "
				"for ~s from ~s",
				[StateData#state.socket,
				    jid:to_string(JID),
				    ejabberd_config:may_hide_data(jlib:ip_to_list(StateData#state.ip))]),
			    ejabberd_hooks:run(c2s_auth_result, StateData#state.server,
				[false, U, StateData#state.server,
				    StateData#state.ip]),
			    Lang = StateData#state.lang,
			    Txt = <<"Legacy authentication forbidden">>,
			    Err = jlib:make_error_reply(El, ?ERRT_NOT_ALLOWED(Lang, Txt)),
			    send_element(StateData, Err),
			    fsm_next_state(wait_for_auth, StateData)
		    end
	    end;
	_ ->
	    process_unauthenticated_stanza(StateData, El),
	    fsm_next_state(wait_for_auth, StateData)
    end;
wait_for_auth(timeout, StateData) ->
    send_stream_end(StateData, 102, <<"wait for auth for timeout">>),
    {stop, normal, StateData};
wait_for_auth({xmlstreamend, _Name}, StateData) ->
    send_stream_end(StateData, 222, <<"wait for auth for xmlstreamend">>),
    {stop, normal, StateData};
wait_for_auth({xmlstreamerror, _}, StateData) ->
    send_stream_end(StateData, 222, <<"wait for auth for xmlstreamerror">>),
%    send_element(StateData, ?INVALID_XML_ERR),
    {stop, normal, StateData};
wait_for_auth(closed, StateData) ->
    send_stream_end(StateData, 102, <<"wait for auth for closed">>),
    {stop, normal, StateData};
wait_for_auth(stop, StateData) ->
    send_stream_end(StateData, 102, <<"wait for auth for stop">>),
    {stop, normal, StateData}.

wait_for_feature_request({xmlstreamelement, #xmlel{name = Name} = El},
			 StateData)
    when ?IS_STREAM_MGMT_TAG(Name) ->
    fsm_next_state(wait_for_feature_request,
		   dispatch_stream_mgmt(El, StateData));
wait_for_feature_request({xmlstreamelement, El},
			 StateData) ->
    #xmlel{name = Name, attrs = Attrs, children = Els} = El,
    Zlib = StateData#state.zlib,
    TLS = StateData#state.tls,
    TLSEnabled = StateData#state.tls_enabled,
    TLSRequired = StateData#state.tls_required,
    SockMod =
	(StateData#state.sockmod):get_sockmod(StateData#state.socket),
    case {fxml:get_attr_s(<<"xmlns">>, Attrs), Name} of
      {?NS_SASL, <<"auth">>}
	  when TLSEnabled or not TLSRequired ->
	  Mech = fxml:get_attr_s(<<"mechanism">>, Attrs),
	  ClientIn = jlib:decode_base64(fxml:get_cdata(Els)),
	  case cyrsasl:server_start(StateData#state.sasl_state,
				    Mech, ClientIn)
	      of
	    {ok, Props} ->
		(StateData#state.sockmod):reset_stream(StateData#state.socket),
		U = identity(Props),
		AuthModule = proplists:get_value(auth_module, Props, undefined),
		?INFO_MSG("(~w) Accepted authentication for ~s "
			  "by ~p from ~s",
			  [StateData#state.socket, U, AuthModule,
			   ejabberd_config:may_hide_data(jlib:ip_to_list(StateData#state.ip))]),
		ejabberd_hooks:run(c2s_auth_result, StateData#state.server,
				   [true, U, StateData#state.server,
				    StateData#state.ip]),
		
	        case catch fxml:get_attr_s(<<"id">>, Attrs) of
        	<<"">> ->
		        send_auth_login_response_sucess(StateData,U,StateData#state.server,<<"0">>,<<"">>);
		ID ->
        	    send_auth_login_response_sucess(StateData,U,StateData#state.server,ID,<<"">>)
	        end,

		fsm_next_state(wait_for_bind,		
				StateData#state{streamid = new_id(),
						authenticated = true,
						auth_module = AuthModule,
                                                sasl_state = undefined,
						user = U});
	    {continue, ServerOut, NewSASLState} ->
		send_element(StateData,
			     #xmlel{name = <<"challenge">>,
				    attrs = [{<<"xmlns">>, ?NS_SASL}],
				    children =
					[{xmlcdata,
					  jlib:encode_base64(ServerOut)}]}),
		fsm_next_state(wait_for_sasl_response,
			       StateData#state{sasl_state = NewSASLState});
	    {error, Error, Username} ->
		?INFO_MSG("(~w) Failed authentication for ~s@~s from ~s",
			[StateData#state.socket,
			    Username, StateData#state.server,
			    ejabberd_config:may_hide_data(jlib:ip_to_list(StateData#state.ip))]),
		ejabberd_hooks:run(c2s_auth_result, StateData#state.server,
				   [false, Username, StateData#state.server,
				    StateData#state.ip]),
         case qtalk_public:kick_error_login_user(Error,self()) of
         true ->
                ok;
         _ ->
	        case catch fxml:get_attr_s(<<"id">>, Attrs) of
	        <<"">> ->
        	        send_auth_login_response_failed(StateData,StateData#state.user,StateData#state.server,<<"0">>,Error);
	        ID ->
             	   send_auth_login_response_failed(StateData,StateData#state.user,StateData#state.server,ID,Error)
	        end
         end,

		fsm_next_state(wait_for_feature_request, StateData);
	    {error, Error} ->
						
	        case catch fxml:get_attr_s(<<"id">>, Attrs) of
	        <<"">> ->
        	        send_auth_login_response_failed(StateData,StateData#state.user,StateData#state.server,<<"0">>,Error);
	        ID ->
             	   send_auth_login_response_failed(StateData,StateData#state.user,StateData#state.server,ID,Error)
	        end,
        qtalk_public:kick_error_login_user(Error,self()),
		fsm_next_state(wait_for_feature_request, StateData)
	  end;
      {?NS_TLS, <<"starttls">>}
	  when TLS == true, TLSEnabled == false,
	       SockMod == gen_tcp ->
	  TLSOpts = case
		      ejabberd_config:get_option(
                        {domain_certfile, StateData#state.server},
                        fun iolist_to_binary/1)
			of
		      undefined -> StateData#state.tls_options;
		      CertFile ->
			  [{certfile, CertFile} | lists:keydelete(certfile, 1,
								  StateData#state.tls_options)]
		    end,
	  Socket = StateData#state.socket,
	  BProceed = send_startTLS(StateData,StateData#state.user,StateData#state.server),
	  TLSSocket = (StateData#state.sockmod):starttls('probuff',Socket,
                             TLSOpts,
                             BProceed),
	  ?DEBUG("TLSSocket ~p ~n",[TLSSocket]),	

	  fsm_next_state(wait_for_feature_request,
			 StateData#state{socket = TLSSocket,
					 streamid = new_id(),
					 tls_enabled = true});
      {?NS_COMPRESS, <<"compress">>}
	  when Zlib == true,
	       (SockMod == gen_tcp) or (SockMod == fast_tls) ->
	  process_compression_request(El, wait_for_feature_request, StateData);
      _ ->
	  if TLSRequired and not TLSEnabled ->
                send_stream_end(StateData, 222, <<"wait for feature for tls error">>),
		{stop, normal, StateData};
	     true ->
		 process_unauthenticated_stanza(StateData, El),
		 fsm_next_state(wait_for_feature_request, StateData)
	  end
    end;
wait_for_feature_request(timeout, StateData) ->
    send_stream_end(StateData, 102, <<"wait for feature for timeout">>),
    {stop, normal, StateData};
wait_for_feature_request({xmlstreamend, _Name},
			 StateData) ->
    send_stream_end(StateData, 222, <<"wait for feature for xmlstreamend">>),
    {stop, normal, StateData};
wait_for_feature_request({xmlstreamerror, _},
			 StateData) ->
%    send_element(StateData, ?INVALID_XML_ERR),
%     send_stream_end(StateData),     
     send_stream_end(StateData, 222, <<"wait for feature for xmlstreamerror">>),
    {stop, normal, StateData};
wait_for_feature_request(closed, StateData) ->
    send_stream_end(StateData, 102, <<"wait for feature for closed">>),
    {stop, normal, StateData};
wait_for_feature_request(stop, StateData) ->
    send_stream_end(StateData, 102, <<"wait for feature for stop">>),
    {stop, normal, StateData}.

wait_for_sasl_response({xmlstreamelement, #xmlel{name = Name} = El}, StateData)
    when ?IS_STREAM_MGMT_TAG(Name) ->
    fsm_next_state(wait_for_sasl_response, dispatch_stream_mgmt(El, StateData));
wait_for_sasl_response({xmlstreamelement, El},
		       StateData) ->
    #xmlel{name = Name, attrs = Attrs, children = Els} = El,
    case {fxml:get_attr_s(<<"xmlns">>, Attrs), Name} of
      {?NS_SASL, <<"response">>} ->
	  ClientIn = jlib:decode_base64(fxml:get_cdata(Els)),
	  case cyrsasl:server_step(StateData#state.sasl_state,
				   ClientIn)
	      of
	    {ok, Props} ->
		catch
		  (StateData#state.sockmod):reset_stream(StateData#state.socket),
		U = identity(Props),
		AuthModule = proplists:get_value(auth_module, Props, <<>>),
		?INFO_MSG("(~w) Accepted authentication for ~s "
			  "by ~p from ~s",
			  [StateData#state.socket, U, AuthModule,
			   ejabberd_config:may_hide_data(jlib:ip_to_list(StateData#state.ip))]),
		ejabberd_hooks:run(c2s_auth_result, StateData#state.server,
				   [true, U, StateData#state.server,
				    StateData#state.ip]),
		send_element(StateData,
			    #xmlel{name = <<"success">>,
				    attrs = [{<<"xmlns">>, ?NS_SASL}],
				    children = []}),
		fsm_next_state(wait_for_stream,
				StateData#state{streamid = new_id(),
						authenticated = true,
						auth_module = AuthModule,
                                                sasl_state = undefined,
						user = U});
	    {ok, Props, ServerOut} ->
		(StateData#state.sockmod):reset_stream(StateData#state.socket),
		U = identity(Props),
		AuthModule = proplists:get_value(auth_module, Props, undefined),
		?INFO_MSG("(~w) Accepted authentication for ~s "
			  "by ~p from ~s",
			  [StateData#state.socket, U, AuthModule,
			   ejabberd_config:may_hide_data(jlib:ip_to_list(StateData#state.ip))]),
		ejabberd_hooks:run(c2s_auth_result, StateData#state.server,
				   [true, U, StateData#state.server,
				    StateData#state.ip]),
		send_element(StateData,
			    #xmlel{name = <<"success">>,
				    attrs = [{<<"xmlns">>, ?NS_SASL}],
				    children =
					[{xmlcdata,
					jlib:encode_base64(ServerOut)}]}),
		fsm_next_state(wait_for_stream,
				StateData#state{streamid = new_id(),
						authenticated = true,
						auth_module = AuthModule,
                                                sasl_state = undefined,
						user = U});
	    {continue, ServerOut, NewSASLState} ->
		send_element(StateData,
			     #xmlel{name = <<"challenge">>,
				    attrs = [{<<"xmlns">>, ?NS_SASL}],
				    children =
					[{xmlcdata,
					  jlib:encode_base64(ServerOut)}]}),
		fsm_next_state(wait_for_sasl_response,
			       StateData#state{sasl_state = NewSASLState});
	    {error, Error, Username} ->
		?INFO_MSG("(~w) Failed authentication for ~s@~s from ~s",
                          [StateData#state.socket,
                           Username, StateData#state.server,
                           ejabberd_config:may_hide_data(jlib:ip_to_list(StateData#state.ip))]),
		ejabberd_hooks:run(c2s_auth_result, StateData#state.server,
				   [false, Username, StateData#state.server,
				    StateData#state.ip]),
		send_element(StateData,
			     #xmlel{name = <<"failure">>,
				    attrs = [{<<"xmlns">>, ?NS_SASL}],
				    children =
					[#xmlel{name = Error, attrs = [],
						children = []}]}),
		fsm_next_state(wait_for_feature_request, StateData);
	    {error, Error} ->
		send_element(StateData,
			     #xmlel{name = <<"failure">>,
				    attrs = [{<<"xmlns">>, ?NS_SASL}],
				    children =
					[#xmlel{name = Error, attrs = [],
						children = []}]}),
		fsm_next_state(wait_for_feature_request, StateData)
	  end;
      _ ->
	  process_unauthenticated_stanza(StateData, El),
	  fsm_next_state(wait_for_feature_request, StateData)
    end;
wait_for_sasl_response(timeout, StateData) ->
    send_stream_end(StateData, 102, <<"wait for sasl for timeout">>),
    {stop, normal, StateData};
wait_for_sasl_response({xmlstreamend, _Name},
		       StateData) ->
    send_stream_end(StateData, 222, <<"wait for sasl for xmlstreamend">>),
    {stop, normal, StateData};
wait_for_sasl_response({xmlstreamerror, _},
		       StateData) ->
%    send_element(StateData, ?INVALID_XML_ERR),
    send_stream_end(StateData, 222, <<"wait for sasl for xmlstreamerror">>),
    {stop, normal, StateData};
wait_for_sasl_response(closed, StateData) ->
    send_stream_end(StateData, 102, <<"wait for sasl for closed">>),
    {stop, normal, StateData};
wait_for_sasl_response(stop, StateData) ->
    send_stream_end(StateData, 102, <<"wait for sasl for stop">>),
    {stop, normal, StateData}.

resource_conflict_action(U, S, R) ->
    OptionRaw = case ejabberd_sm:is_existing_resource(U, S, R) of
		  true ->
		      ejabberd_config:get_option(
                        {resource_conflict, S},
                        fun(setresource) -> setresource;
                           (closeold) -> closeold;
                           (closenew) -> closenew;
                           (acceptnew) -> acceptnew
                        end);
                  false ->
                      acceptnew
		end,
    Option = case OptionRaw of
	       setresource -> setresource;
	       closeold ->
		   acceptnew; %% ejabberd_sm will close old session
	       closenew -> closenew;
	       acceptnew -> acceptnew;
	       _ -> acceptnew %% default ejabberd behavior
	     end,
    case Option of
      acceptnew -> {accept_resource, R};
      closenew -> closenew;
      setresource ->
	  Rnew = new_uniq_id(),
	  {accept_resource, Rnew}
    end.

wait_for_bind({xmlstreamelement, #xmlel{name = Name, attrs = Attrs} = El},
	      StateData)
    when ?IS_STREAM_MGMT_TAG(Name) ->
    case Name of
      <<"resume">> ->
	  case handle_resume(StateData, Attrs) of
	    {ok, ResumedState} ->
		fsm_next_state(session_established, ResumedState);
	    error ->
		fsm_next_state(wait_for_bind, StateData)
	  end;
      _ ->
	  fsm_next_state(wait_for_bind, dispatch_stream_mgmt(El, StateData))
    end;
wait_for_bind({xmlstreamelement, El}, StateData) ->
	?DEBUG("El ~p ~n",[El]),
    case jlib:iq_query_info(El) of
      #iq{type = set, lang = Lang, xmlns = ?NS_BIND, sub_el = SubEl} =
	  IQ ->
	  U = StateData#state.user,
	  R1 = fxml:get_path_s(SubEl,
			      [{elem, <<"resource">>}, cdata]),
	  R = case jid:resourceprep(R1) of
		error -> error;
		<<"">> -> new_uniq_id();
		Resource -> Resource
	      end,
	  case R of
	    error ->
		Txt = <<"Malformed resource">>,
		Err = jlib:make_error_reply(El, ?ERRT_BAD_REQUEST(Lang, Txt)),
		send_element(StateData, Err),
		fsm_next_state(wait_for_bind, StateData);
	    _ ->
		?DEBUG("Rs ~p ~n",[R]),
		case resource_conflict_action(U, StateData#state.server,
					      R)
		    of
		  closenew ->
		      Err = jlib:make_error_reply(El,
						  ?STANZA_ERROR(<<"409">>,
								<<"modify">>,
								<<"conflict">>)),
		      send_element(StateData, Err),
		      fsm_next_state(wait_for_bind, StateData);
		  {accept_resource, R2} ->
                        JID = jid:make(U, StateData#state.server, R2),
                        StateData2 =
                            StateData#state{resource = R2, jid = JID},
                        case open_session(StateData2) of
                            {ok, StateData3} ->
                                Res =
                                    IQ#iq{
                                      type = result,
                                      sub_el =
                                      [#xmlel{name = <<"bind">>,
                                              attrs = [{<<"xmlns">>, ?NS_BIND}],
                                              children =
                                              [#xmlel{name = <<"jid">>,
                                                      attrs = [],
                                                      children =
                                                      [{xmlcdata,
                                                        jid:to_string(JID)}]}]}]},

                		Key = iolist_to_binary([integer_to_binary(random:uniform(65536)) | [jlib:integer_to_binary(X)|| X <- tuple_to_list(os:timestamp())]]),
		                qtalk_c2s:set_redis_user_key(StateData#state.server,U,R2,Key,R2,86400*3),
                                NavPresence = qtalk_public:send_navversion(StateData3#state.user,StateData3#state.server,R2,<<"1003">>),
			
                                NewState = StateData3#state{key = Key},

				try
				    send_element(NewState, jlib:iq_to_xml(Res)),
				    send_element(NewState, NavPresence)
				catch exit:normal ->
					close(self())
				end,
		
          	      		fsm_next_state_pack(session_established,NewState);
		          {error, Error} ->
        		        Err = jlib:make_error_reply(El, Error),
                		send_element(StateData, Err),
                		fsm_next_state(session_established, StateData)
	       	      end
		end
	  end;
	_ ->
	    #xmlel{name = Name, attrs = Attrs, children = _Els} = El,
	    Zlib = StateData#state.zlib,
	    SockMod =
		(StateData#state.sockmod):get_sockmod(StateData#state.socket),
	    case {fxml:get_attr_s(<<"xmlns">>, Attrs), Name} of
		{?NS_COMPRESS, <<"compress">>}
		when Zlib == true,
		     (SockMod == gen_tcp) or (SockMod == fast_tls) ->
		    process_compression_request(El, wait_for_bind, StateData);
		_ ->
		    fsm_next_state(wait_for_bind, StateData)
	    end
    end;
wait_for_bind(timeout, StateData) ->
    send_stream_end(StateData, 102, <<"wait for bind for timeout">>),
    {stop, normal, StateData};
wait_for_bind({xmlstreamend, _Name}, StateData) ->
    send_stream_end(StateData, 222, <<"wait for bind for xmlstreamend">>),
    {stop, normal, StateData};
wait_for_bind({xmlstreamerror, _}, StateData) ->
    send_stream_end(StateData, 222, <<"wait for bind for xmlstreamerror">>),
%    send_element(StateData, ?INVALID_XML_ERR),
%    send_stream_end(StateData),    
    {stop, normal, StateData};
wait_for_bind(closed, StateData) ->
    send_stream_end(StateData, 102, <<"wait for bind for closed">>),
    {stop, normal, StateData};
wait_for_bind(stop, StateData) ->
    send_stream_end(StateData, 102, <<"wait for bind for stop">>),
    {stop, normal, StateData}.

open_session(StateData) ->
    U = StateData#state.user,
    R = StateData#state.resource,
    JID = StateData#state.jid,
    Lang = StateData#state.lang,
    IP = StateData#state.ip,
    case acl:access_matches(StateData#state.access,
			    #{usr => jid:split(JID), ip => IP},
			    StateData#state.server) of
        allow ->
            ?INFO_MSG("(~w) Opened session for ~s",
                      [StateData#state.socket, jid:to_string(JID)]),
            change_shaper(StateData, JID),
            {Fs, Ts} = ejabberd_hooks:run_fold(
                         roster_get_subscription_lists,
                         StateData#state.server,
                         {[], []},
                         [U, StateData#state.server]),
            LJID = jid:tolower(jid:remove_resource(JID)),
            Fs1 = [LJID | Fs],
            Ts1 = [LJID | Ts],
            PrivList =
                ejabberd_hooks:run_fold(
                  privacy_get_user_list,
                  StateData#state.server,
                  #userlist{},
                  [U, StateData#state.server]),
            Conn = get_conn_type(StateData),
            Info = [{ip, StateData#state.ip}, {conn, Conn},
                    {auth_module, StateData#state.auth_module}],
            ejabberd_sm:open_session(
              StateData#state.sid, U, StateData#state.server, R, Info),
            UpdatedStateData =
                StateData#state{
                  conn = Conn,
                  pres_f = ?SETS:from_list(Fs1),
                  pres_t = ?SETS:from_list(Ts1),
                  privacy_list = PrivList},
            {ok, UpdatedStateData};
        _ ->
            ejabberd_hooks:run(forbidden_session_hook,
                               StateData#state.server, [JID]),
            ?INFO_MSG("(~w) Forbidden session for ~s",
                      [StateData#state.socket, jid:to_string(JID)]),
	    Txt = <<"Denied by ACL">>,
            {error, ?ERRT_NOT_ALLOWED(Lang, Txt)}
    end.

session_established({xmlstreamelement, #xmlel{name = Name} = El}, StateData)
    when ?IS_STREAM_MGMT_TAG(Name) ->
    fsm_next_state(session_established, dispatch_stream_mgmt(El, StateData));
session_established({xmlstreamelement,
		     #xmlel{name = <<"active">>,
			    attrs = [{<<"xmlns">>, ?NS_CLIENT_STATE}]}},
		    StateData) ->
    NewStateData = csi_flush_queue(StateData),
    fsm_next_state(session_established, NewStateData#state{csi_state = active});
session_established({xmlstreamelement,
		     #xmlel{name = <<"inactive">>,
			    attrs = [{<<"xmlns">>, ?NS_CLIENT_STATE}]}},
		    StateData) ->
    fsm_next_state(session_established, StateData#state{csi_state = inactive});
session_established({xmlstreamelement, El},StateData) when StateData#state.key =:= <<"">> ->
    FromJID = StateData#state.jid,
    case check_from(El, FromJID) of
	'invalid-from' ->
            ?ERROR_MSG("the invalid El is ~p~n", [{El, FromJID}]),
            send_stream_end(StateData, 222, <<"established for invalid from">>),
	    {stop, normal, StateData};
	_NewEl ->
	    session_established2(El, StateData)
   end;
session_established({xmlstreamelement, El},
		    StateData) ->
    FromJID = StateData#state.jid,
    case check_from(El, FromJID) of
	'invalid-from' ->
            ?ERROR_MSG("the invalid El is ~p~n", [{El, FromJID}]),
            send_stream_end(StateData, 222, <<"established for invalid from">>),
	    {stop, normal, StateData};
	_NewEl ->
	    session_established2(El, StateData)
    end;
%% We hibernate the process to reduce memory consumption after a
%% configurable activity timeout
session_established(timeout, StateData) ->
    Options = [],
    proc_lib:hibernate(?GEN_FSM, enter_loop,
		       [?MODULE, Options, session_established, StateData]),
    fsm_next_state(session_established, StateData);
session_established({xmlstreamend, _Name}, StateData) ->
    send_stream_end(StateData, 222, <<"established for xmlstreamend">>),
    {stop, normal, StateData};
session_established({xmlstreamerror,
		     <<"XML stanza is too big">>},
		    StateData) ->
    send_stream_end(StateData, 222, <<"established for xml stanza too big">>),
    {stop, normal, StateData};
session_established({xmlstreamerror, _}, StateData) ->
    send_stream_end(StateData, 222, <<"established for xmlstreamerror">>),
    {stop, normal, StateData};
session_established(closed, #state{mgmt_state = active} = StateData) ->
    catch (StateData#state.sockmod):close(StateData#state.socket),
    fsm_next_state(wait_for_resume, StateData);
session_established(closed, StateData) ->
    send_stream_end(StateData, 102, <<"established for closed">>),
    {stop, normal, StateData};
session_established(stop, StateData) ->
    send_stream_end(StateData, 102, <<"established for stop">>),
    {stop, normal, StateData}.

%% Process packets sent by user (coming from user on c2s XMPP connection)
session_established2(El, StateData) ->
    #xmlel{name = Name, attrs = Attrs} = El,
    NewStateData = update_num_stanzas_in(StateData, El),
    User = NewStateData#state.user,
    Server = NewStateData#state.server,
    FromJID = NewStateData#state.jid,
    To = fxml:get_attr_s(<<"to">>, Attrs),
    ToJID = case To of
	      <<"">> -> jid:make(User, Server, <<"">>);
	      _ -> jid:from_string(To)
	    end,
    NewEl1 = jlib:remove_attr(<<"xmlns">>, El),
    NewEl = case fxml:get_attr_s(<<"xml:lang">>, Attrs) of
	      <<"">> ->
		  case NewStateData#state.lang of
		    <<"">> -> NewEl1;
		    Lang ->
			fxml:replace_tag_attr(<<"xml:lang">>, Lang, NewEl1)
		  end;
	      _ -> NewEl1
	    end,
    NewState = case ToJID of
		 error ->
		     case fxml:get_attr_s(<<"type">>, Attrs) of
		       <<"error">> -> NewStateData;
		       <<"result">> -> NewStateData;
		       _ ->
			   Err = jlib:make_error_reply(NewEl,
						       ?ERR_JID_MALFORMED),
			   send_packet(NewStateData, Err)
		     end;
		 _ ->
		     case Name of
		       <<"presence">> ->
            	           catch mod_static:add_record(<<"receive_all_presence">>, 1),
                           PType = fxml:get_attr_s(<<"type">>, Attrs),
			   PresenceEl0 =
                    qtalk_c2s:make_new_PresenceEl(Server,User, NewEl, Attrs),
			   PresenceEl =
				 ejabberd_hooks:run_fold(
				   user_send_packet, Server, PresenceEl0,
				   [NewStateData, FromJID, ToJID]),
			   case {ToJID, PType} of
			     {#jid{user = User, server = Server,
				  resource = <<"">>}, T} when T =/= <<"notify">> ->
				 ?DEBUG("presence_update(~p,~n\t~p,~n\t~p)",
					[FromJID, PresenceEl, NewStateData]),
				 presence_update(FromJID, PresenceEl,
						 NewStateData);
			     _ ->
				 presence_track(FromJID, ToJID, PresenceEl,
						NewStateData)
			   end;
		       <<"iq">> ->
            	           catch mod_static:add_record(<<"receive_all_iq">>, 1),
			   case jlib:iq_query_info(NewEl) of
			     #iq{xmlns = Xmlns} = IQ
				 when Xmlns == (?NS_PRIVACY);
				      Xmlns == (?NS_BLOCKING) ->
				 process_privacy_iq(FromJID, ToJID, IQ,
						    NewStateData);
                               #iq{xmlns = ?NS_SESSION} ->
                                   Res = jlib:make_result_iq_reply(
                                           NewEl#xmlel{children = []}),
                                   send_stanza(NewStateData, Res);
                 	%		send_time_key_presence(StateData#state.server,StateData#state.user,StateData#state.resource,StateData);
			     _ ->
				 NewEl0 = ejabberd_hooks:run_fold(
					    user_send_packet, Server, NewEl,
					    [NewStateData, FromJID, ToJID]),
				 check_privacy_route(FromJID, NewStateData,
						     FromJID, ToJID, NewEl0)
			   end;
		       <<"message">> ->
            		catch mod_static:add_record(<<"receive_all_message">>, 1),
                        Packet = qtalk_public:send_recv_repley(NewEl,FromJID,ejabberd_protobuf_c2s, NewStateData),
			   NewEl0 = ejabberd_hooks:run_fold(
				      user_send_packet, Server, NewEl,
				      [NewStateData, FromJID, ToJID]),
%                Packet = NewEl0,

               case privacy_check_packet(NewStateData, FromJID, ToJID, NewEl0,  out) of
               deny ->
                    ErrText = <<"Your active privacy list has denied "
                                           "the routing of this stanza.">>,
                    Err = jlib:make_error_reply(Packet,
                                            ?ERRT_NOT_ACCEPTABLE(NewStateData#state.lang, ErrText)),
                    Err2 = jlib:replace_from_to(ToJID, FromJID, Err),
                    send_stanza(StateData, Err2);
               allow ->
                    qtalk_c2s:carbon_message(FromJID,ToJID,Packet),
                    ejabberd_router:route(FromJID, ToJID, Packet),
                    catch mod_message_statistics:statistic_message_first(FromJID, ToJID, Packet),
                    StateData
               end;
		       _ -> NewStateData
		     end
	       end,
    ejabberd_hooks:run(c2s_loop_debug,
		       [{xmlstreamelement, El}]),
    fsm_next_state(session_established, NewState).

wait_for_resume({xmlstreamelement, _El} = Event, StateData) ->
    Result = session_established(Event, StateData),
    fsm_next_state(wait_for_resume, element(3, Result));
wait_for_resume(timeout, StateData) ->
    ?DEBUG("Timed out waiting for resumption of stream for ~s",
	   [jid:to_string(StateData#state.jid)]),
    send_stream_end(StateData, 102, <<"wait for resume for timeout">>),
    {stop, normal, StateData#state{mgmt_state = timeout}};
wait_for_resume(Event, StateData) ->
    ?DEBUG("Ignoring event while waiting for resumption: ~p", [Event]),
    send_stream_end(StateData, 102, <<"handle sync envent for resume session">>),
    fsm_next_state(wait_for_resume, StateData).

handle_event(_Event, StateName, StateData) ->
    fsm_next_state(StateName, StateData).

handle_sync_event({get_presence}, _From, StateName,
		  StateData) ->
    User = StateData#state.user,
    PresLast = StateData#state.pres_last,
    Show = get_showtag(PresLast),
    Status = get_statustag(PresLast),
    Resource = StateData#state.resource,
    Reply = {User, Resource, Show, Status},
    fsm_reply(Reply, StateName, StateData);
handle_sync_event({get_last_presence}, _From, StateName,
		  StateData) ->
    User = StateData#state.user,
    Server = StateData#state.server,
    PresLast = StateData#state.pres_last,
    Resource = StateData#state.resource,
    Reply = {User, Server, Resource, PresLast},
    fsm_reply(Reply, StateName, StateData);

handle_sync_event(get_subscribed, _From, StateName,
		  StateData) ->
    Subscribed = (?SETS):to_list(StateData#state.pres_f),
    {reply, Subscribed, StateName, StateData};
handle_sync_event({resume_session, Time}, _From, _StateName,
		  StateData) when element(1, StateData#state.sid) == Time ->
    %% The old session should be closed before the new one is opened, so we do
    %% this here instead of leaving it to the terminate callback
    ejabberd_sm:close_session(StateData#state.sid,
			      StateData#state.user,
			      StateData#state.server,
			      StateData#state.resource),
    {stop, normal, {resume, StateData}, StateData#state{mgmt_state = resumed}};
handle_sync_event({resume_session, _Time}, _From, StateName,
		  StateData) ->
    {reply, {error, <<"Previous session not found">>}, StateName, StateData};
handle_sync_event(_Event, _From, StateName,
		  StateData) ->
    Reply = ok, fsm_reply(Reply, StateName, StateData).

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

handle_info({send_text, Text}, StateName, StateData) ->
    send_text(StateData, Text),
    ejabberd_hooks:run(c2s_loop_debug, [Text]),
    fsm_next_state(StateName, StateData);
handle_info(replaced, StateName, StateData) ->
    handle_info({kick, 222, <<"replaced">>}, StateName, StateData);
handle_info(kick, StateName, StateData) ->
    handle_info({kick, 201, <<"kicked by admin">>}, StateName, StateData);
handle_info({kick, Code, Reason}, _StateName, StateData) ->
    send_stream_end(StateData, Code, Reason),
    {stop, normal, StateData#state{authenticated = Code}};
handle_info({route, _From, _To, {broadcast, Data}},
            StateName, StateData) ->
    ?DEBUG("broadcast~n~p~n", [Data]),
    case Data of
        {item, IJID, ISubscription} ->
            fsm_next_state(StateName,
                           roster_change(IJID, ISubscription, StateData));
        {exit, _Reason} ->
            send_stream_end(StateData, 102, <<"serrt conflict">>),
            {stop, normal, StateData};
        {privacy_list, PrivList, PrivListName} ->
            case ejabberd_hooks:run_fold(privacy_updated_list,
                                         StateData#state.server,
                                         false,
                                         [StateData#state.privacy_list,
                                          PrivList]) of
                false ->
                    fsm_next_state(StateName, StateData);
                NewPL ->
                    PrivPushIQ = #iq{type = set,
                                     id = <<"push",
                                            (randoms:get_string())/binary>>,
                                     sub_el =
                                         [#xmlel{name = <<"query">>,
                                                 attrs = [{<<"xmlns">>,
                                                           ?NS_PRIVACY}],
                                                 children =
                                                     [#xmlel{name = <<"list">>,
                                                             attrs = [{<<"name">>,
                                                                       PrivListName}],
                                                             children = []}]}]},
                    PrivPushEl = jlib:replace_from_to(
                                   jid:remove_resource(StateData#state.jid),
                                   StateData#state.jid,
                                   jlib:iq_to_xml(PrivPushIQ)),
                    NewState = send_stanza(
                                 StateData, PrivPushEl),
                    fsm_next_state(StateName,
                                   NewState#state{privacy_list = NewPL})
            end;
        {blocking, What} ->
            NewState = route_blocking(What, StateData),
            fsm_next_state(StateName, NewState);
        _ ->
            fsm_next_state(StateName, StateData)
    end;
%% Process Packets that are to be send to the user
handle_info({route, From, To,
             #xmlel{name = Name, attrs = Attrs, children = Els} = Packet},
            StateName, StateData) ->
    ?DEBUG("C2s Route ~p ~n",[Els]),
    {Pass, NewAttrs, NewState} = case Name of
				   <<"presence">> ->
				       State =
					   ejabberd_hooks:run_fold(c2s_presence_in,
								   StateData#state.server,
								   StateData,
								   [{From, To,
								     Packet}]),
				       case fxml:get_attr_s(<<"type">>, Attrs) of
					 <<"probe">> ->
					     LFrom = jid:tolower(From),
					     LBFrom =
						 jid:remove_resource(LFrom),
					     NewStateData = case
							      (?SETS):is_element(LFrom,
										 State#state.pres_a)
								orelse
								(?SETS):is_element(LBFrom,
										   State#state.pres_a)
								of
							      true -> State;
							      false ->
								  case
								    (?SETS):is_element(LFrom,
										       State#state.pres_f)
								      of
								    true ->
									A =
									    (?SETS):add_element(LFrom,
												State#state.pres_a),
									State#state{pres_a
											=
											A};
								    false ->
									case
									  (?SETS):is_element(LBFrom,
											     State#state.pres_f)
									    of
									  true ->
									      A =
										  (?SETS):add_element(LBFrom,
												      State#state.pres_a),
									      State#state{pres_a
											      =
											      A};
									  false ->
									      State
									end
								  end
							    end,
					     process_presence_probe(From, To,
								    NewStateData),
					     {false, Attrs, NewStateData};
					 <<"error">> ->
					     NewA =
						 remove_element(jid:tolower(From),
								State#state.pres_a),
					     {true, Attrs,
					      State#state{pres_a = NewA}};
					 <<"subscribe">> ->
					     SRes = is_privacy_allow(State,
								     From, To,
								     Packet,
								     in),
					     {SRes, Attrs, State};
					 <<"subscribed">> ->
					     SRes = is_privacy_allow(State,
								     From, To,
								     Packet,
								     in),
					     {SRes, Attrs, State};
					 <<"unsubscribe">> ->
					     SRes = is_privacy_allow(State,
								     From, To,
								     Packet,
								     in),
					     {SRes, Attrs, State};
					 <<"unsubscribed">> ->
					     SRes = is_privacy_allow(State,
								     From, To,
								     Packet,
								     in),
					     {SRes, Attrs, State};
					 <<"notify">> ->
					     SRes = is_privacy_allow(State,
								     From, To,
								     Packet,
								     in),
					     {SRes, Attrs, State};
					 _ ->
                            		?DEBUG("type ~p ~n",[Packet]),
					     case privacy_check_packet(State,
								       From, To,
								       Packet,
								       in)
						 of
					       allow ->
						   LFrom =
						       jid:tolower(From),
						   LBFrom =
						       jid:remove_resource(LFrom),
						   case
						     (?SETS):is_element(LFrom,
									State#state.pres_a)
						       orelse
						       (?SETS):is_element(LBFrom,
									  State#state.pres_a)
						       of
						     true ->
							 {true, Attrs, State};
						     false ->
							 case
							   (?SETS):is_element(LFrom,
									      State#state.pres_f)
							     of
							   true ->
							       A =
								   (?SETS):add_element(LFrom,
										       State#state.pres_a),
							       {true, Attrs,
								State#state{pres_a
										=
										A}};
							   false ->
							       case
								 (?SETS):is_element(LBFrom,
										    State#state.pres_f)
								   of
								 true ->
								     A =
									 (?SETS):add_element(LBFrom,
											     State#state.pres_a),
								     {true,
								      Attrs,
								      State#state{pres_a
										      =
										      A}};
								 false ->
								     {true,
								      Attrs,
								      State}
							       end
							 end
						   end;
					       deny ->
							?DEBUG("deny Packet ~p ~n",[Packet]),
							 {false, Attrs, State}
					     end
				       end;
				   <<"iq">> ->
				       IQ = jlib:iq_query_info(Packet),
				       case IQ of
					 #iq{xmlns = ?NS_LAST} ->
					     LFrom = jid:tolower(From),
					     LBFrom =
						 jid:remove_resource(LFrom),
					     HasFromSub =
						 ((?SETS):is_element(LFrom,
								     StateData#state.pres_f)
						    orelse
						    (?SETS):is_element(LBFrom,
								       StateData#state.pres_f))
						   andalso
						   is_privacy_allow(StateData,
								    To, From,
								    #xmlel{name
									       =
									       <<"presence">>,
									   attrs
									       =
									       [],
									   children
									       =
									       []},
								    out),
					     case HasFromSub of
					       true ->
						   case
						     privacy_check_packet(StateData,
									  From,
									  To,
									  Packet,
									  in)
						       of
						     allow ->
							 {true, Attrs,
							  StateData};
						     deny ->
							 Err =
							     jlib:make_error_reply(Packet,
										   ?ERR_SERVICE_UNAVAILABLE),
							 ejabberd_router:route(To,
									       From,
									       Err),
							 {false, Attrs,
							  StateData}
						   end;
					       _ ->
						   Err =
						       jlib:make_error_reply(Packet,
									     ?ERR_FORBIDDEN),
						   ejabberd_router:route(To,
									 From,
									 Err),
						   {false, Attrs, StateData}
					     end;
					 IQ
					     when is_record(IQ, iq) or
						    (IQ == reply) ->
					     case
					       privacy_check_packet(StateData,
								    From, To,
								    Packet, in)
						 of
					       allow ->
						   {true, Attrs, StateData};
					       deny when is_record(IQ, iq) ->
						   Err =
						       jlib:make_error_reply(Packet,
									     ?ERR_SERVICE_UNAVAILABLE),
						   ejabberd_router:route(To,
									 From,
									 Err),
						   {false, Attrs, StateData};
					       deny when IQ == reply ->
						   {false, Attrs, StateData}
					     end;
					 IQ
					     when (IQ == invalid) or
						    (IQ == not_iq) ->
					     {false, Attrs, StateData}
				       end;
				   <<"message">> ->
				       case privacy_check_packet(StateData,
								 From, To,
								 Packet, in)
					   of
					 allow ->
					     {true, Attrs, StateData};
					 deny ->
                                               case fxml:get_attr_s(<<"type">>, Attrs) of
                                                   <<"error">> -> ok;
                                                   <<"groupchat">> -> ok;
                                                   <<"headline">> -> ok;
                                                   _ ->
						       case fxml:get_subtag_with_xmlns(Packet,
										       <<"x">>,
										       ?NS_MUC_USER)
							   of
							 false ->
							     Err =
								 jlib:make_error_reply(Packet,
										       ?ERR_SERVICE_UNAVAILABLE),
							     ejabberd_router:route(To, From,
										   Err);
							 _ -> ok
						       end
                                               end,
                                               {false, Attrs, StateData}
				       end;
				   _ -> {true, Attrs, StateData}
				 end,
    if Pass ->
		?DEBUG("c2s Recv Pacet ~p ~n",[Packet]),
	    Attrs2 =
		jlib:replace_from_to_attrs(jid:to_string(From),
		    jid:to_string(To), NewAttrs),
	    FixedPacket0 = #xmlel{name = Name, attrs = Attrs2, children = Els},
	    FixedPacket = ejabberd_hooks:run_fold(
		    user_receive_packet,
		    NewState#state.server,
		    FixedPacket0,
		    [NewState, NewState#state.jid, From, To]),
	    SentStateData = send_packet(NewState, FixedPacket),
	    ejabberd_hooks:run(c2s_loop_debug, [{route, From, To, Packet}]),
	    fsm_next_state(StateName, SentStateData);
	true ->
	?DEBUG("c2s Recv true Pacet ~p ~n",[Packet]),
	    ejabberd_hooks:run(c2s_loop_debug, [{route, From, To, Packet}]),
	    fsm_next_state(StateName, NewState)
    end;
handle_info({'DOWN', Monitor, _Type, _Object, _Info},
	    _StateName, StateData)
    when Monitor == StateData#state.socket_monitor ->
    if StateData#state.mgmt_state == active;
       StateData#state.mgmt_state == pending ->
	   fsm_next_state(wait_for_resume, StateData);
       true ->
        send_stream_end(StateData, 111, <<"socket process down">>),    
	   {stop, normal, StateData}
    end;
handle_info(system_shutdown, StateName, StateData) ->
    case StateName of
      wait_for_stream ->
	  send_header(StateData, ?MYNAME, <<"1.0">>, <<"en">>),
%	  send_element(StateData, ?SERR_SYSTEM_SHUTDOWN),
      send_stream_end(StateData, 111, <<"system shutdown">>),
	  ok;
      _ ->
%	  send_element(StateData, ?SERR_SYSTEM_SHUTDOWN),
        send_stream_end(StateData, 111, <<"system shutdown">>),
	  ok
    end,
    {stop, normal, StateData};
handle_info({route_xmlstreamelement, El}, _StateName, StateData) ->
    {next_state, NStateName, NStateData, _Timeout} =
	session_established({xmlstreamelement, El}, StateData),
    fsm_next_state(NStateName, NStateData);
handle_info({force_update_presence, LUser, LServer}, StateName,
	    #state{jid = #jid{luser = LUser, lserver = LServer}} = StateData) ->
    NewStateData = case StateData#state.pres_last of
		     #xmlel{name = <<"presence">>} ->
			 PresenceEl =
			     ejabberd_hooks:run_fold(c2s_update_presence,
						     LServer,
						     StateData#state.pres_last,
						     [LUser, LServer]),
			 StateData2 = StateData#state{pres_last = PresenceEl},
			 presence_update(StateData2#state.jid, PresenceEl,
					 StateData2),
			 StateData2;
		     _ -> StateData
		   end,
    fsm_next_state(StateName, NewStateData);
handle_info({send_filtered, Feature, From, To, Packet}, StateName, StateData) ->
    Drop = ejabberd_hooks:run_fold(c2s_filter_packet, StateData#state.server,
				   true, [StateData#state.server, StateData,
					  Feature, To, Packet]),
    NewStateData = if Drop ->
			  ?DEBUG("Dropping packet from ~p to ~p",
				 [jid:to_string(From),
				  jid:to_string(To)]),
			  StateData;
		      true ->
			  FinalPacket = jlib:replace_from_to(From, To, Packet),
			  case StateData#state.jid of
			    To ->
				case privacy_check_packet(StateData, From, To,
							  FinalPacket, in) of
				  deny ->
				      StateData;
				  allow ->
				      send_stanza(StateData, FinalPacket)
				end;
			    _ ->
				ejabberd_router:route(From, To, FinalPacket),
				StateData
			  end
		   end,
    fsm_next_state(StateName, NewStateData);
handle_info({broadcast, Type, From, Packet}, StateName, StateData) ->
    Recipients = ejabberd_hooks:run_fold(
		   c2s_broadcast_recipients, StateData#state.server,
		   [],
		   [StateData#state.server, StateData, Type, From, Packet]),
    lists:foreach(
      fun(USR) ->
	      ejabberd_router:route(
		From, jid:make(USR), Packet)
      end, lists:usort(Recipients)),
    fsm_next_state(StateName, StateData);
handle_info({set_csi_state, CsiState}, StateName, StateData) ->
    fsm_next_state(StateName, StateData#state{csi_state = CsiState});
handle_info({set_resume_timeout, Timeout}, StateName, StateData) ->
    fsm_next_state(StateName, StateData#state{mgmt_timeout = Timeout});
handle_info(dont_ask_offline, StateName, StateData) ->
    fsm_next_state(StateName, StateData#state{ask_offline = false});
handle_info(close, StateName, StateData) ->
    ?DEBUG("Timeout waiting for stream management acknowledgement of ~s",
	   [jid:to_string(StateData#state.jid)]),
    close(self()),
    fsm_next_state(StateName, StateData#state{mgmt_ack_timer = undefined});
handle_info({_Ref, {resume, OldStateData}}, StateName, StateData) ->
    %% This happens if the resume_session/1 request timed out; the new session
    %% now receives the late response.
    ?DEBUG("Received old session state for ~s after failed resumption",
	   [jid:to_string(OldStateData#state.jid)]),
    handle_unacked_stanzas(OldStateData#state{mgmt_resend = false}),
    fsm_next_state(StateName, StateData);
handle_info(Info, StateName, StateData) ->
    ?ERROR_MSG("Unexpected info: ~p", [Info]),
    fsm_next_state(StateName, StateData).

print_state(State = #state{pres_t = T, pres_f = F, pres_a = A}) ->
    State#state{pres_t = {pres_t, (?SETS):size(T)},
		pres_f = {pres_f, (?SETS):size(F)},
		pres_a = {pres_a, (?SETS):size(A)}}.

terminate(Reason, StateName, StateData) ->
    ?DEBUG("Reason ~p ~n",[Reason]),
    spawn(login_success_util, close_stat, [StateData#state.user, StateData#state.server, StateData#state.resource, StateData#state.ip, StateData#state.sid]),
    case StateData#state.mgmt_state of
      resumed ->
	  ?INFO_MSG("Closing former stream of resumed session for ~s",
		    [jid:to_string(StateData#state.jid)]);
      _ ->
	  if StateName == session_established;
	     StateName == wait_for_resume ->
		 case StateData#state.authenticated of
		   replaced ->
		       ?INFO_MSG("(~w) Replaced session for ~s",
				 [StateData#state.socket,
				  jid:to_string(StateData#state.jid)]),
		       From = StateData#state.jid,
		       Packet = #xmlel{name = <<"presence">>,
				       attrs = [{<<"type">>, <<"unavailable">>}],
				       children =
					   [#xmlel{name = <<"status">>, attrs = [],
						   children =
						       [{xmlcdata,
							 <<"Replaced by new connection">>}]}]},
		       ejabberd_sm:close_session_unset_presence(StateData#state.sid,
								StateData#state.user,
								StateData#state.server,
								StateData#state.resource,
								<<"Replaced by new connection">>),
		       presence_broadcast(StateData, From,
					  StateData#state.pres_a, Packet);
		   _ ->
		       ?INFO_MSG("(~w) Close session for ~s",
				 [StateData#state.socket,
				  jid:to_string(StateData#state.jid)]),
		       EmptySet = (?SETS):new(),
		       case StateData of
			 #state{pres_last = undefined, pres_a = EmptySet} ->
			     ejabberd_sm:close_session(StateData#state.sid,
						       StateData#state.user,
						       StateData#state.server,
						       StateData#state.resource);
			 _ ->
			     From = StateData#state.jid,
			     Packet = #xmlel{name = <<"presence">>,
					     attrs = [{<<"type">>, <<"unavailable">>}],
					     children = []},
			     ejabberd_sm:close_session_unset_presence(StateData#state.sid,
								      StateData#state.user,
								      StateData#state.server,
								      StateData#state.resource,
								      <<"">>),
			     presence_broadcast(StateData, From,
						StateData#state.pres_a, Packet)
		       end,
		       case StateData#state.mgmt_state of
			 timeout ->
			     Info = [{num_stanzas_in,
				      StateData#state.mgmt_stanzas_in}],
			     ejabberd_sm:set_offline_info(StateData#state.sid,
							  StateData#state.user,
							  StateData#state.server,
							  StateData#state.resource,
							  Info);
			 _ ->
			    ok
		       end
		 end,
		 handle_unacked_stanzas(StateData);
%		 bounce_messages();
	     true ->
		 ok
	  end
    end,
    qtalk_public:clear_redis_user_key(StateData#state.server,StateData#state.user,StateData#state.resource),
%    catch send_trailer(StateData),
 %   term_send_stream_end(StateData),
    ?DEBUG("StateData#state.authenticated ~p ~n",[StateData#state.authenticated]),
    (StateData#state.sockmod):close(StateData#state.socket),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

change_shaper(StateData, JID) ->
    Shaper = acl:access_matches(StateData#state.shaper,
				#{usr => jid:split(JID), ip => StateData#state.ip},
				StateData#state.server),
    (StateData#state.sockmod):change_shaper(StateData#state.socket,
					    Shaper).

send_text(StateData, Text) when StateData#state.mgmt_state == pending ->
    ?DEBUG("Cannot send text while waiting for resumption: ~p", [Text]);
send_text(StateData, Text) when StateData#state.xml_socket ->
    ?DEBUG("Send Text on stream = ~p", [Text]),
    (StateData#state.sockmod):send_xml(StateData#state.socket,
				       {xmlstreamraw, Text});
send_text(StateData, Text) when StateData#state.mgmt_state == active ->
    ?DEBUG("Send XML on stream = ~p", [Text]),
    case catch (StateData#state.sockmod):send(StateData#state.socket, Text) of
      {'EXIT', _} ->
	  (StateData#state.sockmod):close(StateData#state.socket),
	  {error, closed};
      _ ->
	  ok
    end;
send_text(StateData, Text) ->
    ?DEBUG("Send XML on stream = ~p", [Text]),
    case catch (StateData#state.sockmod):send(StateData#state.socket, Text) of
      {'EXIT', _} ->
	  (StateData#state.sockmod):close(StateData#state.socket),
	  {error, closed};
      _ ->
	  ok
    end.

send_element(StateData, El) when StateData#state.mgmt_state == pending ->
    ?DEBUG("Cannot send element while waiting for resumption: ~p", [El]);
send_element(StateData, El) when StateData#state.xml_socket ->
    ?DEBUG("Send XML on stream = ~p", [fxml:element_to_binary(El)]),
    (StateData#state.sockmod):send_xml(StateData#state.socket,
				       {xmlstreamelement, El});
send_element(StateData, El) ->
	?DEBUG("send EL ~p ~n",[El]),
%    send_text(StateData, fxml:element_to_binary(El)).
    send_probuf_msg(StateData, El).

send_stanza(StateData, Stanza) when StateData#state.csi_state == inactive ->
    csi_filter_stanza(StateData, Stanza);
send_stanza(StateData, Stanza) when StateData#state.mgmt_state == pending ->
    mgmt_queue_add(StateData, Stanza);
send_stanza(StateData, Stanza) when StateData#state.mgmt_state == active ->
    NewStateData = mgmt_queue_add(StateData, Stanza),
    mgmt_send_stanza(NewStateData, Stanza);
send_stanza(StateData, Stanza) ->
    send_element(StateData, Stanza),
    StateData.

send_packet(StateData, Packet) ->
    case is_stanza(Packet) of
      true ->
	  send_stanza(StateData, Packet);
      false ->
	  send_element(StateData, Packet),
	  StateData
    end.

send_header(StateData, Server, Version, Lang)
    when StateData#state.xml_socket ->
    VersionAttr = case Version of
		    <<"">> -> [];
		    _ -> [{<<"version">>, Version}]
		  end,
    LangAttr = case Lang of
		 <<"">> -> [];
		 _ -> [{<<"xml:lang">>, Lang}]
	       end,
    Header = {xmlstreamstart, <<"stream:stream">>,
	      VersionAttr ++
		LangAttr ++
		  [{<<"xmlns">>, <<"jabber:client">>},
		   {<<"xmlns:stream">>,
		    <<"http://etherx.jabber.org/streams">>},
		   {<<"id">>, StateData#state.streamid},
		   {<<"from">>, Server}]},
    (StateData#state.sockmod):send_xml(StateData#state.socket,
				       Header);
send_header(StateData, Server, Version, Lang) ->
    VersionStr = case Version of
		   <<"">> -> <<"">>;
		   _ -> [<<" version='">>, Version, <<"'">>]
		 end,
    LangStr = case Lang of
		<<"">> -> <<"">>;
		_ -> [<<" xml:lang='">>, Lang, <<"'">>]
	      end,
    Header = io_lib:format(?STREAM_HEADER,
			   [StateData#state.streamid, Server, VersionStr,
			    LangStr]),
    send_text(StateData, iolist_to_binary(Header)).

new_id() -> randoms:get_string().

new_uniq_id() ->
    iolist_to_binary([randoms:get_string(),
		      jlib:integer_to_binary(p1_time_compat:unique_integer([positive]))]).

is_auth_packet(El) ->
    case jlib:iq_query_info(El) of
	#iq{id = ID, type = Type, xmlns = ?NS_AUTH, sub_el = SubEl} ->
	    #xmlel{children = Els} = SubEl,
	    {auth, ID, Type,
		get_auth_tags(Els, <<"">>, <<"">>, <<"">>, <<"">>)};
	_ -> false
    end.

is_stanza(#xmlel{name = Name, attrs = Attrs}) when Name == <<"message">>;
						   Name == <<"presence">>;
						   Name == <<"iq">> ->
    case fxml:get_attr(<<"xmlns">>, Attrs) of
      {value, NS} when NS /= <<"jabber:client">>,
		       NS /= <<"jabber:server">> ->
	  false;
      _ ->
	  true
    end;
is_stanza(_El) ->
    false.

get_auth_tags([#xmlel{name = Name, children = Els} | L],
	      U, P, D, R) ->
    CData = fxml:get_cdata(Els),
    case Name of
      <<"username">> -> get_auth_tags(L, CData, P, D, R);
      <<"password">> -> get_auth_tags(L, U, CData, D, R);
      <<"digest">> -> get_auth_tags(L, U, P, CData, R);
      <<"resource">> -> get_auth_tags(L, U, P, D, CData);
      _ -> get_auth_tags(L, U, P, D, R)
    end;
get_auth_tags([_ | L], U, P, D, R) ->
    get_auth_tags(L, U, P, D, R);
get_auth_tags([], U, P, D, R) ->
    {U, P, D, R}.

%% Copied from ejabberd_socket.erl
-record(socket_state, {sockmod, socket, receiver}).

get_conn_type(StateData) ->
    case (StateData#state.sockmod):get_sockmod(StateData#state.socket) of
    gen_tcp -> pb_c2s;
    fast_tls -> pb_c2s_tls;
    ezlib ->
	case ezlib:get_sockmod((StateData#state.socket)#socket_state.socket) of
	    gen_tcp -> c2s_compressed;
	    fast_tls -> c2s_compressed_tls
	end;
    ejabberd_http_bind -> http_bind;
    ejabberd_http_ws -> websocket;
    _ -> unknown
    end.

process_presence_probe(From, To, StateData) ->
    LFrom = jid:tolower(From),
    LBFrom = setelement(3, LFrom, <<"">>),
    case StateData#state.pres_last of
	undefined ->
	    ok;
	_ ->
	    Cond = ((?SETS):is_element(LFrom, StateData#state.pres_f)
		    orelse
		    ((LFrom /= LBFrom) andalso
		     (?SETS):is_element(LBFrom, StateData#state.pres_f))),
	    if Cond ->
		    %% To is the one sending the presence (the probe target)
		    Packet = jlib:add_delay_info(StateData#state.pres_last, To,
						 StateData#state.pres_timestamp),
		    case privacy_check_packet(StateData, To, From, Packet, out) of
			deny ->
			    ok;
			allow ->
			    Pid=element(2, StateData#state.sid),
			    ejabberd_hooks:run(presence_probe_hook, StateData#state.server, [From, To, Pid]),
			    %% Don't route a presence probe to oneself
			    case From == To of
				false ->
				    ejabberd_router:route(To, From, Packet);
			    	true ->
				    ok
			    end
		    end;
		true ->
		    ok
	    end
    end.

%% User updates his presence (non-directed presence packet)
presence_update(From, Packet, StateData) ->
    #xmlel{attrs = Attrs} = Packet,
    case fxml:get_attr_s(<<"type">>, Attrs) of
      <<"unavailable">> ->
	  Status = case fxml:get_subtag(Packet, <<"status">>) of
		     false -> <<"">>;
		     StatusTag -> fxml:get_tag_cdata(StatusTag)
		   end,
	  Info = [{ip, StateData#state.ip},
		  {conn, StateData#state.conn},
		  {auth_module, StateData#state.auth_module}],
	  ejabberd_sm:unset_presence(StateData#state.sid,
				     StateData#state.user,
				     StateData#state.server,
				     StateData#state.resource, Status, Info),
	  presence_broadcast(StateData, From,
			     StateData#state.pres_a, Packet),
	  StateData#state{pres_last = undefined,
			  pres_timestamp = undefined, pres_a = (?SETS):new()};
      <<"error">> -> StateData;
      <<"probe">> -> StateData;
      <<"subscribe">> -> StateData;
      <<"subscribed">> -> StateData;
      <<"unsubscribe">> -> StateData;
      <<"unsubscribed">> -> StateData;
      _ ->
	  OldPriority = case StateData#state.pres_last of
			  undefined -> 0;
			  OldPresence -> get_priority_from_presence(OldPresence)
			end,
	  NewPriority = get_priority_from_presence(Packet),
	  update_priority(NewPriority, Packet, StateData),
	  FromUnavail = (StateData#state.pres_last == undefined),
	  ?DEBUG("from unavail = ~p~n", [FromUnavail]),
	  NewStateData = StateData#state{pres_last = Packet,
					 pres_timestamp = p1_time_compat:timestamp()},
      catch record_user_show_tag(Packet,NewStateData),
	  NewState = if FromUnavail ->
			    ejabberd_hooks:run(user_available_hook,
					       NewStateData#state.server,
					       [NewStateData#state.jid]),
			    ResentStateData = if NewPriority >= 0 ->
						     resend_offline_messages(NewStateData,true),
						     resend_subscription_requests(NewStateData);
						 true -> NewStateData
					      end,
			    presence_broadcast_first(From, ResentStateData,
						     Packet);
			true ->
			    presence_broadcast_to_trusted(NewStateData, From,
							  NewStateData#state.pres_f,
							  NewStateData#state.pres_a,
							  Packet),
			    if OldPriority < 0, NewPriority >= 0 ->
				   resend_offline_messages(NewStateData,true);
			       true -> ok
			    end,
			    NewStateData
		     end,
	  NewState
    end.

%% User sends a directed presence packet
presence_track(From, To, Packet, StateData) ->
    #xmlel{attrs = Attrs} = Packet,
    LTo = jid:tolower(To),
    User = StateData#state.user,
    Server = StateData#state.server,
    case fxml:get_attr_s(<<"type">>, Attrs) of
      <<"unavailable">> ->
	  A = remove_element(LTo, StateData#state.pres_a),
	  check_privacy_route(From, StateData#state{pres_a = A}, From, To, Packet);
      <<"subscribe">> ->
	  try_roster_subscribe(subscribe, User, Server, From, To, Packet, StateData);
      <<"subscribed">> ->
	  ejabberd_hooks:run(roster_out_subscription, Server,
			     [User, Server, To, subscribed]),
	  check_privacy_route(From, StateData,
			      jid:remove_resource(From), To, Packet);
      <<"unsubscribe">> ->
	  try_roster_subscribe(unsubscribe, User, Server, From, To, Packet, StateData);
      <<"unsubscribed">> ->
	  ejabberd_hooks:run(roster_out_subscription, Server,
			     [User, Server, To, unsubscribed]),
	  check_privacy_route(From, StateData,
			      jid:remove_resource(From), To, Packet);
      <<"error">> ->
	  check_privacy_route(From, StateData, From, To, Packet);
      <<"probe">> ->
	  check_privacy_route(From, StateData, From, To, Packet);
      <<"notify">> ->
	  check_privacy_route(From, StateData, From, To, Packet);
      <<"verify_friend">> ->
       NewPacket = make_new_presence_packet(StateData#state.server,From,Packet,Attrs),
       check_privacy_route(From, StateData, From, To, NewPacket),
      StateData;
      <<"manual_authentication_confirm">> ->
       NewPacket = make_new_presence_packet(StateData#state.server,From,Packet,Attrs),
       check_privacy_route(From, StateData, From, To, NewPacket),
       StateData;
      _ ->
	  A = (?SETS):add_element(LTo, StateData#state.pres_a),
	  check_privacy_route(From, StateData#state{pres_a = A}, From, To, Packet)
    end.

check_privacy_route(From, StateData, FromRoute, To,
		    Packet) ->
    case privacy_check_packet(StateData, From, To, Packet,
			      out)
	of
        deny ->
            Lang = StateData#state.lang,
            ErrText = <<"Your active privacy list has denied "
                       "the routing of this stanza.">>,
            Err = jlib:make_error_reply(Packet,
                                        ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)),
            Err2 = jlib:replace_from_to(To, From, Err),
            send_stanza(StateData, Err2);
        allow ->
            ejabberd_router:route(FromRoute, To, Packet),
            StateData
    end.

%% Check if privacy rules allow this delivery
privacy_check_packet(StateData, From, To, Packet,
		     Dir) ->
    ejabberd_hooks:run_fold(privacy_check_packet,
			    StateData#state.server, allow,
			    [StateData#state.user, StateData#state.server,
			     StateData#state.privacy_list, {From, To, Packet},
			     Dir]).

is_privacy_allow(StateData, From, To, Packet, Dir) ->
    allow ==
      privacy_check_packet(StateData, From, To, Packet, Dir).

%%% Check ACL before allowing to send a subscription stanza
try_roster_subscribe(Type, User, Server, From, To, Packet, StateData) ->
    JID1 = jid:make(User, Server, <<"">>),
    Access = gen_mod:get_module_opt(Server, mod_roster, access, fun(A) when is_atom(A) -> A end, all),
    case acl:match_rule(Server, Access, JID1) of
	deny ->
	    %% Silently drop this (un)subscription request
	    StateData;
	allow ->
	    ejabberd_hooks:run(roster_out_subscription,
			       Server,
			       [User, Server, To, Type]),
	    check_privacy_route(From, StateData, jid:remove_resource(From),
				To, Packet)
    end.

%% Send presence when disconnecting
presence_broadcast(StateData, From, JIDSet, Packet) ->
    JIDs = ?SETS:to_list(JIDSet),
    JIDs2 = format_and_check_privacy(From, StateData, Packet, JIDs, out),
    Server = StateData#state.server,
    send_multiple(From, Server, JIDs2, Packet).

%% Send presence when updating presence
presence_broadcast_to_trusted(StateData, From, Trusted, JIDSet, Packet) ->
    JIDs = ?SETS:to_list(JIDSet),
    JIDs_trusted = [JID || JID <- JIDs, ?SETS:is_element(JID, Trusted)],
    JIDs2 = format_and_check_privacy(From, StateData, Packet, JIDs_trusted, out),
    Server = StateData#state.server,
    send_multiple(From, Server, JIDs2, Packet).

%% Send presence when connecting
presence_broadcast_first(From, StateData, Packet) ->
    JIDsProbe =
	?SETS:fold(
	   fun(JID, L) -> [JID | L] end,
	   [],
	   StateData#state.pres_t),
    PacketProbe = #xmlel{name = <<"presence">>, attrs = [{<<"type">>,<<"probe">>}], children = []},
    JIDs2Probe = format_and_check_privacy(From, StateData, PacketProbe, JIDsProbe, out),
    Server = StateData#state.server,
    send_multiple(From, Server, JIDs2Probe, PacketProbe),
    {As, JIDs} =
	?SETS:fold(
	   fun(JID, {A, JID_list}) ->
		   {?SETS:add_element(JID, A), JID_list++[JID]}
	   end,
	   {StateData#state.pres_a, []},
	   StateData#state.pres_f),
    JIDs2 = format_and_check_privacy(From, StateData, Packet, JIDs, out),
    send_multiple(From, Server, JIDs2, Packet),
    StateData#state{pres_a = As}.

format_and_check_privacy(From, StateData, Packet, JIDs, Dir) ->
    FJIDs = [jid:make(JID) || JID <- JIDs],
    lists:filter(
      fun(FJID) ->
	      case ejabberd_hooks:run_fold(
		     privacy_check_packet, StateData#state.server,
		     allow,
		     [StateData#state.user,
		      StateData#state.server,
		      StateData#state.privacy_list,
		      {From, FJID, Packet},
		      Dir]) of
		  deny -> false;
		  allow -> true
	      end
      end,
      FJIDs).

send_multiple(From, Server, JIDs, Packet) ->
    ejabberd_router_multicast:route_multicast(From, Server, JIDs, Packet).

remove_element(E, Set) ->
    case (?SETS):is_element(E, Set) of
      true -> (?SETS):del_element(E, Set);
      _ -> Set
    end.

roster_change(IJID, ISubscription, StateData) ->
    LIJID = jid:tolower(IJID),
    IsFrom = (ISubscription == both) or (ISubscription == from),
    IsTo = (ISubscription == both) or (ISubscription == to),
    OldIsFrom = (?SETS):is_element(LIJID, StateData#state.pres_f),
    FSet = if
	       IsFrom -> (?SETS):add_element(LIJID, StateData#state.pres_f);
	       true -> remove_element(LIJID, StateData#state.pres_f)
	   end,
    TSet = if
	       IsTo -> (?SETS):add_element(LIJID, StateData#state.pres_t);
	       true -> remove_element(LIJID, StateData#state.pres_t)
	   end,
    case StateData#state.pres_last of
      undefined ->
	  StateData#state{pres_f = FSet, pres_t = TSet};
      P ->
	  ?DEBUG("roster changed for ~p~n",
		 [StateData#state.user]),
	  From = StateData#state.jid,
	  To = jid:make(IJID),
	  Cond1 = IsFrom andalso not OldIsFrom,
	  Cond2 = not IsFrom andalso OldIsFrom andalso
		    ((?SETS):is_element(LIJID, StateData#state.pres_a)),
	  if Cond1 ->
		 ?DEBUG("C1: ~p~n", [LIJID]),
		 case privacy_check_packet(StateData, From, To, P, out)
		     of
		   deny -> ok;
		   allow -> ejabberd_router:route(From, To, P)
		 end,
		 A = (?SETS):add_element(LIJID, StateData#state.pres_a),
		 StateData#state{pres_a = A, pres_f = FSet,
				 pres_t = TSet};
	     Cond2 ->
		 ?DEBUG("C2: ~p~n", [LIJID]),
		 PU = #xmlel{name = <<"presence">>,
			     attrs = [{<<"type">>, <<"unavailable">>}],
			     children = []},
		 case privacy_check_packet(StateData, From, To, PU, out)
		     of
		   deny -> ok;
		   allow -> ejabberd_router:route(From, To, PU)
		 end,
		 A = remove_element(LIJID, StateData#state.pres_a),
		 StateData#state{pres_a = A, pres_f = FSet,
				 pres_t = TSet};
	     true -> StateData#state{pres_f = FSet, pres_t = TSet}
	  end
    end.

update_priority(Priority, Packet, StateData) ->
    Info = [{ip, StateData#state.ip}, {conn, StateData#state.conn},
	    {auth_module, StateData#state.auth_module}],
    ejabberd_sm:set_presence(StateData#state.sid,
			     StateData#state.user, StateData#state.server,
			     StateData#state.resource, Priority, Packet, Info).

get_priority_from_presence(PresencePacket) ->
    case fxml:get_subtag(PresencePacket, <<"priority">>) of
      false -> 0;
      SubEl ->
	  case catch
		 jlib:binary_to_integer(fxml:get_tag_cdata(SubEl))
	      of
	    P when is_integer(P) -> P;
	    _ -> 0
	  end
    end.

process_privacy_iq(From, To,
		   #iq{type = Type, lang = Lang, sub_el = SubEl} = IQ, StateData) ->
    Txt = <<"No module is handling this query">>,
    {Res, NewStateData} =
	case Type of
	    get ->
		R = ejabberd_hooks:run_fold(
		      privacy_iq_get,
		      StateData#state.server,
		      {error, ?ERRT_FEATURE_NOT_IMPLEMENTED(Lang, Txt)},
		      [From, To, IQ,
		       StateData#state.privacy_list]),
		{R, StateData};
	    set ->
		case ejabberd_hooks:run_fold(
		       privacy_iq_set,
		       StateData#state.server,
		       {error, ?ERRT_FEATURE_NOT_IMPLEMENTED(Lang, Txt)},
		       [From, To, IQ])
		of
		    {result, R, NewPrivList} ->
			{{result, R},
			 StateData#state{privacy_list =
					     NewPrivList}};
		    R -> {R, StateData}
		end
	end,
    IQRes = case Res of
	      {result, Result} ->
		  IQ#iq{type = result, sub_el = Result};
	      {error, Error} ->
		  IQ#iq{type = error, sub_el = [SubEl, Error]}
	    end,
    ejabberd_router:route(To, From, jlib:iq_to_xml(IQRes)),
    NewStateData.

resend_offline_messages(_StateData,_) ->
    ok.

resend_subscription_requests(#state{user = User,
				    server = Server} = StateData) ->
    PendingSubscriptions =
	ejabberd_hooks:run_fold(resend_subscription_requests_hook,
				Server, [], [User, Server]),
    lists:foldl(fun (XMLPacket, AccStateData) ->
			send_packet(AccStateData, XMLPacket)
		end,
		StateData,
		PendingSubscriptions).

get_showtag(undefined) -> <<"unavailable">>;
get_showtag(Presence) ->
    case fxml:get_path_s(Presence, [{elem, <<"show">>}, cdata]) of
	<<"">> -> <<"available">>;
	ShowTag -> ShowTag
    end.

get_statustag(undefined) -> <<"">>;
get_statustag(Presence) ->
    fxml:get_path_s(Presence, [{elem, <<"status">>}, cdata]).

process_unauthenticated_stanza(StateData, El) ->
    NewEl = case fxml:get_tag_attr_s(<<"xml:lang">>, El) of
	      <<"">> ->
		  case StateData#state.lang of
		    <<"">> -> El;
		    Lang -> fxml:replace_tag_attr(<<"xml:lang">>, Lang, El)
		  end;
	      _ -> El
	    end,
    case jlib:iq_query_info(NewEl) of
      #iq{lang = L} = IQ ->
	  Res = ejabberd_hooks:run_fold(c2s_unauthenticated_iq,
					StateData#state.server, empty,
					[StateData#state.server, IQ,
					 StateData#state.ip]),
	  case Res of
	    empty ->
		Txt = <<"Authentication required">>,
		ResIQ = IQ#iq{type = error,
			      sub_el = [?ERRT_SERVICE_UNAVAILABLE(L, Txt)]},
		Res1 = jlib:replace_from_to(jid:make(<<"">>,
							  StateData#state.server,
							  <<"">>),
					    jid:make(<<"">>, <<"">>,
							  <<"">>),
					    jlib:iq_to_xml(ResIQ)),
		send_element(StateData,
			     jlib:remove_attr(<<"to">>, Res1));
	    _ -> send_element(StateData, Res)
	  end;
      _ ->
	  % Drop any stanza, which isn't IQ stanza
	  ok
    end.

peerip(SockMod, Socket) ->
    IP = case SockMod of
	   gen_tcp -> inet:peername(Socket);
	   _ -> SockMod:peername(Socket)
	 end,
    case IP of
      {ok, IPOK} -> IPOK;
      _ -> undefined
    end.

%% fsm_next_state_pack: Pack the StateData structure to improve
%% sharing.
fsm_next_state_pack(StateName, StateData) ->
    fsm_next_state_gc(StateName, pack(StateData)).

%% fsm_next_state_gc: Garbage collect the process heap to make use of
%% the newly packed StateData structure.
fsm_next_state_gc(StateName, PackedStateData) ->
    erlang:garbage_collect(),
    fsm_next_state(StateName, PackedStateData).

%% fsm_next_state: Generate the next_state FSM tuple with different
%% timeout, depending on the future state
fsm_next_state(session_established, #state{mgmt_max_queue = exceeded} =
	       StateData) ->
    ?WARNING_MSG("ACK queue too long, terminating session for ~s",
		 [jid:to_string(StateData#state.jid)]),
    send_stream_end(StateData, 102, <<"Too many unacked stanzas">>),
    {stop, normal, StateData#state{mgmt_resend = false}};
fsm_next_state(session_established, #state{mgmt_state = pending} = StateData) ->
    fsm_next_state(wait_for_resume, StateData);
fsm_next_state(session_established, StateData) ->
    {next_state, session_established, StateData,
     ?C2S_HIBERNATE_TIMEOUT};
fsm_next_state(wait_for_resume, #state{mgmt_timeout = 0} = StateData) ->
    send_stream_end(StateData, 102, <<"mgmt_timeout">>),
    {stop, normal, StateData};
fsm_next_state(wait_for_resume, #state{mgmt_pending_since = undefined,
				       sid = SID, jid = JID, ip = IP,
				       conn = Conn, auth_module = AuthModule,
				       server = Host} = StateData) ->
    case StateData of
      #state{mgmt_ack_timer = undefined} ->
	  ok;
      #state{mgmt_ack_timer = Timer} ->
	  erlang:cancel_timer(Timer)
    end,
    ?INFO_MSG("Waiting for resumption of stream for ~s",
	      [jid:to_string(JID)]),
    Info = [{ip, IP}, {conn, Conn}, {auth_module, AuthModule}],
    NewStateData = ejabberd_hooks:run_fold(c2s_session_pending, Host, StateData,
					   [SID, JID, Info]),
    {next_state, wait_for_resume,
     NewStateData#state{mgmt_state = pending,
			mgmt_pending_since = os:timestamp()},
     NewStateData#state.mgmt_timeout};
fsm_next_state(wait_for_resume, StateData) ->
    Diff = timer:now_diff(os:timestamp(), StateData#state.mgmt_pending_since),
    Timeout = max(StateData#state.mgmt_timeout - Diff div 1000, 1),
    {next_state, wait_for_resume, StateData, Timeout};
fsm_next_state(StateName, StateData) ->
    {next_state, StateName, StateData, ?C2S_OPEN_TIMEOUT}.

%% fsm_reply: Generate the reply FSM tuple with different timeout,
%% depending on the future state
fsm_reply(Reply, session_established, StateData) ->
    {reply, Reply, session_established, StateData,
     ?C2S_HIBERNATE_TIMEOUT};
fsm_reply(Reply, wait_for_resume, StateData) ->
    Diff = timer:now_diff(os:timestamp(), StateData#state.mgmt_pending_since),
    Timeout = max(StateData#state.mgmt_timeout - Diff div 1000, 1),
    {reply, Reply, wait_for_resume, StateData, Timeout};
fsm_reply(Reply, StateName, StateData) ->
    {reply, Reply, StateName, StateData, ?C2S_OPEN_TIMEOUT}.

%% Used by c2s blacklist plugins
is_ip_blacklisted(undefined, _Lang) -> false;
is_ip_blacklisted({IP, _Port}, Lang) ->
    ejabberd_hooks:run_fold(check_bl_c2s, false, [IP, Lang]).

%% Check from attributes
%% returns invalid-from|NewElement
check_from(El, FromJID) ->
    case fxml:get_tag_attr(<<"from">>, El) of
	false ->
	    El;
	{value, SJID} ->
	    JID = jid:from_string(SJID),
	    case JID of
		error ->
		    'invalid-from';
		#jid{} ->
		    if
			(JID#jid.luser == FromJID#jid.luser) and
				(JID#jid.lserver == FromJID#jid.lserver) and
				(JID#jid.lresource == FromJID#jid.lresource) ->
			    El;
			(JID#jid.luser == FromJID#jid.luser) and
				(JID#jid.lserver == FromJID#jid.lserver) and
				(JID#jid.lresource == <<"">>) ->
			    El;
			true ->
			    'invalid-from'
		    end
	    end
    end.

fsm_limit_opts(Opts) ->
    case lists:keysearch(max_fsm_queue, 1, Opts) of
      {value, {_, N}} when is_integer(N) -> [{max_queue, N}];
      _ ->
	  case ejabberd_config:get_option(
                 max_fsm_queue,
                 fun(I) when is_integer(I), I > 0 -> I end) of
            undefined -> [];
	    N -> [{max_queue, N}]
	  end
    end.

process_compression_request(El, StateName, StateData) ->
    case fxml:get_subtag(El, <<"method">>) of
	false ->
	    send_element(StateData,
			 #xmlel{name = <<"failure">>,
				attrs = [{<<"xmlns">>, ?NS_COMPRESS}],
				children =
				[#xmlel{name = <<"setup-failed">>,
					attrs = [], children = []}]}),
	    fsm_next_state(StateName, StateData);
	Method ->
	    case fxml:get_tag_cdata(Method) of
		<<"zlib">> ->
		    Socket = StateData#state.socket,
		    BCompressed = fxml:element_to_binary(
				    #xmlel{name = <<"compressed">>,
					   attrs = [{<<"xmlns">>,
						     ?NS_COMPRESS}]}),
		    ZlibSocket = (StateData#state.sockmod):compress(
				   Socket, BCompressed),
		    fsm_next_state(wait_for_stream,
				   StateData#state{socket = ZlibSocket,
						   streamid = new_id()});
		_ ->
		    send_element(StateData,
				 #xmlel{name = <<"failure">>,
					attrs = [{<<"xmlns">>, ?NS_COMPRESS}],
					children =
					[#xmlel{name = <<"unsupported-method">>,
						attrs = [],
						children = []}]}),
		    fsm_next_state(StateName, StateData)
	    end
    end.

%%%----------------------------------------------------------------------
%%% XEP-0191
%%%----------------------------------------------------------------------

route_blocking(What, StateData) ->
    SubEl = case What of
	      {block, JIDs} ->
		  #xmlel{name = <<"block">>,
			 attrs = [{<<"xmlns">>, ?NS_BLOCKING}],
			 children =
			     lists:map(fun (JID) ->
					       #xmlel{name = <<"item">>,
						      attrs =
							  [{<<"jid">>,
							    jid:to_string(JID)}],
						      children = []}
				       end,
				       JIDs)};
	      {unblock, JIDs} ->
		  #xmlel{name = <<"unblock">>,
			 attrs = [{<<"xmlns">>, ?NS_BLOCKING}],
			 children =
			     lists:map(fun (JID) ->
					       #xmlel{name = <<"item">>,
						      attrs =
							  [{<<"jid">>,
							    jid:to_string(JID)}],
						      children = []}
				       end,
				       JIDs)};
	      unblock_all ->
		  #xmlel{name = <<"unblock">>,
			 attrs = [{<<"xmlns">>, ?NS_BLOCKING}], children = []}
	    end,
    PrivPushIQ = #iq{type = set, id = <<"push">>, sub_el = [SubEl]},
    PrivPushEl =
	jlib:replace_from_to(jid:remove_resource(StateData#state.jid),
			     StateData#state.jid, jlib:iq_to_xml(PrivPushIQ)),
    %% No need to replace active privacy list here,
    %% blocking pushes are always accompanied by
    %% Privacy List pushes
    send_stanza(StateData, PrivPushEl).

%%%----------------------------------------------------------------------
%%% XEP-0198
%%%----------------------------------------------------------------------

stream_mgmt_enabled(#state{mgmt_state = disabled}) ->
    false;
stream_mgmt_enabled(_StateData) ->
    true.

dispatch_stream_mgmt(El, #state{mgmt_state = MgmtState} = StateData)
    when MgmtState == active;
	 MgmtState == pending ->
    perform_stream_mgmt(El, StateData);
dispatch_stream_mgmt(El, StateData) ->
    negotiate_stream_mgmt(El, StateData).

negotiate_stream_mgmt(_El, #state{resource = <<"">>} = StateData) ->
    %% XEP-0198 says: "For client-to-server connections, the client MUST NOT
    %% attempt to enable stream management until after it has completed Resource
    %% Binding unless it is resuming a previous session".  However, it also
    %% says: "Stream management errors SHOULD be considered recoverable", so we
    %% won't bail out.
    send_element(StateData, ?MGMT_UNEXPECTED_REQUEST(?NS_STREAM_MGMT_3)),
    StateData;
negotiate_stream_mgmt(#xmlel{name = Name, attrs = Attrs}, StateData) ->
    case fxml:get_attr_s(<<"xmlns">>, Attrs) of
      Xmlns when ?IS_SUPPORTED_MGMT_XMLNS(Xmlns) ->
	  case stream_mgmt_enabled(StateData) of
	    true ->
		case Name of
		  <<"enable">> ->
		      handle_enable(StateData#state{mgmt_xmlns = Xmlns}, Attrs);
		  _ ->
		      Res = if Name == <<"a">>;
			       Name == <<"r">>;
			       Name == <<"resume">> ->
				   ?MGMT_UNEXPECTED_REQUEST(Xmlns);
			       true ->
				   ?MGMT_BAD_REQUEST(Xmlns)
			    end,
		      send_element(StateData, Res),
		      StateData
		end;
	    false ->
	      send_element(StateData, ?MGMT_SERVICE_UNAVAILABLE(Xmlns)),
	      StateData
	  end;
      _ ->
	  send_element(StateData, ?MGMT_UNSUPPORTED_VERSION(?NS_STREAM_MGMT_3)),
	  StateData
    end.

perform_stream_mgmt(#xmlel{name = Name, attrs = Attrs}, StateData) ->
    case fxml:get_attr_s(<<"xmlns">>, Attrs) of
      Xmlns when Xmlns == StateData#state.mgmt_xmlns ->
	  case Name of
	    <<"r">> ->
		handle_r(StateData);
	    <<"a">> ->
		handle_a(StateData, Attrs);
	    _ ->
		Res = if Name == <<"enable">>;
			 Name == <<"resume">> ->
			     ?MGMT_UNEXPECTED_REQUEST(Xmlns);
			 true ->
			     ?MGMT_BAD_REQUEST(Xmlns)
		      end,
		send_element(StateData, Res),
		StateData
	  end;
      _ ->
	  send_element(StateData,
		       ?MGMT_UNSUPPORTED_VERSION(StateData#state.mgmt_xmlns)),
	  StateData
    end.

handle_enable(#state{mgmt_timeout = DefaultTimeout,
		     mgmt_max_timeout = MaxTimeout} = StateData, Attrs) ->
    Timeout = case fxml:get_attr_s(<<"resume">>, Attrs) of
		ResumeAttr when ResumeAttr == <<"true">>;
				ResumeAttr == <<"1">> ->
		    MaxAttr = fxml:get_attr_s(<<"max">>, Attrs),
		    case catch jlib:binary_to_integer(MaxAttr) of
		      Max when is_integer(Max), Max > 0, Max =< MaxTimeout ->
			  Max;
		      _ ->
			  DefaultTimeout
		    end;
		_ ->
		    0
	      end,
    ResAttrs = [{<<"xmlns">>, StateData#state.mgmt_xmlns}] ++
	if Timeout > 0 ->
	       ?INFO_MSG("Stream management with resumption enabled for ~s",
			 [jid:to_string(StateData#state.jid)]),
	       [{<<"id">>, make_resume_id(StateData)},
		{<<"resume">>, <<"true">>},
		{<<"max">>, jlib:integer_to_binary(Timeout)}];
	   true ->
	       ?INFO_MSG("Stream management without resumption enabled for ~s",
			 [jid:to_string(StateData#state.jid)]),
	       []
	end,
    Res = #xmlel{name = <<"enabled">>,
		 attrs = ResAttrs,
		 children = []},
    send_element(StateData, Res),
    StateData#state{mgmt_state = active,
		    mgmt_queue = queue:new(),
		    mgmt_timeout = Timeout * 1000}.

handle_r(StateData) ->
    H = jlib:integer_to_binary(StateData#state.mgmt_stanzas_in),
    Res = #xmlel{name = <<"a">>,
		 attrs = [{<<"xmlns">>, StateData#state.mgmt_xmlns},
			  {<<"h">>, H}],
		 children = []},
    send_element(StateData, Res),
    StateData.

handle_a(StateData, Attrs) ->
    case catch jlib:binary_to_integer(fxml:get_attr_s(<<"h">>, Attrs)) of
      H when is_integer(H), H >= 0 ->
	  NewStateData = check_h_attribute(StateData, H),
	  maybe_renew_ack_request(NewStateData);
      _ ->
	  ?DEBUG("Ignoring invalid ACK element from ~s",
		 [jid:to_string(StateData#state.jid)]),
	  StateData
    end.

handle_resume(StateData, Attrs) ->
    R = case fxml:get_attr_s(<<"xmlns">>, Attrs) of
	  Xmlns when ?IS_SUPPORTED_MGMT_XMLNS(Xmlns) ->
	      case stream_mgmt_enabled(StateData) of
		true ->
		    case {fxml:get_attr(<<"previd">>, Attrs),
			  catch jlib:binary_to_integer(fxml:get_attr_s(<<"h">>, Attrs))}
			of
		      {{value, PrevID}, H} when is_integer(H), H >= 0 ->
			  case inherit_session_state(StateData, PrevID) of
			    {ok, InheritedState, Info} ->
				{ok, InheritedState, Info, H};
			    {error, Err, InH} ->
				{error, ?MGMT_ITEM_NOT_FOUND_H(Xmlns, InH), Err};
			    {error, Err} ->
				{error, ?MGMT_ITEM_NOT_FOUND(Xmlns), Err}
			  end;
		      _ ->
			  {error, ?MGMT_BAD_REQUEST(Xmlns),
			   <<"Invalid request">>}
		    end;
		false ->
		    {error, ?MGMT_SERVICE_UNAVAILABLE(Xmlns),
		     <<"XEP-0198 disabled">>}
	      end;
	  _ ->
	      {error, ?MGMT_UNSUPPORTED_VERSION(?NS_STREAM_MGMT_3),
	       <<"Invalid XMLNS">>}
	end,
    case R of
      {ok, ResumedState, ResumedInfo, NumHandled} ->
	  NewState = check_h_attribute(ResumedState, NumHandled),
	  AttrXmlns = NewState#state.mgmt_xmlns,
	  AttrId = make_resume_id(NewState),
	  AttrH = jlib:integer_to_binary(NewState#state.mgmt_stanzas_in),
	  send_element(NewState,
		       #xmlel{name = <<"resumed">>,
			      attrs = [{<<"xmlns">>, AttrXmlns},
				       {<<"h">>, AttrH},
				       {<<"previd">>, AttrId}],
			      children = []}),
	  SendFun = fun(_F, _T, El, Time) ->
			    NewEl = add_resent_delay_info(NewState, El, Time),
			    send_element(NewState, NewEl)
		    end,
	  handle_unacked_stanzas(NewState, SendFun),
	  send_element(NewState,
		       #xmlel{name = <<"r">>,
			      attrs = [{<<"xmlns">>, AttrXmlns}],
			      children = []}),
	  NewState1 = csi_flush_queue(NewState),
	  NewState2 = ejabberd_hooks:run_fold(c2s_session_resumed,
					      StateData#state.server,
					      NewState1,
					      [NewState1#state.sid,
					       NewState1#state.jid,
					       ResumedInfo]),
	  ?INFO_MSG("Resumed session for ~s",
		    [jid:to_string(NewState2#state.jid)]),
	  {ok, NewState2};
      {error, El, Msg} ->
	  send_element(StateData, El),
	  ?INFO_MSG("Cannot resume session for ~s@~s: ~s",
		    [StateData#state.user, StateData#state.server, Msg]),
	  error
    end.

check_h_attribute(#state{mgmt_stanzas_out = NumStanzasOut} = StateData, H)
    when H > NumStanzasOut ->
    ?DEBUG("~s acknowledged ~B stanzas, but only ~B were sent",
	   [jid:to_string(StateData#state.jid), H, NumStanzasOut]),
    mgmt_queue_drop(StateData#state{mgmt_stanzas_out = H}, NumStanzasOut);
check_h_attribute(#state{mgmt_stanzas_out = NumStanzasOut} = StateData, H) ->
    ?DEBUG("~s acknowledged ~B of ~B stanzas",
	   [jid:to_string(StateData#state.jid), H, NumStanzasOut]),
    mgmt_queue_drop(StateData, H).

update_num_stanzas_in(#state{mgmt_state = MgmtState} = StateData, El)
    when MgmtState == active;
	 MgmtState == pending ->
    NewNum = case {is_stanza(El), StateData#state.mgmt_stanzas_in} of
	       {true, 4294967295} ->
		   0;
	       {true, Num} ->
		   Num + 1;
	       {false, Num} ->
		   Num
	     end,
    StateData#state{mgmt_stanzas_in = NewNum};
update_num_stanzas_in(StateData, _El) ->
    StateData.

mgmt_send_stanza(StateData, Stanza) ->
    case send_element(StateData, Stanza) of
      ok ->
	  maybe_request_ack(StateData);
      _ ->
	  StateData#state{mgmt_state = pending}
    end.

maybe_request_ack(#state{mgmt_ack_timer = undefined} = StateData) ->
    request_ack(StateData);
maybe_request_ack(StateData) ->
    StateData.

request_ack(#state{mgmt_xmlns = Xmlns,
		   mgmt_ack_timeout = AckTimeout} = StateData) ->
    AckReq = #xmlel{name = <<"r">>, attrs = [{<<"xmlns">>, Xmlns}]},
    case {send_element(StateData, AckReq), AckTimeout} of
      {ok, undefined} ->
	  ok;
      {ok, Timeout} ->
	  Timer = erlang:send_after(Timeout, self(), close),
	  StateData#state{mgmt_ack_timer = Timer,
			  mgmt_stanzas_req = StateData#state.mgmt_stanzas_out};
      _ ->
	  StateData#state{mgmt_state = pending}
    end.

maybe_renew_ack_request(#state{mgmt_ack_timer = undefined} = StateData) ->
    StateData;
maybe_renew_ack_request(#state{mgmt_ack_timer = Timer,
			       mgmt_queue = Queue,
			       mgmt_stanzas_out = NumStanzasOut,
			       mgmt_stanzas_req = NumStanzasReq} = StateData) ->
    erlang:cancel_timer(Timer),
    case NumStanzasReq < NumStanzasOut andalso not queue:is_empty(Queue) of
      true ->
	  request_ack(StateData#state{mgmt_ack_timer = undefined});
      false ->
	  StateData#state{mgmt_ack_timer = undefined}
    end.

mgmt_queue_add(StateData, El) ->
    NewNum = case StateData#state.mgmt_stanzas_out of
	       4294967295 ->
		   0;
	       Num ->
		   Num + 1
	     end,
    NewQueue = queue:in({NewNum, p1_time_compat:timestamp(), El}, StateData#state.mgmt_queue),
    NewState = StateData#state{mgmt_queue = NewQueue,
			       mgmt_stanzas_out = NewNum},
    check_queue_length(NewState).

mgmt_queue_drop(StateData, NumHandled) ->
    NewQueue = jlib:queue_drop_while(fun({N, _T, _E}) -> N =< NumHandled end,
				     StateData#state.mgmt_queue),
    StateData#state{mgmt_queue = NewQueue}.

check_queue_length(#state{mgmt_max_queue = Limit} = StateData)
    when Limit == infinity;
	 Limit == exceeded ->
    StateData;
check_queue_length(#state{mgmt_queue = Queue,
			  mgmt_max_queue = Limit} = StateData) ->
    case queue:len(Queue) > Limit of
      true ->
	  StateData#state{mgmt_max_queue = exceeded};
      false ->
	  StateData
    end.

handle_unacked_stanzas(#state{mgmt_state = MgmtState} = StateData, F)
    when MgmtState == active;
	 MgmtState == pending;
	 MgmtState == timeout ->
    Queue = StateData#state.mgmt_queue,
    case queue:len(Queue) of
      0 ->
	  ok;
      N ->
	  ?DEBUG("~B stanza(s) were not acknowledged by ~s",
		 [N, jid:to_string(StateData#state.jid)]),
	  lists:foreach(
	    fun({_, Time, #xmlel{attrs = Attrs} = El}) ->
		    From_s = fxml:get_attr_s(<<"from">>, Attrs),
		    From = jid:from_string(From_s),
		    To_s = fxml:get_attr_s(<<"to">>, Attrs),
		    To = jid:from_string(To_s),
		    F(From, To, El, Time)
	    end, queue:to_list(Queue))
    end;
handle_unacked_stanzas(_StateData, _F) ->
    ok.

handle_unacked_stanzas(#state{mgmt_state = MgmtState} = StateData)
    when MgmtState == active;
	 MgmtState == pending;
	 MgmtState == timeout ->
    ResendOnTimeout =
	case StateData#state.mgmt_resend of
	  Resend when is_boolean(Resend) ->
	      Resend;
	  if_offline ->
	      Resource = StateData#state.resource,
	      case ejabberd_sm:get_user_resources(StateData#state.user,
						  StateData#state.server) of
		[Resource] -> % Same resource opened new session
		    true;
		[] ->
		    true;
		_ ->
		    false
	      end
	end,
    Lang = StateData#state.lang,
    ReRoute = case ResendOnTimeout of
		true ->
		    fun(From, To, El, Time) ->
			    NewEl = add_resent_delay_info(StateData, El, Time),
			    ejabberd_router:route(From, To, NewEl)
		    end;
		false ->
		    fun(From, To, El, _Time) ->
			    Txt = <<"User session terminated">>,
			    Err =
				jlib:make_error_reply(
				  El,
				  ?ERRT_SERVICE_UNAVAILABLE(Lang, Txt)),
			    ejabberd_router:route(To, From, Err)
		    end
	      end,
    F = fun(From, _To, #xmlel{name = <<"presence">>}, _Time) ->
		?DEBUG("Dropping presence stanza from ~s",
		       [jid:to_string(From)]);
	   (From, To, #xmlel{name = <<"iq">>} = El, _Time) ->
		Txt = <<"User session terminated">>,
		Err = jlib:make_error_reply(
			El, ?ERRT_SERVICE_UNAVAILABLE(Lang, Txt)),
		ejabberd_router:route(To, From, Err);
	   (From, To, El, Time) ->
		%% We'll drop the stanza if it was <forwarded/> by some
		%% encapsulating protocol as per XEP-0297.  One such protocol is
		%% XEP-0280, which says: "When a receiving server attempts to
		%% deliver a forked message, and that message bounces with an
		%% error for any reason, the receiving server MUST NOT forward
		%% that error back to the original sender."  Resending such a
		%% stanza could easily lead to unexpected results as well.
		case is_encapsulated_forward(El) of
		  true ->
		      ?DEBUG("Dropping forwarded message stanza from ~s",
			     [fxml:get_attr_s(<<"from">>, El#xmlel.attrs)]);
		  false ->
		      case ejabberd_hooks:run_fold(message_is_archived,
						   StateData#state.server,
						   false,
						   [StateData, From,
						    StateData#state.jid, El]) of
			true ->
			    ?DEBUG("Dropping archived message stanza from ~s",
				   [fxml:get_attr_s(<<"from">>,
						    El#xmlel.attrs)]),
			    ok;
			false ->
			    ReRoute(From, To, El, Time)
		      end
		end
	end,
    handle_unacked_stanzas(StateData, F);
handle_unacked_stanzas(_StateData) ->
    ok.

is_encapsulated_forward(#xmlel{name = <<"message">>} = El) ->
    SubTag = case {fxml:get_subtag(El, <<"sent">>),
		   fxml:get_subtag(El, <<"received">>),
		   fxml:get_subtag(El, <<"result">>)} of
	       {false, false, false} ->
		   false;
	       {Tag, false, false} ->
		   Tag;
	       {false, Tag, false} ->
		   Tag;
	       {_, _, Tag} ->
		   Tag
	    end,
    if SubTag == false ->
	   false;
       true ->
	   case fxml:get_subtag(SubTag, <<"forwarded">>) of
	     false ->
		 false;
	     _ ->
		 true
	   end
    end;
is_encapsulated_forward(_El) ->
    false.

inherit_session_state(#state{user = U, server = S} = StateData, ResumeID) ->
    case jlib:base64_to_term(ResumeID) of
      {term, {R, Time}} ->
	  case ejabberd_sm:get_session_pid(U, S, R) of
	    none ->
		case ejabberd_sm:get_offline_info(Time, U, S, R) of
		  none ->
		      {error, <<"Previous session PID not found">>};
		  Info ->
		      case proplists:get_value(num_stanzas_in, Info) of
			undefined ->
			    {error, <<"Previous session timed out">>};
			H ->
			    {error, <<"Previous session timed out">>, H}
		      end
		end;
	    OldPID ->
		OldSID = {Time, OldPID},
		case catch resume_session(OldSID) of
		  {resume, OldStateData} ->
		      NewSID = {Time, self()}, % Old time, new PID
		      Priority = case OldStateData#state.pres_last of
				   undefined ->
				       0;
				   Presence ->
				       get_priority_from_presence(Presence)
				 end,
		      Conn = get_conn_type(StateData),
		      Info = [{ip, StateData#state.ip}, {conn, Conn},
			      {auth_module, StateData#state.auth_module}],
		      ejabberd_sm:open_session(NewSID, U, S, R,
					       Priority, Info),
		      {ok, StateData#state{conn = Conn,
					   sid = NewSID,
					   jid = OldStateData#state.jid,
					   resource = OldStateData#state.resource,
					   pres_t = OldStateData#state.pres_t,
					   pres_f = OldStateData#state.pres_f,
					   pres_a = OldStateData#state.pres_a,
					   pres_last = OldStateData#state.pres_last,
					   pres_timestamp = OldStateData#state.pres_timestamp,
					   privacy_list = OldStateData#state.privacy_list,
					   aux_fields = OldStateData#state.aux_fields,
					   mgmt_xmlns = OldStateData#state.mgmt_xmlns,
					   mgmt_queue = OldStateData#state.mgmt_queue,
					   mgmt_timeout = OldStateData#state.mgmt_timeout,
					   mgmt_stanzas_in = OldStateData#state.mgmt_stanzas_in,
					   mgmt_stanzas_out = OldStateData#state.mgmt_stanzas_out,
					   mgmt_state = active,
					   csi_state = active}, Info};
		  {error, Msg} ->
		      {error, Msg};
		  _ ->
		      {error, <<"Cannot grab session state">>}
		end
	  end;
      _ ->
	  {error, <<"Invalid 'previd' value">>}
    end.

resume_session({Time, PID}) ->
    (?GEN_FSM):sync_send_all_state_event(PID, {resume_session, Time}, 15000).

make_resume_id(StateData) ->
    {Time, _} = StateData#state.sid,
    jlib:term_to_base64({StateData#state.resource, Time}).

add_resent_delay_info(_State, #xmlel{name = <<"iq">>} = El, _Time) ->
    El;
add_resent_delay_info(#state{server = From}, El, Time) ->
    jlib:add_delay_info(El, From, Time, <<"Resent">>).

%%%----------------------------------------------------------------------
%%% XEP-0352
%%%----------------------------------------------------------------------

csi_filter_stanza(#state{csi_state = CsiState, jid = JID, server = Server} =
		  StateData, Stanza) ->
    {StateData1, Stanzas} = ejabberd_hooks:run_fold(csi_filter_stanza, Server,
						    {StateData, [Stanza]},
						    [Server, JID, Stanza]),
    StateData2 = lists:foldl(fun(CurStanza, AccState) ->
				     send_stanza(AccState, CurStanza)
			     end, StateData1#state{csi_state = active},
			     Stanzas),
    StateData2#state{csi_state = CsiState}.

csi_flush_queue(#state{csi_state = CsiState, jid = JID, server = Server} =
		StateData) ->
    {StateData1, Stanzas} = ejabberd_hooks:run_fold(csi_flush_queue, Server,
						    {StateData, []},
						    [Server, JID]),
    StateData2 = lists:foldl(fun(CurStanza, AccState) ->
				     send_stanza(AccState, CurStanza)
			     end, StateData1#state{csi_state = active},
			     Stanzas),
    StateData2#state{csi_state = CsiState}.

%%%----------------------------------------------------------------------
%%% JID Set memory footprint reduction code
%%%----------------------------------------------------------------------

%% Try to reduce the heap footprint of the four presence sets
%% by ensuring that we re-use strings and Jids wherever possible.
pack(S = #state{pres_a = A, pres_f = F,
		pres_t = T}) ->
    {NewA, Pack2} = pack_jid_set(A, gb_trees:empty()),
    {NewF, Pack3} = pack_jid_set(F, Pack2),
    {NewT, _Pack4} = pack_jid_set(T, Pack3),
    S#state{pres_a = NewA, pres_f = NewF,
	    pres_t = NewT}.

pack_jid_set(Set, Pack) ->
    Jids = (?SETS):to_list(Set),
    {PackedJids, NewPack} = pack_jids(Jids, Pack, []),
    {(?SETS):from_list(PackedJids), NewPack}.

pack_jids([], Pack, Acc) -> {Acc, Pack};
pack_jids([{U, S, R} = Jid | Jids], Pack, Acc) ->
    case gb_trees:lookup(Jid, Pack) of
      {value, PackedJid} ->
	  pack_jids(Jids, Pack, [PackedJid | Acc]);
      none ->
	  {NewU, Pack1} = pack_string(U, Pack),
	  {NewS, Pack2} = pack_string(S, Pack1),
	  {NewR, Pack3} = pack_string(R, Pack2),
	  NewJid = {NewU, NewS, NewR},
	  NewPack = gb_trees:insert(NewJid, NewJid, Pack3),
	  pack_jids(Jids, NewPack, [NewJid | Acc])
    end.

pack_string(String, Pack) ->
    case gb_trees:lookup(String, Pack) of
      {value, PackedString} -> {PackedString, Pack};
      none -> {String, gb_trees:insert(String, String, Pack)}
    end.

transform_listen_option(Opt, Opts) ->
    [Opt|Opts].

identity(Props) ->
    case proplists:get_value(authzid, Props, <<>>) of
	<<>> -> proplists:get_value(username, Props, <<>>);
	AuthzId -> AuthzId
    end.

opt_type(domain_certfile) -> fun iolist_to_binary/1;
opt_type(max_fsm_queue) ->
    fun (I) when is_integer(I), I > 0 -> I end;
opt_type(resource_conflict) ->
    fun (setresource) -> setresource;
	(closeold) -> closeold;
	(closenew) -> closenew;
	(acceptnew) -> acceptnew
    end;
opt_type(_) ->
    [domain_certfile, max_fsm_queue, resource_conflict].

%%%%%%%%%%%%%%-----------------------------------------------------------
%%%%%%%%%%%%%% @date 2017-03
%%%%%%%%%%%%%% 记录用户的show_tag
%%%%%%%%%%%%%%-----------------------------------------------------------
record_user_show_tag(Packet,StateData) ->
    case catch qtalk_c2s:get_presence_show_tag(Packet) of
    <<"unknown">> -> ok;
    Show ->
       ejabberd_sm:record_show(StateData#state.user,
            StateData#state.server,StateData#state.resource, Show)
    end.            


make_new_presence_packet(LServer,From,Packet,_Attrs) ->
    Num = mod_user_relation:get_users_friend_num(LServer,From#jid.luser, From#jid.lserver),
        fxml:replace_tag_attr(<<"friend_num">>, http_utils:to_binary(Num,<<"0">>), Packet).



send_probuf_msg(StateData, Packet) ->
    To = qtalk_public:get_xml_attrs_to(Packet,{StateData#state.user,StateData#state.server,StateData#state.resource}),
    From = qtalk_public:get_xml_attrs_from(Packet,{StateData#state.user,StateData#state.server,StateData#state.resource}),
    catch do_send_probuf_msg(StateData,ejabberd_pb2xml_public:list_and_character_to_binary(From),
                                ejabberd_pb2xml_public:list_and_character_to_binary(To), Packet).

do_send_probuf_msg(StateData,From,To, Packet = #xmlel{name = <<"iq">>}) ->
   ?DEBUG("PB_IQ ~p,~p  ~n",[Packet,StateData]),
    PB_IQ = encode_iq_pb_packet(StateData,From,To,Packet),
   ?DEBUG("PB_IQ ~p,~p  ~n",[PB_IQ,Packet]),
    case PB_IQ of
    <<"error">> ->
        ok;
    _ ->
        Text = ejabberd_encode_protobuf:uint32_pack(byte_size(PB_IQ),PB_IQ),
        ?DEBUG("PB_IQ ~p ,Text ~p ~n",[PB_IQ,Text]),
        send_text(StateData, Text)
    end;
do_send_probuf_msg(StateData,From,To, Packet = #xmlel{name = <<"message">>}) ->
    PB_MSG = ejabberd_xml2pb_message:xml2pb_msg(From,To,Packet),
    case PB_MSG of
    <<"">> ->
        ?INFO_MSG("PB_MSG ~p ~n",[Packet]);
    _ ->
        ok
    end,
    Text =
        case catch ejabberd_encode_protobuf:uint32_pack(byte_size(PB_MSG),PB_MSG) of
        I  when is_binary(I) ->
            I;
        V ->
            ?DEBUG("Paket ~p ~n",[Packet]),
            V
        end,
    send_text(StateData, Text);
do_send_probuf_msg(StateData,From,To, Packet = #xmlel{name = <<"presence">>}) ->
	?DEBUG("Packet ~p ~n",[Packet]),
    PB_PRESENCE =
        case catch fxml:get_attr_s(<<"xmlns">>,Packet#xmlel.attrs) of
        <<"http://jabber.org/protocol/muc#invite">> ->
                ejabberd_xml2pb_presence:encode_presence_invite_muc(From,To,Packet);
        <<"http://jabber.org/protocol/muc#del_register">> ->
                ejabberd_xml2pb_presence:encode_del_muc_register(From,To,Packet);
        <<"http://jabber.org/protocol/muc#muc_user_subscribe_v2">> ->
                ejabberd_xml2pb_presence:encode_set_user_subscribe_v2(From,To,Packet);
        <<"http://jabber.org/protocol/muc#vcard_update">> ->
                ejabberd_xml2pb_presence:encode_update_muc_vcard(From,To,Packet);
        <<"jabber:x:verify_friend">> ->
                case proplists:get_value(<<"result">>,Packet#xmlel.attrs) of
                undefined ->
                    case proplists:get_value(<<"method">>,Packet#xmlel.attrs) of
                    <<"manual_authentication_confirm">> ->
                        ejabberd_xml2pb_presence:encode_manual_authentication_confirm(From,To,Packet);
                    _ ->
                        <<"error">>
                    end;
                _ ->
                    ejabberd_xml2pb_presence:encode_verify_friend(From,To,Packet)
                end;
        <<"http://jabber.org/protocol/user#invite_rslt">> ->
                case proplists:get_value(<<"result">>,Packet#xmlel.attrs) of
                undefined ->
                    <<"error">>;
                _ ->
                    ejabberd_xml2pb_presence:encode_verify_friend(From,To,Packet)
                end;
        <<"jabber:x:delete_friend">> ->
                ejabberd_xml2pb_presence:encode_delete_friend(From,To,Packet);
        <<"jabber:x:mask_user">> ->
                ejabberd_xml2pb_presence:encode_presence_mask_user(From,To,Packet);
		<<"http://jabber.org/protocol/muc#muc_forbidden_words">> ->
				ejabberd_xml2pb_presence:encode_presence_forbidden_words(From,To,Packet);
        <<"">> ->
            case catch qtalk_public:get_sub_xmlns_name(Packet) of
            {<<"x">>,<<"http://jabber.org/protocol/muc#user">>} ->
                case catch  fxml:get_attr(<<"type">>, Packet#xmlel.attrs) of
                <<"unavailable">> ->
                    ok;
                _ ->
                    ejabberd_xml2pb_presence:encode_x_user_packet(From,To,Packet)
                end;
            {<<"query">>,<<"http://jabber.org/protocol/muc#owner">>} ->
                ejabberd_xml2pb_presence:encode_encode_update_muc_vcard(From,To,Packet);
            {<<"notify">>, <<"jabber:x:presence_notify">>} ->
                ejabberd_xml2pb_presence:encode_notify_presence(From,To,Packet);
            _ ->
                case catch fxml:get_subtag(Packet,<<"show">>) of
                false ->
                    <<"error">>;
                _ ->
                    ejabberd_xml2pb_presence:enocde_status(From,To,Packet)
                end
            end;
        _ ->
            <<"error">>
        end,

    case PB_PRESENCE of
    <<"error">> ->
        ok;
    _ ->
        Text = ejabberd_encode_protobuf:uint32_pack(byte_size(PB_PRESENCE),PB_PRESENCE),
        ?DEBUG("PB_PRESENCE ~p ,Text ~p ~n",[PB_PRESENCE,Text]),
        send_text(StateData, Text)
    end;
do_send_probuf_msg(_StateData, _From, _To, Packet) ->
    ?ERROR_MSG("Packet drop ~p ~n",[Packet]).

send_welcome_msg(StateData,User,Server,Version,SockMod) ->
    From = jlib:jid_to_string({User,Server,<<"">>}),
    PBMsg = ejabberd_encode_protobuf:struct_pb_welcome(From,From,Server,Version,User,SockMod),
    Text = ejabberd_encode_protobuf:uint32_pack(byte_size(PBMsg),PBMsg),
    send_text(StateData, Text).

send_startTLS(_StateData, User, Server) ->
    From = jlib:jid_to_string({User,Server,<<"">>}),
    PBMsg = ejabberd_encode_protobuf:struct_pb_startTLS(From,From),
    Text = ejabberd_encode_protobuf:uint32_pack(byte_size(PBMsg),PBMsg),
    Text.

send_stream_end(StateData,Code,Reason) ->
    catch ?ERROR_MSG("the send stream end, the code is ~p, the reason is ~p, username is ~p~n", [Code, Reason, jid:to_string(StateData#state.jid)]),
    End_Msg = ejabberd_encode_protobuf:struct_pb_streamend(<<"">>,<<"">>,Code,Reason),
    Text = ejabberd_encode_protobuf:uint32_pack(byte_size(End_Msg),End_Msg),
    send_text(StateData, Text).

send_auth_login_response_sucess(StateData,User,Server,Msg_ID,INFO) ->
    From = jlib:jid_to_string({User,Server,<<"">>}),
    Auth_Msg = ejabberd_encode_protobuf:struct_pb_response_suc(From,From,0,Msg_ID,INFO,<<"login sucess">>),
    Text = ejabberd_encode_protobuf:uint32_pack(byte_size(Auth_Msg),Auth_Msg),
        ?DEBUG("Text login  auth ~p, ~p ,~p  ~n",[Text,StateData#state.sockmod,StateData#state.socket]),
    send_text(StateData, Text).

send_auth_login_response_failed(StateData,User,Server,Msg_ID,INFO) ->
    From = jlib:jid_to_string({User,Server,<<"">>}),
    Auth_Msg = ejabberd_encode_protobuf:struct_pb_response_err(From,From,0,Msg_ID,INFO,<<"login failed">>),
    Text = ejabberd_encode_protobuf:uint32_pack(byte_size(Auth_Msg),Auth_Msg),
        ?DEBUG("Text login  auth ~p ,~p ,~p ~n",[Text,StateData#state.sockmod,StateData#state.socket]),
    send_text(StateData, Text).

encode_iq_pb_packet(StateData,From,To, Packet) ->
	?DEBUG("Packet ~p ~n",[Packet]),
    case fxml:get_attr(<<"type">>, Packet#xmlel.attrs) of
    {value,<<"error">>} ->
        encode_iq_error_pb_packet(From,To,Packet);
    _ ->
        encode_iq_result_pb_packet(StateData,From,To,Packet)
    end.


encode_iq_error_pb_packet(From,To,Packet) ->
    ejabberd_xml2pb_iq:encode_pb_error_iq(From,To,Packet).

encode_iq_result_pb_packet(StateData,From,To, Packet) ->
    case catch qtalk_public:get_sub_xmlns_name(Packet) of
    {<<"bind">>,<<"urn:ietf:params:xml:ns:xmpp-bind">>} ->
        ejabberd_xml2pb_iq:encode_pb_iq_bind_result(From,To,Packet,StateData#state.key);
    {<<"query">>,<<"http://jabber.org/protocol/muc#user_mucs">>} ->
        ejabberd_xml2pb_iq:encode_user_muc_pb(From,To,Packet);
    {<<"query">>,<<"http://jabber.org/protocol/create_muc">>} ->
        ejabberd_xml2pb_iq:encode_pb_iq_create_muc(From,To,Packet);
    {<<"query">>,<<"http://jabber.org/protocol/muc#invite_v2">>} ->
        ejabberd_xml2pb_iq:encode_muc_invite_user_v2_pb(From,To,Packet);
    {<<"query">>,<<"http://jabber.org/protocol/muc#register">>} ->
        case fxml:get_subtag(Packet,<<"query">>) of
        false ->
            <<"error">>;
        Query ->
            case fxml:get_subtags(Query,<<"m_user">>) of
            false ->
                ejabberd_xml2pb_iq:encode_pb_muc_user_register(From,To,Packet);
            _ ->
                ejabberd_xml2pb_iq:encode_muc_user_pb(From,To,Packet)
            end
        end;
    {<<"query">>,<<"http://jabber.org/protocol/muc#del_register">>} ->
        ejabberd_xml2pb_iq:encode_pb_muc_user_del_register(From,To,Packet);
    {<<"query">>,<<"http://jabber.org/protocol/muc#admin">>} ->
        ejabberd_xml2pb_iq:encode_pb_muc_amdin(From,To,Packet);
    {<<"query">>,<<"http://jabber.org/protocol/muc#owner">>} ->
        ejabberd_xml2pb_iq:encode_pb_destroy_muc(From,To,Packet);
    {<<"query">>, <<"http://jabber.org/protocol/muc#muc_forbidden_words">>} ->
        ejabberd_xml2pb_iq:encode_pb_forbidden_words(From,To,Packet);
    {<<"get_verify_friend_mode">>,<<"jabber:iq:verify_friend_mode">>} ->
        ejabberd_xml2pb_iq:encode_pb_get_friend_opt(From,To,Packet);
    {<<"set_verify_friend_mode">>,<<"jabber:iq:verify_friend_mode">>} ->
        ejabberd_xml2pb_iq:encode_pb_set_friend_opt(From,To,Packet);
    {<<"get_user_friends">>,<<"jabber:x:get_friend">>} ->
        ejabberd_xml2pb_iq:encode_pb_get_user_friends(From,To,Packet);
    {<<"delete_friend">>,<<"jabber:x:delete_friend">>} ->
        ejabberd_xml2pb_iq:encode_pb_del_user_friend(From,To,Packet);
    {<<"key">>,<<"urn:xmpp:key">>} ->
        ejabberd_xml2pb_iq:encode_pb_time_http_key(From,To,Packet);
    {<<"query">>,<<"jabber:x:mask_user_v2">>} ->
        ejabberd_xml2pb_iq:encode_pb_get_mask_user(From,To,Packet);
    {<<"query">>, <<"http://jabber.org/protocol/muc#muc_user_subscribe">>} ->
        ejabberd_xml2pb_iq:encode_pb_handle_user_subscribe(From,To,Packet);
    {<<"query">>, <<"http://jabber.org/protocol/muc#muc_user_subscribe_v2">>} ->
        ejabberd_xml2pb_iq:encode_pb_handle_user_subscribe_v2(From,To,Packet);
    {<<"mask_user">>,<<"jabber:x:mask_user">>} ->
        ejabberd_xml2pb_iq:encode_pb_set_mask_user(From,To,Packet);
    {<<"cancel_mask_user">>,<<"jabber:x:mask_user">>} ->
        ejabberd_xml2pb_iq:encode_pb_cancel_mask_user(From,To,Packet);
    {<<"virtual_user">>,<<"jabber:x:virtual_user">>} ->
        ejabberd_xml2pb_iq:encode_pb_get_virtual_user(From,To,Packet);
    {<<"on_duty_virtual_user">>,<<"jabber:x:virtual_user">>} ->
        ejabberd_xml2pb_iq:encode_pb_get_vuser_role(From,To,Packet);
    {<<"start_session">>,<<"jabber:x:virtual_user">>} ->
        ejabberd_xml2pb_iq:encode_pb_start_session(From,To,Packet);
    {<<"end_session">>,<<"jabber:x:virtual_user">>} ->
        ejabberd_xml2pb_iq:encode_pb_end_session(From,To,Packet);
    _ ->
        case fxml:get_attr(<<"type">>, Packet#xmlel.attrs) of
        {value,<<"result">>} ->
             if Packet#xmlel.children =:= [] ->
                 ejabberd_xml2pb_iq:encode_pb_ping(From,To,Packet);
             true ->
                 <<"error">>
             end;
         _ ->
             <<"error">>
         end
    end.
