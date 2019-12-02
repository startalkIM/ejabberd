%%%----------------------------------------------------------------------
%%% File    : ejabberd_receiver.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Socket receiver for C2S and S2S connections
%%% Created : 10 Nov 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_protobuf_receiver).

-author('alexey@process-one.net').

-behaviour(gen_server).

%% API
-export([start_link/4,
	 start/3,
	 start/4,
	 change_shaper/2,
	 reset_stream/1,
	 starttls/2,
	 compress/2,
	 become_controller/2,
	 close/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("message_pb.hrl").
-include("jlib.hrl").

-record(state,
	{socket :: inet:socket() | fast_tls:tls_socket() | ezlib:zlib_socket(),
         sock_mod = gen_tcp :: gen_tcp | fast_tls | ezlib,
         shaper_state = none :: shaper:shaper(),
         c2s_pid :: pid(),
	 max_stanza_size = infinity :: non_neg_integer() | infinity,
         xml_stream_state :: fxml_stream:xml_stream_state(),
         left = false,
         last_data = 0,
         last_data_size = 0, 
         head = true,
         data_size = 0,
         data = 0,
         timeout = infinity:: timeout()}).

-define(HIBERNATE_TIMEOUT, ejabberd_config:get_option(receiver_hibernate, fun(X) when is_integer(X); X == hibernate-> X end, 90000)).


-spec start_link(inet:socket(), atom(), shaper:shaper(),
                 non_neg_integer() | infinity) -> ignore |
                                                  {error, any()} |
                                                  {ok, pid()}.

start_link(Socket, SockMod, Shaper, MaxStanzaSize) ->
    gen_server:start_link(?MODULE,
			  [Socket, SockMod, Shaper, MaxStanzaSize], []).

-spec start(inet:socket(), atom(), shaper:shaper()) -> undefined | pid().

start(Socket, SockMod, Shaper) ->
    start(Socket, SockMod, Shaper, infinity).

-spec start(inet:socket(), atom(), shaper:shaper(),
            non_neg_integer() | infinity) -> undefined | pid().

start(Socket, SockMod, Shaper, MaxStanzaSize) ->
    {ok, Pid} = gen_server:start(ejabberd_protobuf_receiver,
				 [Socket, SockMod, Shaper, MaxStanzaSize], []),
    Pid.

-spec change_shaper(pid(), shaper:shaper()) -> ok.

change_shaper(Pid, Shaper) ->
    gen_server:cast(Pid, {change_shaper, Shaper}).

-spec reset_stream(pid()) -> ok | {error, any()}.

reset_stream(Pid) -> do_call(Pid, reset_stream).

-spec starttls(pid(), fast_tls:tls_socket()) -> ok.

starttls(Pid, TLSSocket) ->
    do_call(Pid, {starttls, TLSSocket}).

-spec compress(pid(), iodata() | undefined) -> {error, any()} |
                                               {ok, ezlib:zlib_socket()}.

compress(Pid, Data) ->
    do_call(Pid, {compress, Data}).

-spec become_controller(pid(), pid()) -> ok | {error, any()}.

become_controller(Pid, C2SPid) ->
    do_call(Pid, {become_controller, C2SPid}).

-spec close(pid()) -> ok.

close(Pid) ->
    gen_server:cast(Pid, close).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Socket, SockMod, Shaper, MaxStanzaSize]) ->
    ShaperState = shaper:new(Shaper),
    Timeout = case SockMod of
		ssl -> 20;
		_ -> infinity
	      end,
    {ok,
     #state{socket = Socket, sock_mod = SockMod,
	    shaper_state = ShaperState,
	    max_stanza_size = MaxStanzaSize, timeout = Timeout}}.

handle_call({starttls, TLSSocket}, _From, State) ->
    State1 = reset_parser(State),
    NewState = State1#state{socket = TLSSocket,
                            sock_mod = fast_tls},
    case fast_tls:recv_data(TLSSocket, <<"">>) of
	{ok, TLSData} ->
	    {reply, ok,
	%%	process_data(TLSData, NewState), ?HIBERNATE_TIMEOUT};
		process_recv_data(TLSData, NewState), ?HIBERNATE_TIMEOUT};
	{error, _Reason} ->
	    {stop, normal, ok, NewState}
    end;
handle_call({compress, Data}, _From,
	    #state{socket = Socket, sock_mod = SockMod} =
		State) ->
    ejabberd:start_app(ezlib),
    {ok, ZlibSocket} = ezlib:enable_zlib(SockMod,
						 Socket),
    if Data /= undefined -> do_send(State, Data);
       true -> ok
    end,
    State1 = reset_parser(State),
    NewState = State1#state{socket = ZlibSocket,
			   sock_mod = ezlib},
    case ezlib:recv_data(ZlibSocket, <<"">>) of
      {ok, ZlibData} ->
	    {reply, {ok, ZlibSocket},
		process_data(ZlibData, NewState), ?HIBERNATE_TIMEOUT};
      {error, _Reason} ->
	    {stop, normal, ok, NewState}
    end;
handle_call(reset_stream, _From, State) ->
    NewState = reset_parser(State),
    Reply = ok,
    {reply, Reply, NewState, ?HIBERNATE_TIMEOUT};
handle_call({become_controller, C2SPid}, _From, State) ->
    XMLStreamState = fxml_stream:new(C2SPid, State#state.max_stanza_size),
    NewState = State#state{c2s_pid = C2SPid,
			   xml_stream_state = XMLStreamState},
    activate_socket(NewState),
    Reply = ok,
    {reply, Reply, NewState, ?HIBERNATE_TIMEOUT};
handle_call(_Request, _From, State) ->
    Reply = ok, {reply, Reply, State, ?HIBERNATE_TIMEOUT}.

handle_cast({change_shaper, Shaper}, State) ->
    NewShaperState = shaper:new(Shaper),
    {noreply, State#state{shaper_state = NewShaperState},
     ?HIBERNATE_TIMEOUT};
handle_cast(close, State) -> {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State, ?HIBERNATE_TIMEOUT}.

handle_info({Tag, _TCPSocket, Data},
	    #state{socket = Socket, sock_mod = SockMod} = State)
    when (Tag == tcp) or (Tag == ssl) or
	   (Tag == ejabberd_xml) ->
    case SockMod of
      fast_tls ->
	  case fast_tls:recv_data(Socket, Data) of
	    {ok, TLSData} ->
%%		{noreply, process_data(TLSData, State),
		{noreply, process_recv_data(TLSData, State),
		 ?HIBERNATE_TIMEOUT};
	    {error, Reason} ->
		  if is_binary(Reason) ->
			  ?DEBUG("TLS error = ~s", [Reason]);
		     true ->
			  ok
		  end,
		  {stop, normal, State}
	  end;
      ezlib ->
	  case ezlib:recv_data(Socket, Data) of
	    {ok, ZlibData} ->
%%		{noreply, process_data(ZlibData, State),
		{noreply, process_recv_data(ZlibData, State),
		 ?HIBERNATE_TIMEOUT};
	    {error, _Reason} -> {stop, normal, State}
	  end;
      _ ->
%	  {noreply, process_data(Data, State), ?HIBERNATE_TIMEOUT}
	  {noreply, process_recv_data(Data, State), ?HIBERNATE_TIMEOUT}
    end;
handle_info({Tag, _TCPSocket}, State)
    when (Tag == tcp_closed) or (Tag == ssl_closed) ->
    {stop, normal, State};
handle_info({Tag, _TCPSocket, Reason}, State)
    when (Tag == tcp_error) or (Tag == ssl_error) ->
    case Reason of
      timeout -> {noreply, State, ?HIBERNATE_TIMEOUT};
      _ -> {stop, normal, State}
    end;
handle_info({timeout, _Ref, activate}, State) ->
    activate_socket(State),
    {noreply, State, ?HIBERNATE_TIMEOUT};
handle_info(timeout, State) ->
    proc_lib:hibernate(gen_server, enter_loop,
		       [?MODULE, [], State]),
    {noreply, State, ?HIBERNATE_TIMEOUT};
handle_info(_Info, State) ->
    {noreply, State, ?HIBERNATE_TIMEOUT}.

terminate(_Reason,
	  #state{xml_stream_state = XMLStreamState,
		 c2s_pid = C2SPid} =
	      State) ->
    close_stream(XMLStreamState),
    if C2SPid /= undefined ->
	   gen_fsm:send_event(C2SPid, closed);
       true -> ok
    end,
    catch (State#state.sock_mod):close(State#state.socket),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

activate_socket(#state{socket = Socket,
		       sock_mod = SockMod}) ->
    PeerName = case SockMod of
		 gen_tcp ->
		     inet:setopts(Socket, [{active, once}]),
		     inet:peername(Socket);
		 _ ->
		     SockMod:setopts(Socket, [{active, once}]),
		     SockMod:peername(Socket)
	       end,
    case PeerName of
      {error, _Reason} -> self() ! {tcp_closed, Socket};
      {ok, _} -> ok
    end.

%% Data processing for connectors directly generating xmlelement in
%% Erlang data structure.
%% WARNING: Shaper does not work with Erlang data structure.
process_data([], State) ->
    activate_socket(State), State;
process_data([Element | Els],
	     #state{c2s_pid = C2SPid} = State)
    when element(1, Element) == xmlel;
	 element(1, Element) == xmlstreamstart;
	 element(1, Element) == xmlstreamelement;
	 element(1, Element) == xmlstreamend ->
    if C2SPid == undefined -> State;
       true ->
	   catch gen_fsm:send_event(C2SPid,
				    element_wrapper(Element)),
	   process_data(Els, State)
    end;
%% Data processing for connectors receivind data as string.
process_data(Data,
	     #state{xml_stream_state = XMLStreamState,
		    shaper_state = ShaperState, c2s_pid = C2SPid} =
		 State) ->
    ?DEBUG("Received XML on stream = ~p", [(Data)]),
    XMLStreamState1 = case XMLStreamState of
                          undefined ->
                              XMLStreamState;
                          _ ->
                              fxml_stream:parse(XMLStreamState, Data)
                      end,
    {NewShaperState, Pause} = shaper:update(ShaperState, byte_size(Data)),
    if
	C2SPid == undefined ->
	    ok;
	Pause > 0 ->
	    erlang:start_timer(Pause, self(), activate);
	true ->
	    activate_socket(State)
    end,
    State#state{xml_stream_state = XMLStreamState1,
		shaper_state = NewShaperState}.

%% Element coming from XML parser are wrapped inside xmlstreamelement
%% When we receive directly xmlelement tuple (from a socket module
%% speaking directly Erlang XML), we wrap it inside the same
%% xmlstreamelement coming from the XML parser.
element_wrapper(XMLElement)
    when element(1, XMLElement) == xmlel ->
    {xmlstreamelement, XMLElement};
element_wrapper(Element) -> Element.

close_stream(undefined) -> ok;
close_stream(XMLStreamState) ->
    fxml_stream:close(XMLStreamState).

reset_parser(#state{xml_stream_state = undefined} = State) ->
    State;
reset_parser(#state{c2s_pid = C2SPid,
                    max_stanza_size = MaxStanzaSize,
                    xml_stream_state = XMLStreamState}
             = State) ->
    NewStreamState = try fxml_stream:reset(XMLStreamState)
                     catch error:_ ->
                             close_stream(XMLStreamState),
                             case C2SPid of
                                 undefined ->
                                     undefined;
                                 _ ->
                                     fxml_stream:new(C2SPid, MaxStanzaSize)
                             end
                     end,
    State#state{xml_stream_state = NewStreamState}.

do_send(State, Data) ->
    (State#state.sock_mod):send(State#state.socket, Data).

do_call(Pid, Msg) ->
    case catch gen_server:call(Pid, Msg) of
      {'EXIT', Why} -> {error, Why};
      Res -> Res
    end.

process_recv_data(Data,State) ->
    FullData = case State#state.left of
        true -> <<(State#state.last_data)/binary,Data/binary>>;
        _ -> Data
    end,
    parse_recv_data(FullData,State).

parse_recv_data(Data, State) ->
    case process_data_len(Data) of
        false ->
	    self() ! {tcp_closed, State#state.socket};
	continue ->
            activate_socket(State),
	    State#state{left = true,last_data = Data};
	{Len, RData} ->
	    RDataSize = byte_size(RData),
	    if Len > RDataSize ->
                    activate_socket(State),
		    State#state{left = true,last_data = Data};
	       Len == RDataSize ->
                    decode_pb_message(RData, State),
                    activate_socket(State),
		    State#state{left = false,last_data = <<>>};
	       true ->
                    <<RRData:Len/binary, R/binary>> = RData,
		    decode_pb_message(RRData, State),
		    parse_recv_data(R, State#state{left = true,last_data = R})
            end
    end.

process_data_len(Data) ->
    process_data_len(Data, 0, 0).

%% process_data_len(Data, Length, Weight, Count)
%% Data: 处理的数据
%% Length：累加的长度
%% Count: 表示长度的字节数
process_data_len(_, _, 4) ->
    false;
process_data_len(<<>>, _, _) ->
    continue;
process_data_len(<<0:1, H:7, R/binary>>, Base, Count) ->
    {Base + (H bsl (Count * 7)), R};
process_data_len(<<1:1, H:7, R/binary>>, Base, Count) ->
    process_data_len(R, Base + (H bsl (Count * 7)), Count + 1).

handle_pro_msg(Signaltype,Proto_msg,State) ->
        do_handle_pro_msg(message_pb:int_to_enum(signaltype,Signaltype),Proto_msg,State).

do_handle_pro_msg('SignalTypeWelcome',Proto_msg, #state{c2s_pid = C2SPid})->
        Message  = Proto_msg#protomessage.message,
        Welcome = message_pb:decode_welcomemessage(Message),
        ?DEBUG("welconMsg ~p ~n",[Welcome]),
        El = make_welcome_xml(Welcome),
        catch gen_fsm:send_event(C2SPid,  {xmlstreamstart, <<"">>, El#xmlel.attrs});
do_handle_pro_msg('SignalStartTLS',Proto_msg, #state{c2s_pid = C2SPid})->
    Message  = Proto_msg#protomessage.message,
    StartTls =  message_pb:decode_starttls(Message),
    ?DEBUG("startTls ~p ~n",[StartTls]),
        El = make_startTls_xml(StartTls),
        catch gen_fsm:send_event(C2SPid,  {xmlstreamelement, El});
do_handle_pro_msg('SignalTypeAuth',Proto_msg, #state{c2s_pid = C2SPid})->
        Message  = Proto_msg#protomessage.message,
        AuthMsg = message_pb:decode_authmessage(Message),
        ?DEBUG("AuthMsg ~p ~n",[AuthMsg]),
        El = make_auth_xml(AuthMsg),
        catch gen_fsm:send_event(C2SPid,  {xmlstreamelement, El});
do_handle_pro_msg('SignalTypeIQ',Proto_msg, #state{c2s_pid = C2SPid})->
        ?DEBUG("Proto_msg ~p ~n",[Proto_msg]),
        case catch ejabberd_pb2xml_iq:parse_iq_message(Proto_msg) of
        {Stream,Packet}  when is_record(Packet,xmlel) ->
                ?DEBUG("IQ packet~p, ~p ~n",[Stream,Packet]),
                catch gen_fsm:send_event(C2SPid,  {Stream, Packet});
        _ ->
                ok
        end;
do_handle_pro_msg('SignalTypePresence',Proto_msg, #state{c2s_pid = C2SPid})->
        ?DEBUG("Presence Packet ~p ~n",[Proto_msg]),
        case catch ejabberd_pb2xml_presence:parse_presence_message(Proto_msg) of
        {Stream,Packet}  when is_record(Packet,xmlel) ->
                ?DEBUG("Presence packet ~p ~n",[Packet]),
                catch gen_fsm:send_event(C2SPid,  {Stream, Packet});
        _ ->
                ok
        end;
do_handle_pro_msg('SignalTypeChat',Proto_msg, #state{c2s_pid = C2SPid})->
        ?DEBUG("chat message ~p ~n",[Proto_msg]),
        case catch ejabberd_pb2xml_message:parse_xmpp_message(Proto_msg) of
        {Stream,Packet} when is_record(Packet,xmlel) ->
                ?DEBUG("xmlel ~p ~n",[Packet]),
                catch gen_fsm:send_event(C2SPid,{Stream,Packet});
        _ ->
                ok
        end;
do_handle_pro_msg('SignalTypeGroupChat',Proto_msg, #state{c2s_pid = C2SPid})->
        ?DEBUG("chat message ~p ~n",[Proto_msg]),
        case catch ejabberd_pb2xml_message:parse_xmpp_message(Proto_msg) of
        {Stream,Packet} when is_record(Packet,xmlel) ->
                ?DEBUG("xmlel ~p ~n",[Packet]),
                catch gen_fsm:send_event(C2SPid,{Stream,Packet});
        _ ->
                ok
        end;
do_handle_pro_msg('SignalTypeSubscription',Proto_msg, #state{c2s_pid = C2SPid})->
        ?DEBUG("chat message ~p ~n",[Proto_msg]),
        case catch ejabberd_pb2xml_message:parse_xmpp_message(Proto_msg) of
        {Stream,Packet} when is_record(Packet,xmlel) ->
                ?DEBUG("xmlel ~p ~n",[Packet]),
                catch gen_fsm:send_event(C2SPid,{Stream,Packet});
        _ ->
                ok
        end;
do_handle_pro_msg('SignalTypeTyping',Proto_msg, #state{c2s_pid = C2SPid})->
        ?DEBUG("SignalTypeTyping message ~p ~n",[Proto_msg]),
        case catch ejabberd_pb2xml_message:parse_xmpp_message(Proto_msg) of
        {Stream,Packet} when is_record(Packet,xmlel) ->
                ?DEBUG("xmlel ~p ~n",[Packet]),
                catch gen_fsm:send_event(C2SPid,{Stream,Packet});
        _ ->
                ok
        end;
do_handle_pro_msg('SignalTypeHeadline',Proto_msg, #state{c2s_pid = C2SPid})->
        ?DEBUG("chat message ~p ~n",[Proto_msg]),
        case catch ejabberd_pb2xml_message:parse_xmpp_message(Proto_msg) of
        {Stream,Packet} when is_record(Packet,xmlel) ->
                ?DEBUG("xmlel ~p ~n",[Packet]),
                catch gen_fsm:send_event(C2SPid,{Stream,Packet});
        _ ->
                ok
        end;
do_handle_pro_msg('SignalTypeWebRtc',Proto_msg, #state{c2s_pid = C2SPid})->
        ?INFO_MSG("chat message ~p ~n",[Proto_msg]),
        case catch ejabberd_pb2xml_message:parse_xmpp_message(Proto_msg) of
        {Stream,Packet} when is_record(Packet,xmlel) ->
                ?DEBUG("xmlel ~p ~n",[Packet]),
                catch gen_fsm:send_event(C2SPid,{Stream,Packet});
        _ ->
                ok
        end;
do_handle_pro_msg('SignalTypeReadmark',Proto_msg, #state{c2s_pid = C2SPid})->
        ?DEBUG("readmark message ~p ~n",[Proto_msg]),
        case catch ejabberd_pb2xml_message:parse_xmpp_message(Proto_msg) of
        {Stream,Packet} when is_record(Packet,xmlel) ->
                ?DEBUG("xmlel ~p ~n",[Packet]),
                catch gen_fsm:send_event(C2SPid,{Stream,Packet});
        _ ->
                ok
        end;
do_handle_pro_msg('SignalTypeRevoke',Proto_msg, #state{c2s_pid = C2SPid})->
        ?DEBUG("chat message ~p ~n",[Proto_msg]),
        case catch ejabberd_pb2xml_message:parse_xmpp_message(Proto_msg) of
        {Stream,Packet} when is_record(Packet,xmlel) ->
                ?DEBUG("xmlel ~p ~n",[Packet]),
                catch gen_fsm:send_event(C2SPid,{Stream,Packet});
        _ ->
                ok
        end;
do_handle_pro_msg('SignalTypeConsult',Proto_msg, #state{c2s_pid = C2SPid})->
        ?DEBUG("chat message ~p ~n",[Proto_msg]),
        case catch ejabberd_pb2xml_message:parse_xmpp_message(Proto_msg) of
        {Stream,Packet} when is_record(Packet,xmlel) ->
                ?DEBUG("xmlel ~p ~n",[Packet]),
                catch gen_fsm:send_event(C2SPid,{Stream,Packet});
        _ ->
                ok
        end;
do_handle_pro_msg('SignalTypeEncryption',Proto_msg, #state{c2s_pid = C2SPid})->
        ?DEBUG("chat message ~p ~n",[Proto_msg]),
        case catch ejabberd_pb2xml_message:parse_xmpp_message(Proto_msg) of
        {Stream,Packet} when is_record(Packet,xmlel) ->
                ?DEBUG("xmlel ~p ~n",[Packet]),
                catch gen_fsm:send_event(C2SPid,{Stream,Packet});
        _ ->
                ok
        end;
do_handle_pro_msg(_,Pro_msg,_State) ->
        ?DEBUG("unkown Type ~p ~n",[Pro_msg]).

make_welcome_xml(Welcome) ->
        Server = iolist_to_binary(Welcome#welcomemessage.domain),
        Version = iolist_to_binary(Welcome#welcomemessage.version),
        User = iolist_to_binary(Welcome#welcomemessage.user),
        #xmlel{name = <<"">>,
                   attrs =
                        [{<<"xmlns">>,<<"jabber:client">>}, {<<"xmlns:stream">>,<<"http://etherx.jabber.org/streams">>},
                         {<<"to">>,Server},     {<<"version">>,Version},{<<"id">>,<<"3754522579">>},{<<"user">>,User},
                         {<<"from">>,Server},{<<"xml:lang">>,<<"en">>}],
                   children = []}.

make_startTls_xml(_StartTls) ->
    #xmlel{name = <<"starttls">>,
       attrs = [{<<"xmlns">>,
                 <<"urn:ietf:params:xml:ns:xmpp-tls">>}],
       children = []}.

make_auth_xml(Auth) ->
        Mechanism = iolist_to_binary(Auth#authmessage.mechanism),
        %%Method =  Auth#authmessage.method,
        Authkey = iolist_to_binary(Auth#authmessage.authkey),
        %%OhterBody = Auth#authmessage.otherbody,
        ID = case Auth#authmessage.msgid of
                undefined ->
                        list_to_binary("PBMSG_" ++ integer_to_list(random:uniform(65536)) ++ integer_to_list(qtalk_public:get_exact_timestamp()));
                I ->
                        iolist_to_binary(I)
                end,

        #xmlel{name = <<"auth">>,
                   attrs = [{<<"xmlns">>,<<"urn:ietf:params:xml:ns:xmpp-sasl">>},
                                        {<<"mechanism">>,Mechanism},
                                        {<<"id">>,ID}],
                   children = [{xmlcdata,Authkey}]}.

decode_pb_message(Dt,State) ->
                ?DEBUG("Dt ~p ~n",[Dt]),
        case ejabberd_pb2xml_public:get_potoheader_base_pb(Dt) of
        false ->
                ok;
        Pro_Header ->
                case ejabberd_pb2xml_public:get_potomessage_base_pbheader(Pro_Header) of
                false ->
                        ok;
                Pro_Msg  ->
                        handle_pro_msg(Pro_Msg#protomessage.signaltype,Pro_Msg,State)
                end
        end.

