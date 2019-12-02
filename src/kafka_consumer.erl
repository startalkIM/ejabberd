-module(kafka_consumer).

-behaviour(gen_server).

-include("ejabberd.hrl").
-include("logger.hrl").
-include_lib("brod/include/brod.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/1]).

-export([handle_message/4, init/2]).
%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {consumer, topic, hosts}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link(Option) ->
    Topic = proplists:get_value(topic, Option),
    Server = get_server_name(Topic),
    gen_server:start_link({local, Server}, ?MODULE, [Option], []).

%% brod_group_subscriber behaviour callback
init(_GroupId, _Arg) -> {ok, []}.

%% brod_group_subscriber behaviour callback
handle_message(_Topic, Partition, Message, State) ->
  #kafka_message{ offset = Offset
                , key   = Key
                , value = Value
                } = Message,
  ?ERROR_MSG("~p ~p: offset:~w key:~s value:~s\n",
                        [self(), Partition, Offset, Key, Value]),
  {ok, ack, State}.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Option]) ->
    Servers = proplists:get_value(servers, Option),
    Hosts = lists:map(fun(H) ->
        [Host, Port] = binary:split(H, <<":">>),
        {list_to_atom(binary_to_list(Host)), binary_to_integer(Port)}
    end, Servers),
    Topic = proplists:get_value(topic, Option),

    Client = get_client_name(Topic),

    GroupConfig = [{offset_commit_policy, commit_to_kafka_v2},
                 {offset_commit_interval_seconds, 5}
                ],
    GroupId = proplists:get_value(groupid, Option),
    ClientConfig = [{reconnect_cool_down_seconds, 10}],
    ConsumerConfig = [{begin_offset, earliest}],

    {ok, _} = application:ensure_all_started(brod),
    ok = brod:start_client(Hosts, Client, ClientConfig),
    brod:start_link_group_subscriber(Client, GroupId, [Topic],
                                   GroupConfig, ConsumerConfig,
                                   _CallbackModule  = ?MODULE,
                                   _CallbackInitArg = []),

    {ok, #state{consumer = Client, topic = Topic, hosts = Hosts}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
get_server_name(Topic) ->
    list_to_atom(binary_to_list(<<Topic/binary, "_server">>)).

get_client_name(Topic) ->
    list_to_atom(binary_to_list(<<Topic/binary, "_client">>)).
