-module(kafka_producer).

-behaviour(gen_server).

-include("ejabberd.hrl").
-include("logger.hrl").
-include_lib("brod/include/brod.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/1]).

-export([send_message/4,
         send_message/3]).
%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {producer, topic, hosts}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link(Option) ->
    Name = proplists:get_value(name, Option),
    Server = get_server_name(Name),
    gen_server:start_link({local, Server}, ?MODULE, [Option], []).

send_message(Topic, Key, Value) ->
    case catch erlang:process_info(whereis(get_server_name(Topic)),message_queue_len) of
    {message_queue_len,N} when N > 10000 ->
        ?INFO_MSG("Kakfa queue is ~p ~n,drop Value ~n",[N]);
    {message_queue_len, _} ->
        gen_server:call(get_server_name(Topic), {send_message, Key, Key, Value});
    _ -> ok
    end.

send_message(Topic, Partition, Key, Value) ->
    gen_server:call(get_server_name(Topic), {send_message, Partition, Key, Value}).

start_producer(_Hosts, _Topic, N) when N =< 0->
    ok;
start_producer(Hosts, Topic, N) ->
    Client = get_client_name(Topic, N),
    ClientConfig = [{reconnect_cool_down_seconds, 10}, {request_timeout, 10000}],
    {ok, _} = application:ensure_all_started(brod),
    ok = brod:start_client(Hosts, Client, ClientConfig),

    brod:start_producer(_Client = Client, _Topic = Topic, _ProducerConfig = []),
    start_producer(Hosts, Topic, N-1).

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
    N = 4,

    start_producer(Hosts, Topic, N),
    {ok, #state{producer = N, topic = Topic, hosts = Hosts}}.

handle_call({send_message, _Partition, Key, Value},  _From, State) ->
    N = State#state.producer,
    Topic = State#state.topic,
    Client = get_one_client_name(Topic, N),
    Res = brod:produce(Client
                           , Topic
                           , fun(_, C, _, _) ->
                                 {ok, erlang:phash(p1_time_compat:unique_integer(), C) - 1}
                             end
                           , Key, Value),
    ?DEBUG("the params is xxxxxxxxxxxxx ~p, the res is ~p~n", [{N, Topic, Client}, Res]),
    {reply, Res, State};
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

get_client_name(Topic, N) ->
    Nlist = integer_to_binary(N),
    list_to_atom(binary_to_list(<<Topic/binary, "_client_", Nlist/binary>>)).

get_one_client_name(Topic, N) ->
    One = erlang:phash(p1_time_compat:unique_integer(), N),
    OneList = integer_to_binary(One),
    list_to_atom(binary_to_list(<<Topic/binary, "_client_", OneList/binary>>)).
