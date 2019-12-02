-module(mod_static).

-behaviour(gen_server).
-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("logger.hrl").

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/2,stop/1,start_link/1]).
-export([depends/2, mod_opt_type/1]).

-export([
        add_record/2,
        get_static0/0,
        get_static1/0,
        get_static2/0,
        get_static3/0,
        get_static4/0
    ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
        current_munite = dict:new(),
        before_munite1 = dict:new(),
        before_munite2 = dict:new(),
        before_munite3 = dict:new(),
        before_munite4 = dict:new()
    }).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start(Host,Opts) ->
    Proc = get_proc_name(Host),
    ChildSpec = {Proc,{?MODULE, start_link, [Opts]}, temporary,1000,worker,[?MODULE]},
    {ok,_Pid} = supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = get_proc_name(Host),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Options], []).

add_record(Key, Value) ->
    gen_server:cast(?SERVER, {add_record, Key, Value}).

get_static0() ->
    gen_server:call(?SERVER, get_static0).

get_static1() ->
    gen_server:call(?SERVER, get_static1).

get_static2() ->
    gen_server:call(?SERVER, get_static2).

get_static3() ->
    gen_server:call(?SERVER, get_static3).

get_static4() ->
    gen_server:call(?SERVER, get_static4).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([_Options]) ->
    erlang:send_after(60000, self(), update),
    {ok, #state{}}.

handle_call(get_static0, _From, State = #state{current_munite = Static}) ->
    {reply, dict:to_list(Static), State};
handle_call(get_static1, _From, State = #state{before_munite1 = Static}) ->
    {reply, dict:to_list(Static), State};
handle_call(get_static2, _From, State = #state{before_munite2 = Static}) ->
    {reply, dict:to_list(Static), State};
handle_call(get_static3, _From, State = #state{before_munite3 = Static}) ->
    {reply, dict:to_list(Static), State};
handle_call(get_static4, _From, State = #state{before_munite4 = Static}) ->
    {reply, dict:to_list(Static), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({add_record, Key, Value}, State = #state{current_munite = Static}) ->
    NewStatic =
    case dict:find(Key, Static) of
        error ->
            dict:store(Key, Value, Static);
        {ok, V} ->
            dict:store(Key, V + Value, Static)
    end,
    {noreply, State#state{current_munite = NewStatic}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(update, State = #state{
        current_munite = Current,
        before_munite1 = B1,
        before_munite2 = B2,
        before_munite3 = B3}) ->
    erlang:send_after(60000, self(), update),
    {noreply, State#state{
            current_munite = dict:new(),
            before_munite1 = Current,
            before_munite2 = B1,
            before_munite3 = B2,
            before_munite4 = B3
        }};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
get_proc_name(Host) ->
    gen_mod:get_module_proc(Host, ?MODULE).

depends(_Host, _Opts) ->
    [].

mod_opt_type(_) -> [].
