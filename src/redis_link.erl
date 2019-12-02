-module(redis_link).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
                        terminate/2,code_change/3]).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-include("ejabberd.hrl").
-include("logger.hrl").

-record(state, {redis_pid}).

start_link(Opts) ->
    gen_server:start_link(?MODULE, [Opts], []).

init([Opts]) ->
    Host = gen_mod:get_opt(host, Opts, fun(A) -> A end, <<"redis_server">>),
    Port = gen_mod:get_opt(port, Opts, fun(A) -> A end, 26379),
    Pass = gen_mod:get_opt(password, Opts, fun(A) -> A end, <<"redis_password">>),
    Tab = gen_mod:get_opt(tab, Opts, fun(A) -> A end, 0),
    {ok, Pid} = eredis:start_link(binary_to_list(Host),Port,Tab,binary_to_list(Pass),1000),
    {ok, #state{redis_pid = Pid}}.

handle_call({Tab, {qp,Pipeline}},_,State)  ->
    [_|Ret] =  eredis:qp(State#state.redis_pid, [["SELECT", Tab]|Pipeline]),
    {reply, Ret, State};
handle_call({Tab, Cmd}, _From, State) ->
    [_, Ret] =  eredis:qp(State#state.redis_pid, [["SELECT", Tab], Cmd]),
    {reply, Ret, State}.

handle_cast({Tab, Cmd}, State) ->
    eredis:qp(State#state.redis_pid, [["SELECT", Tab], Cmd]),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_From,State) ->
    {noreply,State}.

terminate(_Reason, State) ->
    {ok,State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
