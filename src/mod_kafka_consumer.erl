-module(mod_kafka_consumer).

-behaviour(gen_mod).
-behaviour(supervisor).

-include("ejabberd.hrl").
-include("logger.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/2, stop/1, depends/2, mod_opt_type/1]).

-export([start_link/1]).

-export([init/1]).
%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-define(SERVER, ?MODULE).
-define(PROCNAME, mod_kafka_producer).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start(Host, Opts)->
    Proc = get_proc_name(Host),
    ChildSpec =  {Proc,{?MODULE, start_link,[Opts]}, permanent, infinity, supervisor, [Proc]},
    {ok, _Pid} = supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = get_proc_name(Host),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc),
    ok.

get_proc_name(Host) ->
    gen_mod:get_module_proc(Host, ?PROCNAME).


start_link(Opts) ->
    case supervisor:start_link({local, ?SERVER}, ?MODULE, [Opts]) of
        {ok, Pid} ->
            {ok, Pid};
        {error, Reason} ->
            ?DEBUG(" supervisor ~p  start error ~p ",[?MODULE,Reason]),
            {error, Reason}
     end.

init([Opts]) ->
    {ok, {{one_for_one, 10, 1},
                               lists:map(fun({Proc, Option}) ->
                                                 {Proc, {kafka_consumer, start_link, [Option]}, transient, 2000, worker, [kafka_consumer]}
                                         end, Opts)}}.

depends(_Host, _Opts) ->
    [].

mod_opt_type(_) -> [servers, topic, groupid].
