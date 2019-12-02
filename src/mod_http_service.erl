-module(mod_http_service).

-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("logger.hrl").

-export([start/2,stop/1]).
-export([start_link/1,init/1]).
-export([mod_opt_type/1,depends/2]).

start(_Host, Opts)->
    ChildSpec =  {?MODULE,{?MODULE, start_link,[Opts]}, permanent, infinity,supervisor,[?MODULE]},
    {ok, _Pid} = supervisor:start_child(ejabberd_sup, ChildSpec).

start_link(Opts) ->
    case supervisor:start_link({local, ?MODULE}, ?MODULE, [Opts]) of
	{ok, Pid} ->
	        {ok, Pid};	
   	{error, Reason} ->
	        ?DEBUG(" supervisor start error ~p ",[Reason])
     end.

init([Opts]) ->
	Http_service   = {http_service, {http_service,start_link, [Opts]}, permanent, brutal_kill, worker, [http_service]},
    {ok, {{one_for_one, 1, 1},[Http_service]}}.

stop(_Host) ->
    supervisor:terminate_child(ejabberd_sup, ?MODULE),
    supervisor:delete_child(ejabberd_sup, ?MODULE).

depends(_,_)->
    [].

mod_opt_type(_) ->
    [].
