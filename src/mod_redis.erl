%%%----------------------------------------------------------------------
%%%%%% File    : mod_redis.erl
%%%%%% Offfer redis service
%%%%%%
%%%%%%----------------------------------------------------------------------

-module(mod_redis).

-include("ejabberd.hrl").
-include("logger.hrl").

-export([start/0,stop/1]).
-export([start_link/0,init/1]).
-export([mod_opt_type/1,depends/2]).
-export([expire_time/3,
         str_set/3,
         str_setex/4,
         str_get/2,
         hash_set/4,
         hash_get/3,
         hash_del/3,
         str_del/2,
         get_all_keys/1,
         ttl_key/2,
         redis_cmd/2,
         q/2,
         qp/2]).

-define(POOLNAME, redis_pool).

start()->
    ChildSpec =  {?MODULE,{?MODULE, start_link,[]}, permanent, infinity,supervisor,[?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).


start_link() ->
    case supervisor:start_link({local, ?MODULE}, ?MODULE, []) of
    {ok, Pid} ->
            {ok, Pid};
    {error, Reason} ->
            ?DEBUG(" supervisor start error ~p ",[Reason])
     end.

init([]) ->
    SizeArgs = ejabberd_config:get_option(redis_size_args, fun(A) -> A end, [{size, 10},{max_overflow, 20}]),
    WorkerArgs = ejabberd_config:get_option(redis_worker_args, fun(A) -> A end, []),
    ?INFO_MSG("the xxxxxxxxxxxxxxxxx opts is ~p~n", [{SizeArgs, WorkerArgs}]),
    PoolArgs = [{name, {local, ?POOLNAME}},
                {worker_module, redis_link}] ++ SizeArgs,
    PoolSpecs = [poolboy:child_spec(?POOLNAME, PoolArgs, WorkerArgs)],
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.

depends(_Host, _Opts) -> [].

mod_opt_type(_) -> [].

stop(_Host) ->
    supervisor:terminate_child(ejabberd_sup, ?MODULE),
    supervisor:delete_child(ejabberd_sup, ?MODULE).

expire_time(Tab,Key,Time) ->
    do_call({Tab, ["EXPIRE", Key, Time]}).

str_set(Tab, Key, Val) ->
    do_call({Tab, ["SET", Key, Val]}).

str_setex(Tab, Key, Time, Val) ->
    do_call({Tab,  ["SETEX",Key, Time, Val]}).

str_get(Tab,Key) ->
    do_call({Tab,["GET",Key]}).

hash_set(Tab,Key,Field,Val) ->
    do_call({Tab, ["HSET", Key, Field, Val]}).

hash_get(Tab,Key,Field) ->
    do_call({Tab, ["HGET",Key,Field]}).

hash_del(Tab,Key,Field) ->
    do_cast({Tab, ["HDEL", Key, Field]}).

str_del(Tab,Key) ->
    do_cast({Tab, ["DEL", Key]}).

get_all_keys(Tab) ->
    do_call({Tab, ["KEYS", <<"*">>]}).

ttl_key(Tab,Key) ->
    do_call({Tab, ["TTL", Key]}).

redis_cmd(Tab,Cmd) ->
    do_call({Tab, Cmd}).

q(Tab,Cmd) ->
    do_call({Tab,Cmd}).

qp(Tab,Pipeline) ->	
    do_call({Tab,{qp,Pipeline}}).

do_call({Tab, Request}) ->
    Worker = poolboy:checkout(?POOLNAME),
    Res = gen_server:call(Worker, {Tab, Request}),
    poolboy:checkin(?POOLNAME, Worker),
    Res.

do_cast({Tab, Request}) ->
    Worker = poolboy:checkout(?POOLNAME),
    Res = gen_server:cast(Worker, {Tab, Request}),
    poolboy:checkin(?POOLNAME, Worker),
    Res.
