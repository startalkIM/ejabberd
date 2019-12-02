%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created :  8 May 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(ejabberd_redis).

-behaviour(ejabberd_config).

%% API
-define(SERVER, ?MODULE).

-export([start/0,stop/1, opt_type/1]).
-export([start_link/2,init/1]).

-export([add_pid/3, remove_pid/3, get_pids/2, get_random_pid/2]).
-export([add_pid/2, remove_pid/2, get_pids/1, get_random_pid/1]).

-define(DEFAULT_POOL_SIZE, 25).
-define(CONNECT_TIMEOUT, 500).

-include("logger.hrl").
-include("ejabberd.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    Host = ?SERVER_KEY,
    PoolSize = ejabberd_config:get_option(redis_pool_size, fun(A) -> A end, 1 ),
    StartMode = ejabberd_config:get_option(redis_start_mode, fun(A) -> A end, 1),
    SHosts = ejabberd_config:get_option(redis_sentinel_hosts,   fun(A) -> A end,"localhost"),
    RMaster = ejabberd_config:get_option(redis_master, fun(A) -> A end,"sentinel:mymaster"),
    RServer = ejabberd_config:get_option(redis_server, fun(A) -> A end,"localhost"),
    RPort = ejabberd_config:get_option(redis_port, fun(A) -> A end, 26379),
    PassConfigured = ejabberd_config:get_option(redis_password, fun(A) -> A end, ""),
    RTab =  ejabberd_config:get_option(redis_tab,fun(A) -> A end,<<"0,1,2,3,5,7">>),
    
    start(Host,[{redis_pool_size,PoolSize},{redis_start_mode,StartMode},{redis_tab,RTab},
			{redis_sentinel_hosts,SHosts},{redis_master,RMaster},{redis_server,RServer},{redis_port,RPort},{redis_password,PassConfigured}]).

start(Host, Opts)->
    start_redis_link(Host,Opts,qtalk_public:get_timestamp()).

start_redis_link(Host,Opts,Time) ->
    ChildSpec =  {?MODULE,{?MODULE, start_link,[Host,Opts]}, permanent, infinity,supervisor,[?MODULE]},
    catch ets:insert(redis_start_flag,{redis_start_flag,Time}),
    supervisor:start_child(ejabberd_sup, ChildSpec).


start_link(Host,Opts) ->
    catch ets:new(redis_pid, [named_table, bag, public]),
    catch ets:new(redis_start_flag, [named_table, set, public]),
    case supervisor:start_link({local, ?MODULE}, ?MODULE, [Host,Opts]) of
    {ok, Pid} ->
            {ok, Pid};
    {error, Reason} ->
            ?DEBUG(" supervisor start error ~p ",[Reason])
    end.


init([Host,Opts]) ->
    StartMode = gen_mod:get_opt(redis_start_mode, Opts, fun(A) -> A end, 1),
    PoolSize = gen_mod:get_opt(redis_pool_size, Opts, fun(A) -> A end, ?DEFAULT_POOL_SIZE),
    case StartMode of
    1 ->
        Redis_sentinel_hosts = parse_sentinel_host(gen_mod:get_opt(redis_sentinel_hosts, Opts, fun(A) -> A end, "")),
        eredis_sentinel:start_link(Redis_sentinel_hosts);
    _ ->
        ok
    end,
    Redis_Tabs = string:tokens(binary_to_list(gen_mod:get_opt(redis_tab, Opts, fun(A) -> A end, <<"0,1,2,3,5,7">>)),","),
    Tabs = lists:flatmap(fun(T) ->
                case catch list_to_integer(T) of
                I when is_integer(I) ->
                    [I];
                _ ->
                    []
                end end,Redis_Tabs),
    {ok,
     {{one_for_one, ?DEFAULT_POOL_SIZE * 10, 1},
        lists:flatmap(fun (I) -> lists:map(fun(Tab) ->
            {I*1000+Tab,
             {redis_link, start_link,
              [Host, Tab,Opts]},
             permanent, 2000, worker, [?MODULE]}
        end,Tabs) end,
        lists:seq(1, PoolSize))}}.


get_pids(Tab) ->
	get_pids(?SERVER_KEY,Tab).
	
get_pids(_Host,Tab) ->
   Host = ?SERVER_KEY,
    case ets:lookup(redis_pid,Host) of
    [] ->
        case whereis(?MODULE) of
        undefined ->
            restart_redis(Host),
            [];
        _ ->
            []
        end;
    Rs when is_list(Rs) ->
           lists:flatmap(fun(B) ->
                 case element(2,B) of
                 Tab -> [element(3,B)];
                 _ -> []
                 end
          end,Rs);
    _ ->
        []
    end.

get_random_pid(Tab) ->
	get_random_pid(?SERVER_KEY,Tab).

get_random_pid(_Host,Tab) ->
     Host = ?SERVER_KEY,
    case get_pids(Host,Tab) of
      [] -> none;
      Pids -> lists:nth(erlang:phash(p1_time_compat:unique_integer(), length(Pids)), Pids)
    end.

add_pid(Tab, Pid) ->
	add_pid(?SERVER_KEY,Tab, Pid).

add_pid(_Host,Tab, Pid) ->
	Host = ?SERVER_KEY,
      ets:insert(redis_pid,{Host,Tab, Pid}).

remove_pid(Tab, Pid) ->
	remove_pid(?SERVER_KEY,Tab, Pid).

remove_pid(_Host,Tab, Pid) ->
	Host = ?SERVER_KEY,
   catch ets:delete_object(redis_pid,{Host,Tab,Pid}).

stop_child() ->
    lists:foreach(fun({D,T,Pid}) ->
        catch ets:delete_object(redis_pid,{D,T,Pid}),
        catch gen_server:cast(Pid, stop) end,ets:tab2list(redis_pid)).

stop(_Host) ->
    supervisor:terminate_child(ejabberd_sup, ?MODULE),
    supervisor:delete_child(ejabberd_sup, ?MODULE),
    catch stop_child(),
    catch ets:delete(redis_pid),
    catch ets:delete(redis_start_flag).

parse_sentinel_host(Configure) ->
    Sentinel_hosts = str:tokens(Configure,<<",">>),
    lists:map(fun(Sentinel) ->
        [Host,Port] = str:tokens(Sentinel,<<":">>),
        {binary_to_list(Host),binary_to_integer(Port)} end,Sentinel_hosts).

restart_redis(Host) ->
    Now = qtalk_public:get_timestamp(),
    case ets:lookup(redis_start_flag,redis_start_flag) of
    [{_,Time}] ->
        if Now - Time > 10 ->
            gen_mod:start_module(Host, mod_redis);
        true ->
            ok
        end;
    _ ->
        gen_mod:start_module(Host, mod_redis)
    end.

opt_type(_) -> [].
