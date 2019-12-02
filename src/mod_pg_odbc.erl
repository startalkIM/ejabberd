%%%----------------------------------------------------------------------
%%% File    : pg_odbc_sup.erl
%%%----------------------------------------------------------------------

-module(mod_pg_odbc).

-author('liufannana@sina.com').

-include("logger.hrl").
-include("ejabberd.hrl").

%% API
-export([start/2,stop/1]).
-export([start_link/1, init/1]).

-export([add_pid/2, remove_pid/2,
	 get_pids/1, get_random_pid/1, transform_options/1]).

-export([mod_opt_type/1, depends/2]).

-define(PROCNAME, mod_pg_odbc).
-define(DEFAULT_POOL_SIZE, 30).
-define(PGSQL_PORT, 5432).

start(_, Opts)->
    Host = ?SERVER_KEY,
    ?DEBUG("mod_pg_odbc Opts ~p ~n",[Opts]),
    Proc = get_proc_name(Host),
    ChildSpec =  {Proc,{?MODULE, start_link,[Opts]}, permanent, infinity,supervisor,[Proc]},
    {ok, _Pid} = supervisor:start_child(ejabberd_sup, ChildSpec).

start_link(Options) ->
    ets:new(pg_odbc_pid, [named_table, bag, public]),
    supervisor:start_link({local,?MODULE}, ?MODULE, [Options]).


init([Options]) ->
	ChildSpec = init(Options, []),
	{ok,
     {{one_for_one, 1000, 1}, ChildSpec}}.

init([], Acc) ->
	Acc;
init([{_, Option}|R], Acc) ->
    PoolSize = proplists:get_value(pool_size, Option, ?DEFAULT_POOL_SIZE),
    Host = proplists:get_value(host, Option),
	NewAcc = 
	lists:map(fun(I) ->
		{{Host, I},
			{pg_odbc, start_link, [Option]},
			transient,
			2000,
			worker,
			[?MODULE]} end, lists:seq(1, PoolSize)) ++ Acc,

	init(R, NewAcc).

get_pids(Host) ->
    case ets:tab2list(pg_odbc_pid) of
    [] ->
		[];
    Rs when is_list(Rs) ->
		 lists:foldl(fun({H, Pid}, Acc) when H =:= Host ->
                        [Pid|Acc];
                     (_, Acc) ->
                        Acc 
                     end, [], Rs);
    _ ->
		[]
    end.

get_random_pid(Host) ->
    case get_pids(Host) of
      [] -> undefined;
      Pids -> lists:nth(erlang:phash(os:timestamp(), length(Pids)), Pids)
    end.

add_pid(Host, Pid) ->
      ets:insert(pg_odbc_pid,{Host, Pid}).

remove_pid(Host, Pid) ->
      ets:delete_object(pg_odbc_pid,{Host, Pid}).

transform_options(Opts) ->
    lists:foldl(fun transform_options/2, [], Opts).

transform_options({odbc_server, {Type, Server, Port, DB, User, Pass}}, Opts) ->
    [{odbc_type, Type},
     {odbc_server, Server},
     {odbc_port, Port},
     {odbc_database, DB},
     {odbc_username, User},
     {odbc_password, Pass}|Opts];

transform_options({odbc_server, {pgsql, Server, DB, User, Pass}}, Opts) ->
    transform_options({odbc_server, {pgsql, Server, ?PGSQL_PORT, DB, User, Pass}}, Opts);
transform_options(Opt, Opts) ->
    [Opt|Opts].

%%get_option(_, []) ->
%%	undefined;
%%get_option(Host, [Option|R]) ->
%%	case proplists:get_value(host, Option) of
%%	    Host -> Option;
%%	    _ -> get_option(Host, R)
%%	end. 

stop(_) ->
    Host = ?SERVER_KEY,
    Proc = get_proc_name(Host),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc),
    ets:delete(pg_odbc_pid),
    ok.

get_proc_name(Host) ->
    gen_mod:get_module_proc(Host, ?PROCNAME).

depends(_Host, _Opts) ->
    [].

mod_opt_type(_) ->
    [pool_size, keepalive_timeout, host, odbc_server, odbc_port, odbc_database, odbc_user, odbc_password, start_interval, dbtype].
