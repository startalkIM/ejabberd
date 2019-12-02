%%%----------------------------------------------------------------------
%%% File    : ejabberd_sql_sup.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : SQL connections supervisor
%%% Created : 22 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_sql_sup).

-behaviour(ejabberd_config).

-author('alexey@process-one.net').

-export([start_link/1, init/1,sql_call/1,
	 transform_options/1, opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

-define(POOLNAME, pg_pool).

start_link(_) ->
    Host = ?SERVER_KEY,
    supervisor:start_link({local,
			   gen_mod:get_module_proc(Host, ?MODULE)},
			  ?MODULE, []).

init([]) ->
    SizeArgs = ejabberd_config:get_option(pg_size_args, fun(A) -> A end, [{size, 10},{max_overflow, 20}]),
    WorkerArgs = ejabberd_config:get_option(pg_worker_args, fun(A) -> A end, []),
    ?INFO_MSG("the xxxxxxxxxxxxxxxxx opts is ~p~n", [{SizeArgs, WorkerArgs}]),
    PoolArgs = [{name, {local, ?POOLNAME}},
                {worker_module, ejabberd_sql}] ++ SizeArgs,
    PoolSpecs = [poolboy:child_spec(?POOLNAME, PoolArgs, WorkerArgs)],
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.

sql_call(Fun) ->
    Worker = poolboy:checkout(?POOLNAME),
    Res = Fun(Worker),
    poolboy:checkin(?POOLNAME, Worker),
    Res.

opt_type(_) ->
    [].


transform_options(Opts) ->
    lists:foldl(fun transform_options/2, [], Opts).

transform_options({odbc_server, {pgsql, Server, DB, User, Pass}}, Opts) ->
    transform_options({odbc_server, {pgsql, Server, 5432, DB, User, Pass}}, Opts);
transform_options(Opt, Opts) ->
    [Opt|Opts].
