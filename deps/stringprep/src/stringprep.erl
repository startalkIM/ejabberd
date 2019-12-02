%%%----------------------------------------------------------------------
%%% File    : stringprep.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Interface to stringprep
%%% Created : 16 Feb 2003 by Alexey Shchepin <alexey@proces-one.net>
%%%
%%%
%%% stringprep, Copyright (C) 2002-2016   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(stringprep).

-author('alexey@process-one.net').

-export([start/0, load_nif/0, tolower/1, nameprep/1,
	 nodeprep/1, resourceprep/1]).

%%%===================================================================
%%% API functions
%%%===================================================================
start() ->
    application:start(stringprep).

load_nif() ->
    SOPath = p1_nif_utils:get_so_path(?MODULE, [stringprep], "stringprep"),
    case catch erlang:load_nif(SOPath, 0) of
        ok -> ok;
        Err -> error_logger:warning_msg("unable to load stringprep NIF: ~p~n", [Err]),
               {error, unable_to_load_nif}
    end.

-spec tolower(iodata()) -> binary() | error.
tolower(_String) ->
    erlang:nif_error(nif_not_loaded).

-spec nameprep(iodata()) -> binary() | error.
nameprep(_String) ->
    erlang:nif_error(nif_not_loaded).

-spec nodeprep(iodata()) -> binary() | error.
nodeprep(_String) ->
    erlang:nif_error(nif_not_loaded).

-spec resourceprep(iodata()) -> binary() | error.
resourceprep(_String) ->
    erlang:nif_error(nif_not_loaded).
