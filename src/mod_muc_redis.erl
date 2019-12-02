-module(mod_muc_redis).

-export([register_room/3,get_muc_room_pid/2,room_destroyed/3,get_vh_rooms/1,clear_all_muc/1,shutdown_rooms/1]).
-export([stop_muc/2,shutdown_local_rooms/1,clean_table_from_node/2]).
-export([test1/2, get_all_rooms/1]).


-define(MUC_REDIS_TABLE,0).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").
-include("mod_muc.hrl").


register_room(Host, Room, Pid) ->
    M = #muc_online_room{name_host = {Room, Host},   pid = Pid},
    mod_redis:qp(?MUC_REDIS_TABLE,[["HSET", Room, Host, term_to_binary(M)],["HSET",muc_to_key(Host),Room,term_to_binary(M)]]).



get_muc_room_pid(Room,Host) ->
    case catch mod_redis:hash_get(?MUC_REDIS_TABLE,Room,Host) of
    {ok,undefined} ->
        [];
    {ok,M}  ->
        [binary_to_term(M)];
    _ ->
        []
    end.

clear_all_muc(Host) ->
    case catch get_vh_rooms(Host) of
    L when is_list(L) ->
       lists:foreach(fun(M) ->
            {Room, Host} = M#muc_online_room.name_host,
            Pid = M#muc_online_room.pid,
            case node(Pid) =:= node() of
            true ->      room_destroyed(Host,Room,M#muc_online_room.pid) ;
            false ->
                ok
            end  end,L) ;
    _ ->
        ok
    end.
   

room_destroyed(Host,Room,_Pid) ->
%    M = #muc_online_room{name_host = {Room, Host},   pid = Pid},
    mod_redis:qp(?MUC_REDIS_TABLE,[["HDEL", Room, Host],["HDEL",muc_to_key(Host),Room]]).

stop_muc(Room,Host) ->
    case catch get_muc_room_pid(Room,Host) of
    [R] ->
        Pid = R#muc_online_room.pid,
        Pid ! shutdown;
    _ ->
        ok
    end.

get_vh_rooms(Host) ->
    case catch mod_redis:redis_cmd(?MUC_REDIS_TABLE,["HVALS", muc_to_key(Host)]) of
    {ok,L} when is_list(L) ->
        lists:map(fun(I) ->
                binary_to_term(I) end,L);
    _ ->
        []
    end.
    
muc_to_key(Host) ->
    <<"MOD_MUC_HOST:", Host/binary>>.


shutdown_rooms(Host)    ->
    case catch get_vh_rooms(Host)  of
    [] ->
        ok;
    L when is_list(L) ->
        lists:foreach(fun(Muc) ->
          %  Muc = binary_to_term(M),
            Pid = Muc#muc_online_room.pid,
            Pid ! shutdown end,L);
    _ ->
        ok
    end.
    

shutdown_local_rooms(Host)    ->
    clean_table_from_node(node(),Host).


clean_table_from_node(Node,Host) ->
    case catch get_all_rooms(Host)  of
    [] ->
        ok;
    L when is_list(L) ->
        lists:foreach(fun(Muc) ->
            Pid = Muc#muc_online_room.pid,
            case Node =:= node(Pid)  of
            true ->
                Pid ! shutdown;
            _ ->
                ok
            end end,L);
    _ ->
        ok
    end.

get_all_rooms(Host) ->
    case catch mod_redis:redis_cmd(?MUC_REDIS_TABLE,["KEYS",<<"*">>]) of
    {ok,L} when is_list(L) ->
        lists:flatmap(fun(I) ->
                case str:str(I,<<"get_online_flags">>) of
                0 ->
                    case catch mod_redis:redis_cmd(?MUC_REDIS_TABLE,[<<"hget">>,I,Host]) of
                    {ok,M} when is_binary(M) ->
                        [binary_to_term(M)];
                    _ ->
                        []
                    end;
                _ ->
                    []
                end end,L);
    _ ->
        []
    end.

test1(_Host, Domain) ->
    {ok,L} =  mod_redis:redis_cmd(?MUC_REDIS_TABLE,["KEYS",<<"*">>]) ,
     lists:foreach(fun(I) ->
                case str:str(I,<<"get_online_flags">>) of
                0 ->
                    case catch mod_redis:redis_cmd(?MUC_REDIS_TABLE,[<<"hget">>,I,Domain]) of
                    {ok,M} when is_binary(M) ->
                        mod_redis:redis_cmd(?MUC_REDIS_TABLE,["DEL",I]);
                    _ ->
                        ok
                    end;
                 _ ->
                    ok
                end end,L).
