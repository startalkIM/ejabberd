%%%%%%----------------------------------------------------------------------
%%%%%% File    : qtalk_muc.erl
%%%%%% Purpose : qtalk_muc_room
%%%%%%----------------------------------------------------------------------

-module(qtalk_muc).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-include("mod_muc_room.hrl").

-export([get_muc_registed_user_num/2,set_muc_room_users/5,del_muc_room_users/5]).
-export([get_muc_users/2]).

%%%%%%%%%%%--------------------------------------------------------------------
%%%%%%%%%%% @date 2017-03-01
%%%%%%%%%%% 向ets表中添加群用户成员
%%%%%%%%%%%--------------------------------------------------------------------

set_muc_room_users(Server,User,Room,Domain,Host) ->
    case catch qtalk_sql:insert_muc_users_sub_push(Server,Room, Domain, User, Host) of
        {updated,1} -> true;
        _ -> false
    end.

%%%%%%%%%%% @date 2017-03-01
%%%%%%%%%%% 获取群用户注册的数量
%%%%%%%%%%%--------------------------------------------------------------------
get_muc_registed_user_num(Room,Domain) ->
    length(get_muc_users(Room,Domain)).

%%%%%%%%%%%--------------------------------------------------------------------
%%%%%%%%%%% @date 2017-03-01
%%%%%%%%%%% 删除ets表中muc_room_users数据
%%%%%%%%%%%--------------------------------------------------------------------
del_muc_room_users(Server,Room, Domain, User,Host) ->
    catch qtalk_sql:del_muc_user(Server,Room, Domain, User, Host).

get_muc_users(Room, Host) ->
    case catch qtalk_sql:get_muc_users(?SERVER_KEY,Room, Host) of
        {selected, _, Res} ->
            lists:foldl(fun([U, H], Acc) ->
                case str:str(H,<<"conference">>) of
                    0 -> [{U, H}|Acc];
                    _ -> Acc
                end
            end, [], Res);
        _ -> []
    end.
