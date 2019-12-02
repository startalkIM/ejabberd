%%%%%%------------------------------------------------------------------------
%%%%%%%%% File    : qtalk_sql.erl
%%%%%%%%% Purpose : sql
%%%%%%%%%----------------------------------------------------------------------

-module(qtalk_sql).

-compile([{parse_transform, ejabberd_sql_pt}]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").
-include("jlib.hrl").

-export([get_muc_users/3,get_user_muc_subscribe/3,get_user_register_muc/2]).
-export([insert_muc_users_sub_push/5,del_muc_user/5,del_user_muc_subscribe/4,add_user_muc_subscribe/6]).
-export([get_muc_opts/3,get_muc_vcard_info/3,insert_muc_users/5]).
-export([get_user_register_mucs/3,insert_muc_msg/8]).
-export([insert_warn_msg/8,insert_msg_v1/8,insert_msg_v2/8, insert_msg_v3/11]).
-export([insert_user_register_mucs/5,update_register_mucs/6,del_muc_vcard_info/3,restore_muc_user_mark/3]).
-export([del_user_register_mucs/3,get_concats/2,get_muc_concats/2,insert_user_block_list/3]).
-export([del_muc_users/3,get_department_info/1,get_department_info1/1]).
-export([get_blacklist/1,get_whitelist/1,insert_subscribe_msg/5]).
-export([get_s2s_host_info/1,get_muc_user_host/4,get_muc_user_host/3]).
-export([insert_muc_vcard_info/7,get_muc_msg_last_timestamp/2]).
-export([delete_muc_last/2,get_white_list_users/1,update_no_insert/4]).
-export([list_users/1,get_flogin_user/1,insert_msg_by_table/9,get_password_by_host/2,get_password_salt_by_host/2]).
-export([get_host_info/1]).

get_muc_users(LServer, Muc, Domain) ->
    ejabberd_sql:sql_query(LServer,
        %%?SQL("select @(username)s,@(host)s from muc_room_users where muc_name=%(Muc)s")).
        [<<"select username, host from muc_room_users where muc_name='">>, Muc, <<"' and domain ='">>, Domain, <<"';">>]).

get_user_muc_subscribe(LServer,Muc, Domain) ->
    ejabberd_sql:sql_query(LServer,
        %%?SQL("select @(username)s,@(host)s from muc_room_users where muc_name=%(Muc)s and subscribe_flag = '1'")).
        [<<"select username, host from muc_room_users where muc_name='">>, Muc, <<"' and domain ='">>, Domain, <<"' and subscribe_flag = '1';">>]).

get_user_register_muc(LServer,Muc) ->
    ejabberd_sql:sql_query(LServer,
        %%?SQL("select @(username)s,@(domain)s from user_register_mucs where muc_name=%(Muc)s")).
        [<<"select username, domain from user_register_mucs where muc_name='">>, Muc, <<"';">>]).

insert_muc_users(LServer,Muc_name, Domain, User,Host) ->
    Time = integer_to_binary(qtalk_public:get_timestamp()),
    Num = <<"0">>,
    ejabberd_sql:sql_query(LServer,
        %%?SQL("insert into muc_room_users (muc_name,username,host,subscribe_flag,login_date)"
        %%        " values (%(Muc_name)s,%(User)s,%(Host)s,%(Num)s,%(Time)d);")).
        [<<"insert into muc_room_users (muc_name,domain, username,host,subscribe_flag,login_date) values ('">>,
         Muc_name, <<"', '">>,
         Domain, <<"', '">>,
         User, <<"', '">>,
         Host, <<"', '">>,
         Num, <<"', '">>,
         Time, <<"');">>]).

insert_muc_users_sub_push(LServer,Muc_name, Domain, User,Host) ->
    Time = integer_to_binary(qtalk_public:get_timestamp()),
    Num = <<"1">>,
    ejabberd_sql:sql_query(LServer,
        %%?SQL("insert into muc_room_users (muc_name,username,host,subscribe_flag,login_date)"
        %%        " values (%(Muc_name)s,%(User)s,%(Host)s,%(Num)s,%(Time)d);")).
        [<<"insert into muc_room_users (muc_name, domain, username,host,subscribe_flag,login_date) values ('">>,
         Muc_name, <<"', '">>,
         Domain, <<"', '">>,
         User, <<"', '">>,
         Host, <<"', '">>,
         Num, <<"', '">>,
         Time, <<"');">>]).

del_muc_user(LServer,Muc, Domain, User, Host) ->
    ejabberd_sql:sql_query(LServer,
        %%?SQL("delete from muc_room_users where username = %(User)s and muc_name = %(Muc)s")).
        [<<"delete from muc_room_users where username = '">>,User, <<"' and host = '">>, Host, <<"' and muc_name = '">>,Muc, <<"' and domain = '">>, Domain, <<"';">>]).

del_user_muc_subscribe(LServer,Muc, Domain, User) ->
    ejabberd_sql:sql_query(LServer,
        %%?SQL("update muc_room_users set subscribe_flag = '0' where username = %(User)s and muc_name = %(Muc)s")).
        [<<"update muc_room_users set subscribe_flag = '0' where username = '">>, User, <<"' and muc_name = '">>, Muc, <<"' and domain = '">>, Domain, <<"';">>]).

add_user_muc_subscribe(LServer,Muc, Domain, User, Host,SFlag) ->
    ejabberd_sql:sql_query(LServer,
        %%?SQL("update muc_room_users set subscribe_flag = '1' where username = %(User)s and muc_name = %(Muc)s")).
        [<<"update muc_room_users set subscribe_flag = '">>, SFlag, <<"' where username = '">>, User, <<"' and host = '">>,Host,
				<<"' and muc_name = '">>, Muc, <<"' and domain = '">>, Domain, <<"';">>]).

get_muc_opts(LServer,Muc,Host) ->
    ejabberd_sql:sql_query(LServer,
        %%?SQL("select @(opts)s from muc_room where name=%(Muc)s and host=%(Host)s")).   
        [<<"select opts from muc_room where name = '">>, Muc, <<"' and host = '">>, Host, <<"';">>]).

get_muc_vcard_info(LServer,Muc,Host) ->
    FMuc = qtalk_public:concat(Muc,<<"@">>,Host),
    ejabberd_sql:sql_query(LServer,
        %%?SQL("select @(show_name)s,@(muc_desc)s,@(muc_title)s,@(muc_pic)s,@(version)s from muc_vcard_info where "
        %%    " muc_name = %(Muc)s or muc_name = %(FMuc)s ;")).
        [<<"select show_name, muc_desc, muc_title, muc_pic, version from muc_vcard_info where muc_name = '">>,
         Muc, <<"' or muc_name = '">>, FMuc, <<"';">>]).

get_user_register_mucs(LServer,User,Host) ->
    ejabberd_sql:sql_query(LServer,
        %%?SQL("select @(muc_name)s,@(domain)s from user_register_mucs where username=%(User)s and registed_flag = 1")).
        [<<"select muc_name, domain from user_register_mucs where username= '">>, User, <<"' and host = '">>,Host,<<"' and registed_flag > 0;">>]).

insert_muc_msg(LServer,Muc,Nick,Host,Packet,Size,ID,Time) ->
    ejabberd_sql:sql_query(LServer,
        [<<"insert into muc_room_history(muc_room_name,nick,host,packet,have_subject,size,msg_id,create_time) values ('">>,
            Muc,<<"','">>,Nick,<<"','">>,Host,<<"','">>,Packet,<<"','1','">>,integer_to_list(Size),<<"','">>,ID,<<"',">>,Time,<<");">>]).


insert_warn_msg(LServer,From,To,FHost,THost,Body,ID,Time) ->
    ejabberd_sql:sql_query(LServer,
        [<<"insert into warn_msg_history(m_from,m_to,m_body,msg_id,from_host,to_host,read_flag,create_time) values ('">>,
            From,<<"','">>,To,<<"','">>,Body,<<"','">>,ID,<<"','">>,FHost,<<"','">>,THost,<<"',">>,<<"1,">>,Time,<<");">>]).

insert_msg_v1(LServer,From,To,FHost,THost,Body,ID,Time) ->
    ejabberd_sql:sql_query(LServer,
        [<<"insert into msg_history(m_from,m_to,m_body,msg_id,from_host,to_host,read_flag,create_time) values ('">>,
            From,<<"','">>,To,<<"','">>,Body,<<"','">>,ID,<<"','">>,FHost,<<"','">>,THost,<<"',1,">>,Time,<<");">>]).

insert_msg_v2(LServer,From,To,FHost,THost,Body,ID,Time) ->
    ejabberd_sql:sql_query(LServer,
        [<<"insert into msg_history(m_from,m_to,m_body,msg_id,from_host,to_host,create_time) values ('">>,
            From,<<"','">>,To,<<"','">>,Body,<<"','">>,ID,<<"','">>,FHost,<<"','">>,THost,<<"',">>,Time,<<");">>]).


insert_msg_v3(LServer,From,To,FHost,THost,Body,ID,Time, Realfrom, Realto, Type) ->
    ejabberd_sql:sql_query(LServer,
        [<<"insert into msg_history(m_from,m_to,m_body,msg_id,from_host,to_host,create_time,realfrom, realto, msg_type) values ('">>,
            From,<<"','">>,To,<<"','">>,Body,<<"','">>,ID,<<"','">>,FHost,<<"','">>,THost,<<"',">>,Time, <<",'">>, Realfrom, <<"','">>, Realto, <<"','">>, Type, <<"');">>]).


del_user_register_mucs(LServer,Muc,Domain) ->
    catch ejabberd_sql:sql_query(LServer,
        %%?SQL("update user_register_mucs set registed_flag = 0 , created_at = now() where muc_name = %(Muc)s ;")).
        [<<"update user_register_mucs set registed_flag = 0 , created_at = (now())::timestamp(3) where muc_name = '">>, Muc, <<"' and domain = '">>,Domain,<<"';">>]).

del_muc_users(LServer,Muc, Domain) ->
    Now = qtalk_public:get_timestamp(),
    catch ejabberd_sql:sql_query(LServer,
            [<<"insert into muc_user_mark (muc_name,user_name,login_date,logout_date) SELECT muc_name,username,login_date,">>,
                integer_to_binary(Now),<<" from muc_room_users where muc_name = '">>,Muc, <<"' and domain = '">>, Domain, <<"';">>]),
    catch ejabberd_sql:sql_query(LServer,
        %%?SQL("delete from muc_room_users where muc_name =  %(Muc)s;")).
        [<<"delete from muc_room_users where muc_name = '">>, Muc, <<"' and domain = '">>, Domain, <<"';">>]).

restore_muc_user_mark(LServer,Muc, Domain) ->
    Logout = integer_to_binary(qtalk_public:get_timestamp()),
    case catch ejabberd_sql:sql_query(LServer,
            %%?SQL("select @(username)s,@(login_date)s from muc_room_users where muc_name = %(Muc)s;")) of
            [<<"select username, login_date from muc_room_users where muc_name = '">>, Muc, <<"' and domain = '">>, Domain, <<"';">>]) of
    {selected,_, SRes} when is_list(SRes) ->
        lists:foreach(fun([User,Login]) ->
            catch ejabberd_sql:sql_query(LServer,
                [<<"insert into muc_user_mark(muc_name,user_name,login_date,logout_date) values ('">>,Muc,<<"','">>,
                    User,<<"',">>,Login,<<",">>,Logout,<<");">>]) end,SRes);
    _ ->
        ok
    end,
    catch ejabberd_sql:sql_query(LServer,
	%%?SQL("delete from muc_room_users where  muc_name = %(Muc)s;")).
	[<<"delete from muc_room_users where  muc_name = '">>, Muc, <<"' and domain = '">>, Domain, <<"';">>]).


del_muc_vcard_info(LServer,Muc,Reason) ->
    FMuc = qtalk_public:concat(Muc,<<"@conference.">>,LServer),
    ?INFO_MSG("del_muc_vcard_info Muc:[~p], Reason : [~p] ~n",[Muc,Reason]),
    case catch ejabberd_sql:sql_query(LServer,
                %%?SQL("select @(show_name)s from muc_vcard_info where muc_name = %(Muc)s or muc_name = %(FMuc)s ;")) of
                [<<"select show_name from muc_vcard_info where muc_name ='">>, Muc, <<"' or muc_name ='">>, FMuc, <<"';">>]) of
    {selected,_,[[Show]]}  ->
        ?INFO_MSG("del_muc_vcard_info Muc:[~p], Nick : [~p] ~n",[Muc,Show]),
        catch ejabberd_sql:sql_query(LServer,
                    %%?SQL("insert into destroy_muc_info(muc_name,nick_name,reason) values ( %(Muc)s, %(Show)s,%(Reason)s)")),
                    [<<"insert into destroy_muc_info(muc_name,nick_name,reason) values ('">>, Muc, <<"', '">>, Show, <<"', '">>, Reason, <<"');">>]),
        catch ejabberd_sql:sql_query(LServer,
                    %%?SQL("delete from muc_vcard_info where muc_name = %(Muc)s or muc_name = %(FMuc)s ;"));
                    [<<"delete from muc_vcard_info where muc_name = '">>, Muc, <<"' or muc_name = '">>, FMuc, <<"';">>]);
    Err  ->
        ?INFO_MSG("del_muc_vcard_info Muc:[~p], error : [~p] ~n",[Muc,Err]),
        case Reason of
        <<"Owner Destroy">> ->
            catch ejabberd_sql:sql_query(LServer,
                    %%?SQL("delete from muc_vcard_info where muc_name = %(Muc)s or muc_name = %(FMuc)s ;"));
                    [<<"delete from muc_vcard_info where muc_name = '">>, Muc, <<"' or muc_name = '">>, FMuc, <<"';">>]);
        _ ->
            ok
        end
    end.

update_register_mucs(LServer,User,Host,Muc,Domain,RFlag) ->
 %   Flag = binary_to_integer(RFlag),
    case catch ejabberd_sql:sql_query(LServer,
        %%?SQL("update user_register_mucs set registed_flag = %(Flag)d,created_at = now() where username = "
        %%    "%(User)s and muc_name = %(Muc)s and domain = %(Domain)s ;")) of
        [<<"update user_register_mucs set registed_flag = '">>, RFlag, <<"', created_at = (now())::timestamp(3) where username = '">>, User,
			 <<"' and muc_name = '">>, Muc, <<"' and domain = '">>, Domain, <<"' and host = '">>,Host,<<"';">>]) of
        {updated, 1} ->
            ok;
         Error ->
            ?INFO_MSG("update user_register_mucs user ~p ,muc ~p Error ~p ",[User,Muc,Error])
         end.

insert_user_register_mucs(LServer,User,Host,Muc,Domain) ->
    case catch ejabberd_sql:sql_query(LServer,
        %%?SQL("insert into user_register_mucs(username,muc_name,domain) values ("
        %%    "%(User)s,%(Muc)s,%(Domain)s);")) of
        [<<"insert into user_register_mucs(username,host,muc_name,domain) values ('">>, User, <<"','">>,Host,<<"','">>, Muc, <<"', '">>, Domain, <<"');">>]) of
    {updated, 1} ->
            ok;
    _ ->
        update_register_mucs(LServer,User,Host,Muc,Domain,<<"1">>)
    end.

get_concats(LServer,User) ->
    ejabberd_sql:sql_query(LServer,
        [<<"select u from (select case when m_from = '">>,User,
            <<"' then m_to else m_from end as u,max(create_time) as m_time from msg_history where m_from = '">>,User,
                   <<"' or m_to='">>,User,<<"' group by m_from,m_to ) tab order by tab.m_time desc ;">>]).

get_muc_concats(LServer,User) ->
        ejabberd_sql:sql_query(LServer,
            [<<"select muc_name from  (select muc_room_name as muc_name,max(create_time) as m_time from muc_room_history where muc_room_name in">>,
                    <<" (select muc_name from muc_room_users where username = '">>,User,<<"') group by muc_room_name) tab order by m_time desc ">>]).
    
insert_user_block_list(LServer,User,BUser) ->
    ejabberd_sql:sql_query(LServer,
        %%?SQL("insert into user_block_list (username,blockuser) values (%(User)s,%(BUser)s);")).
        [<<"insert into user_block_list (username, blockuser) values ('">>, User, <<"', '">>, BUser, <<"');">>]).

get_department_info(LServer) ->
    ejabberd_sql:sql_query(LServer,
         %%?SQL("select @(dep1)s,@(dep2)s,@(dep3)s,@(dep4)s,@(dep5)s,@(username)s,@(name)s,@(department)s,@(fpinyin)s,@(spinyin)s "
         %%       "from users where hire_flag > 0  order by dep1,dep2,dep3,dep4,dep5;")).
         [<<"select dep1, dep2, dep3, dep4, dep5, username, name, department, fpinyin, spinyin from users where hire_flag > 0  order by dep1,dep2,dep3,dep4,dep5;">>]).

get_department_info1(LServer) ->
    ejabberd_sql:sql_query(LServer,
         [<<"select host_id,dep1, dep2, dep3, dep4, dep5, user_id, user_name, department, pinyin 
			from host_users where hire_flag > 0  order by dep1,dep2,dep3,dep4,dep5;">>]).

get_blacklist(LServer) ->
    ejabberd_sql:sql_query(LServer,
                %%?SQL("select @(username)s from users where frozen_flag = '1';")).
                [<<"select user_id from host_users where frozen_flag = '1';">>]).

get_whitelist(LServer) ->
    ejabberd_sql:sql_query(LServer,
                %%?SQL("select @(username)s, @(single_flag)s from white_list")).
                [<<"select username, single_flag from white_list;">>]).

insert_subscribe_msg(LServer,SubUsers,Muc,Nick,Msg) ->
    ejabberd_sql:sql_query(LServer,
                [<<"select spilt_users_to_insert_xml('">>,SubUsers,<<"','">>,Muc,<<"','">>,Nick,<<"','">>,Msg,<<"');">>]).

get_host_info(LServer) ->
	ejabberd_sql:sql_query(LServer,
		[<<"select id,host from host_info;">>]).

get_s2s_host_info(LServer) ->
    ejabberd_sql:sql_query(LServer,
        %%?SQL("select @(domain)s,@(host)s,@(port)s,@(priority)s,@(weight)s from s2s_mapped_host")).
        [<<"select domain, host, port, priority, weight from s2s_mapped_host;">>]).

get_muc_user_host(LServer,Muc,User) ->
    ejabberd_sql:sql_query(LServer,
        [<<"select host from muc_room_users where username='">>, User, <<"' and muc_name='">>, Muc, <<"';">>]).

get_muc_user_host(LServer,Muc,User, Domain) ->
    ejabberd_sql:sql_query(LServer,
        %%?SQL("select @(host)s from muc_room_users where username = %(User)s and muc_name = %(Muc)s ;")).
        [<<"select host from muc_room_users where username='">>, User, <<"' and muc_name='">>, Muc, <<"' and domain = '">>, Domain, <<"';">>]).

insert_muc_vcard_info(LServer,Muc,Nick,Desc,Title,Pic,Ver) ->
    ejabberd_sql:sql_query(LServer,
        %%?SQL("insert into muc_vcard_info(muc_name,show_name,muc_desc,muc_title,muc_pic,version) values 
        %%    (%(Muc)s,%(Nick)s,%(Desc)s,%(Title)s,%(Pic)s,%(Ver)d);")).
        [<<"insert into muc_vcard_info(muc_name,show_name,muc_desc,muc_title,muc_pic,version) values ('">>,
            Muc, <<"', '">>,
            Nick, <<"', '">>,
            Desc, <<"', '">>,
            Title, <<"', '">>,
            Pic, <<"', ">>,
            Ver, <<");">>]).

get_muc_msg_last_timestamp(LServer, Muc) ->
    ejabberd_sql:sql_query(LServer,
        [<<"select extract(epoch from create_time) from muc_room_history where muc_room_name = '">>,  Muc,
                <<"' order by create_time desc limit 1">>]).

delete_muc_last(LServer,Muc) ->
    ejabberd_sql:sql_query(LServer,
            %%?SQL("delete from muc_last where muc_name = %(Muc)s;")).
            [<<"delete from muc_last where muc_name = '">>, Muc, <<"';">>]).

get_white_list_users(LServer) ->
    ejabberd_sql:sql_query(LServer,
            %%?SQL("select @(username)s, @(single_flag)s from white_list;")).
            [<<"select username, single_flag from white_list;">>]).

get_flogin_user(LServer) ->
    ejabberd_sql:sql_query(LServer,
            %%?SQL("select @(username)s, @(single_flag)s from white_list;")).
            [<<"select username from flogin_user;">>]).

list_users(LServer) ->
    ejabberd_sql:sql_query(LServer,
            %%?SQL("select @(username)s from users where hire_flag > 0;")).
            [<<"select user_id from host_users where hire_flag > 0;">>]).

get_password_by_host(Host,User) ->
	ejabberd_sql:sql_query(Host,
		[<<"select password from host_users where host_id in (select id from host_info where host = '">>,
			Host,<<"') and user_id = '">>,User,<<"' and hire_flag = '1';">>]).

get_password_salt_by_host(Host,User) ->
	ejabberd_sql:sql_query(Host,
		[<<"select password, pwd_salt from host_users where host_id in (select id from host_info where host = '">>,
			Host,<<"') and user_id = '">>,User,<<"' and hire_flag = '1' and frozen_flag='0';">>]).


update_no_insert(Table, Fields, Vals, Where) ->
    UPairs = lists:zipwith(fun (A, B) ->
                   <<A/binary, "='", B/binary, "'">>
               end,
               Fields, Vals),
    case ejabberd_sql:sql_query_t([<<"update ">>, Table,
                    <<" set ">>, join(UPairs, <<", ">>),
                    <<" where ">>, Where, <<";">>])
    of
      {updated, 1} -> ok;
      Reason ->
            Reason
    end.

insert_msg_by_table(LServer,Table,From,From_host,To,To_host,MsgID,Time,Packet) ->
    catch ejabberd_sql:sql_query(LServer,
            [<<"insert into ">>,Table,<<" (m_from,from_host,m_to,to_host,m_body,msg_id,create_time) values ('">>,
                From,<<"','">>,From_host,<<"','">>,To,<<"','">>,To_host,<<"','">>,
                ejabberd_sql:escape(Packet),<<"','">>,MsgID,<<"',">>,Time,<<");">>]).

join([], _Sep) -> [];
join([H | T], Sep) -> [H, [[Sep, X] || X <- T]].



