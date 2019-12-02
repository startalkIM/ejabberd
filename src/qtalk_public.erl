%%%----------------------------------------------------------------------
%%%%%%%%% File    : qtalk_public.erl
%%%%%% add qtalk c2s function
%%%%%%

-module(qtalk_public).

-export([get_nick/1,get_nick/2,get_lan_platform/1,get_wlan_platform/1]).
-export([get_timestamp/0,get_exact_timestamp/0,datetime_to_timestamp/1,timestamp_to_datetime/1,timestamp_to_datetime_utc1/1,get_month_first_day/1]).
-export([get_timestamp_of_end_day/1,get_timestamp_of_start_day/1,get_datetime_before_day/2]).
-export([concat/3,get_pg_default_val/2,format_time/1,make_default_presence_packet/1]).
-export([add_body_id/2,make_revoke_packet/4]).
-export([pg2timestamp/1,is_conference_server/1,check_pid_alive/1,to_integer/1]).
-export([make_message_packet/4,check_user_reg_muc/3,clear_ets_muc_room_users/3,check_user_reg_muc_local/4]).
-export([get_xml_attrs_id/1,get_xml_attrs_from/2,get_xml_attrs_to/2,get_sub_xmlns_name/1]).
-export([tokens_jid/1,remove_subtags_by_name/2,deal_timestamp/1]).
-export([clear_redis_user_key/3,add_attrs_realfrom/3]).
-export([get_host_by_conhost/1,send_recv_repley/4]).
-export([get_errcode_by_reason/1,get_reason_by_errcode/1]).
-export([strtime2timestamp/1,kick_error_login_user/2]).
-export([send_navversion/4]).
-export([get_default_host/0, get_default_domain/0]).


-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-define(DAY_TIMESTAMP,86400).

%%%%%%%%--------------------------------------------------------------------
%%%%%%%% @date 2017-03
%%%%%%%% 获取用户昵称，和订阅者昵称
%%%%%%%%--------------------------------------------------------------------
get_nick(User) ->
	get_nick(User, get_default_host()).
		

get_nick(User,Host) ->
    case ejabberd_sql:sql_query(Host, [<<"select user_name from host_users left join host_info on host_users.host_id = host_info.id where user_id='">>, User, <<"' and host='">>, Host, <<"';">>]) of
        {selected,[<<"user_name">>], [[Name]]} -> Name;
        _ -> <<User/binary, "_", Host/binary>>
    end.

%%%%%%%%--------------------------------------------------------------------
%%%%%%%% @date 2017-03
%%%%%%%% 根据Resource检查平台
%%%%%%%%--------------------------------------------------------------------

get_lan_platform(Resource) ->
    case str:str(Resource,<<"QIM_PC">>) of
    0 ->
        <<"MAC">>;
    _ ->
        <<"QIM_PC">>
    end.
   
get_wlan_platform(Resource) -> 
    case str:str(Resource,<<"iPhone">>) of
    0 ->
        <<"Android">>;
    _ ->
        <<"iPhone">>
    end. 

%%%%%%%%%%%--------------------------------------------------------------------
%%%%%%%%%%% @date 2017-03
%%%%%%%%%%% 时间处理函数
%%%%%%%%%%%--------------------------------------------------------------------

get_timestamp() ->
    {MegaSecs, Secs,_MicroSec} = os:timestamp(),
    MegaSecs * 1000000 + Secs.

get_exact_timestamp() ->
    {MegaSecs, Secs,MicroSec} = os:timestamp(),
    1000000000 * MegaSecs + Secs * 1000 + MicroSec div 1000.

deal_timestamp(Time) ->
    {MegaSecs, Secs,_MicroSec} = Time,
    MegaSecs * 1000000 + Secs.

datetime_to_timestamp(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) -
        calendar:datetime_to_gregorian_seconds({{1970,1,1}, {8,0,0}}).

timestamp_to_datetime(Timestamp) ->
    calendar:gregorian_seconds_to_datetime(Timestamp +
        calendar:datetime_to_gregorian_seconds({{1970,1,1}, {8,0,0}})).

timestamp_to_datetime_utc1(Timestamp) ->
    calendar:gregorian_seconds_to_datetime(Timestamp +
              calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}})).

get_month_first_day(Timestamp) ->
    case timestamp_to_datetime(Timestamp) of
    {{Year,Month,_Day},{_,_,_}} ->
        datetime_to_timestamp({{Year,Month,1},{0,0,0}});
    _ ->
        0
    end.

get_datetime_before_day(Timestamp,Day) ->
    Timestamp - ?DAY_TIMESTAMP * Day.

get_timestamp_of_end_day(Timestamp) ->
    case timestamp_to_datetime(Timestamp)  of
    {{Year,Month,Day},{_,_,_}} ->
        datetime_to_timestamp({{Year,Month,Day},{23,59,59}});
    _ ->
        0
    end.


get_timestamp_of_start_day(Timestamp) ->
    case  timestamp_to_datetime(Timestamp)  of
    {{Year,Month,Day},{_,_,_}} ->
        datetime_to_timestamp({{Year,Month,Day},{0,0,0}});
     _ ->
            0
    end.


strtime2timestamp(Str) ->
    case size(Str) of
    17 ->
        [Date, Time] = str:tokens(Str, <<"T">>),
        Y = binary_to_integer(str:substr(Date,1,4)),
        M = binary_to_integer(str:substr(Date,5,2)),
        D = binary_to_integer(str:substr(Date,7,2)),
        HH = binary_to_integer(str:substr(Time,1,2)),
        MM = binary_to_integer(str:substr(Time,4,2)),
        SS = binary_to_integer(str:substr(Time,7,2)),
        datetime_to_timestamp({{Y,M,D},{HH,MM,SS}}) * 1000;
    _ ->
        qtalk_public:get_exact_timestamp()
    end.

%%%%%%%%%%%--------------------------------------------------------------------
%%%%%%%%%%% @date 2017-03
%%%%%%%%%%% 处理sent_repley
%%%%%%%%%%%--------------------------------------------------------------------

make_sent_packet(JID,ID, Now) ->
   fxml:to_xmlel(
        {xmlel ,<<"message">>, [{<<"type">>,<<"mstat">>},{<<"msec_times">>, integer_to_binary(Now)}, {<<"to">>,jlib:jid_to_string(jlib:jid_remove_resource(JID))}],
                     [{xmlel,<<"body">>,[{<<"id">>,ID},{<<"stat">>,<<"sent">>}],[]}]}).

%%%%%%%%%%%--------------------------------------------------------------------
%%%%%%%%%%% @date 2017-03
%%%%%%%%%%% 添加body id
%%%%%%%%%%%--------------------------------------------------------------------
add_body_id(Packet,ID) ->
    ID_Packet = fxml:replace_tag_attr(<<"id">>,ID,fxml:get_subtag(Packet,<<"body">>)),
    case catch remove_subtags_by_name(Packet,<<"body">>) of
    Body when is_record(Body,xmlel) ->
        fxml:append_subtags(Body,[ID_Packet]);
    Res ->
	?DEBUG("Res ~p ~n",[Res]),
        Packet
    end.

%%%%%%%%%%%--------------------------------------------------------------------
%%%%%%%%%%% @date 2017-03
%%%%%%%%%%% str concat:use for concat like( <<"Muc">>,<<"@">>,<<"conference.host">>)
%%%%%%%%%%%--------------------------------------------------------------------
concat(A,B,C) ->
    <<A/binary,B/binary,C/binary>>.

%%%%%%%%%%% 获取pgsql中空值默认值
%%%%%%%%%%%--------------------------------------------------------------------
get_pg_default_val(null,Default) ->
    Default;
get_pg_default_val(V,_) ->
    V.


pg2timestamp(Time) when is_binary(Time)->
    do_pg2timestamp(binary_to_integer(Time));
pg2timestamp(Time) when is_list(Time)->
    do_pg2timestamp(list_to_integer(Time));
pg2timestamp(Time) when is_integer(Time) ->
    do_pg2timestamp(Time);
pg2timestamp(_Time)  ->
     <<"error">>.

do_pg2timestamp(T) ->
    Time =
        if T > 1000000000000 ->
            T /1000;
        true ->
            T/1
        end,
    Str = str:concat(float_to_binary(Time,[{decimals, 3}]),<<"-0.000005)::timestamptz(3)">>),
    str:concat(<<"to_timestamp(">>,Str).

is_conference_server(Server) ->
    str:str(Server,<<"conference">>) =/= 0.

get_host_by_conhost(Domain) ->
	case str:str(Domain,<<"conference.">>) of
	0 ->
		Domain;
	N when is_integer(N) ->
		str:substr(Domain,12,size(Domain) - 11);
	_ ->
		Domain
	end.


check_pid_alive(Pid) ->
    if node() =:= node(Pid) ->
        is_process_alive(Pid);
    true ->
        true
    end.

make_revoke_packet(ID,From,To, Msec) ->
    {{Year, Month, Day},{Hour, Minute, Second}} = calendar:now_to_universal_time(os:timestamp()),
    E_info = rfc4627:encode({obj, [{<<"messageId">>, ID}, {<<"fromId">>, jlib:jid_to_string(From)}]}),
    fxml:to_xmlel(
            {xmlel  ,<<"message">>, [{<<"type">>,<<"revoke">>},{<<"to">>,jlib:jid_to_string(To)}, {<<"msec_times">>, Msec}],
                [{xmlel,<<"body">>,[{<<"id">>,ID},{<<"msgType">>,<<"-1">>},{<<"extendInfo">>,E_info}],
                    [{xmlcdata, <<"[撤销一条消息]"/utf8>>}]},
                 {stime,<<"stime">>,[{<<"xmlns">>, ?NS_TIME91},
                                     {<<"stamp">>,
                        iolist_to_binary(io_lib:format("~4..0w~2..0w~2..0wT~2..0w:~2..0w:~2..0w",[Year, Month, Day, Hour, Minute,Second]))}
                                     ],[]}]}).

make_message_packet(Type,Msg,Extend_Info,undefined) ->
    Bid = list_to_binary("http" ++ binary_to_list(randoms:get_string()) ++ integer_to_list(qtalk_public:get_exact_timestamp())),
    fxml:to_xmlel(
            {xmlel  ,<<"message">>, [{<<"type">>,Type}],
                [{xmlel,<<"body">>,[{<<"id">>,Bid},{<<"msgType">>,<<"1">>},{<<"extendInfo">>,Extend_Info}],[{xmlcdata, Msg}]}]});
make_message_packet(Type,Msg,Extend_Info,Msg_Type) ->
    Bid = list_to_binary("http" ++ binary_to_list(randoms:get_string()) ++ integer_to_list(qtalk_public:get_exact_timestamp())),
    fxml:to_xmlel(
            {xmlel  ,<<"message">>, [{<<"type">>,Type}],
                [{xmlel,<<"body">>,[{<<"id">>,Bid},{<<"msgType">>,Msg_Type},{<<"extendInfo">>,Extend_Info}],[{xmlcdata, Msg}]}]}).


check_user_reg_muc(Server,Muc,User) ->
    case catch qtalk_sql:get_muc_user_host(Server,Muc,User) of
    {selected,_, [[_]]}  ->
        true;
    _ ->
        false
    end.

check_user_reg_muc_local(Muc,Domain,User,Host) ->
    case str:str(Domain,Host) of
    N when N > 0 ->
        case catch qtalk_sql:get_muc_user_host(Host,Muc,User) of
        {selected,_, [[_]]}  ->
            true;
         _ ->
            false
        end;
    _ ->
        true
    end.

clear_redis_user_key(Server,User,Resource) ->
    case catch mod_redis:hash_get(1,User,Resource) of
    {ok,undefined} ->
                    ok;
    {ok,Key} ->
                    mod_redis:hash_del(1,User,Resource),
                        ?INFO_MSG("delete redis 2 key User ~p ,Key ~p ~n",[User,Key]),
    		    FUser = qtalk_public:concat(User,<<"@">>,Server),
                    catch mod_redis:hash_del(2,FUser,Key),
                    mod_redis:hash_del(2,User,Key);
     _ ->
                    ok
     end.

clear_ets_muc_room_users(Muc,User,Server) ->
    case catch ets:lookup(muc_users,Muc) of
    [{_,UL}] when is_list(UL) andalso  UL /= [] ->
        case lists:delete({User,Server},UL) of
        UL ->
            ok;
        UDL when is_list(UDL) ->
            case UDL of
            [] ->
                ets:delete(muc_users,Muc);
            _ ->
                ets:insert(muc_users,{Muc,UDL})
            end;
        _ ->
            ok
        end;
    _ ->
        ok
    end.


to_integer(Str) when is_binary(Str) ->
    binary_to_integer(Str);
to_integer(Str) when is_list(Str) ->
    list_to_integer(Str);
to_integer(Str) when is_integer(Str) ->
    Str;
to_integer(_) ->
    0.


format_time(Time) when is_binary(Time)->
    handle_time(binary_to_integer(Time));
format_time(Time) when is_integer(Time)->
    handle_time(Time);
format_time(Time) when is_list(Time) ->
    handle_time(list_to_integer(Time));
format_time(_Time) ->
    <<"error">>.

handle_time(Time) ->
    case Time > 3000000000 of
    true ->
        Msec = Time rem 1000,
        Time1 = Time div 1000,
        do_format_time(Time1,Msec);
    _ ->
        do_format_time(Time,0)
    end.

do_format_time(Time,Msec) ->
    {{Y,M,D},{H,Mi,S}} = timestamp_to_datetime(Time),
    list_to_binary(
            [integer_to_list(Y),"-",integer_to_list(M),"-",integer_to_list(D)," ",
                       integer_to_list(H),":",integer_to_list(Mi),":",integer_to_list(S),msec_time(Msec),integer_to_list(Msec)]).

msec_time(Msec) when Msec > 100 ->
    ".";
msec_time(Msec) when Msec > 10 ->
    ".0";
msec_time(_Msec) ->
    ".00".

make_default_presence_packet(ItemAttrs) ->
    #xmlel{name = <<"presence">>,
        attrs = [{<<"priority">>,<<"5">>},{<<"version">>,<<"2">>}],
            children = [
                #xmlel{name = <<"x">>,
                 attrs =[{<<"xmlns">>,
                        ?NS_MUC_USER}],
                 children = [
                        #xmlel{name = <<"item">>,attrs = ItemAttrs,children =  []},
                        #xmlel{name = <<"status">>,attrs = [{<<"code">>, <<"110">>}],children = []}]}]}.



get_xml_attrs_id(Packet) ->
    case  fxml:get_attr(<<"id">>,Packet#xmlel.attrs) of
    false ->
            integer_to_binary(get_timestamp());
    {_Value,I} ->
            I
    end.

get_xml_attrs_to(Packet,DT) ->
    case  catch fxml:get_attr(<<"to">>,Packet#xmlel.attrs) of
    {_Value,To} ->
        To;
    _ ->
        To = jlib:jid_to_string(jlib:make_jid(DT)),
        To
    end.
get_xml_attrs_from(Packet,DF) ->
    case  catch fxml:get_attr(<<"from">>,Packet#xmlel.attrs) of
    {_Value,From} ->
            From;
    _ ->
        JID = jlib:jid_to_string(jlib:make_jid(DF)),
        JID
    end.


get_sub_xmlns_name(#xmlel{children = Els}) ->
    get_sub_xmlns_name1(Els).

get_sub_xmlns_name1( [El | Els]) ->
    case is_record(El,xmlel) of
    true ->
        case fxml:get_attr(<<"xmlns">>, El#xmlel.attrs) of
        {value, XMLNS} ->
            {El#xmlel.name,XMLNS};
        _ -> get_sub_xmlns_name1(Els)
        end;
    _ ->
        get_sub_xmlns_name1(Els)
    end;
get_sub_xmlns_name1([]) -> false.



tokens_jid(Jid) ->
    case catch str:tokens(Jid,<<"@">>) of
    [Jid] ->
        Jid;
    [] ->
        Jid;
    L when is_list(L) ->
        lists:nth(1,L);
    _ ->
        Jid
    end.

remove_subtags_by_name(#xmlel{name = TagName, attrs = TagAttrs, children = Els},
  Name) ->
    #xmlel{name = TagName, attrs = TagAttrs,
        children = remove_subtags1(Els, [], Name)}.

remove_subtags1([], NewEls, _Name) ->
    lists:reverse(NewEls);
remove_subtags1([El | Els], NewEls, Name) ->
    case El of
      #xmlel{name = Name} ->
        remove_subtags1(Els, NewEls, Name);
      _ -> remove_subtags1(Els, [El | NewEls], Name)
    end.

add_attrs_realfrom(Packet,RealFrom,Time) ->
    #xmlel{name = Name, attrs = Attrs, children =  Els} = Packet,
    Attrs1 = [{<<"sendjid">>,RealFrom},{<<"realfrom">>,RealFrom}] ++ proplists:delete(<<"sendjid">>, proplists:delete(<<"realfrom">>, Attrs)),
    case lists:keyfind(<<"msec_times">>, 1, Attrs1) of
       {<<"msec_times">>, _} -> #xmlel{name = Name, attrs = Attrs1, children = Els};
       _ -> #xmlel{name = Name, attrs = [{<<"msec_times">>,integer_to_binary(Time)}] ++ Attrs1, children = Els}
    end.

%%%%%%%%%%%%%%-----------------------------------------------------------
%%%%%%%%%%%%%% @date 2017-03
%%%%%%%%%%%%%% 发送sent repley
%%%%%%%%%%%%%%-----------------------------------------------------------
send_recv_repley(Packet,From,Mod, StateData) ->
    Now = get_exact_timestamp(),
    NPacket = fxml:replace_tag_attr(<<"msec_times">>, integer_to_binary(Now), Packet),
    case catch fxml:get_subtag_cdata(NPacket, <<"body">>) of
    <<>> ->
        NPacket;
    _ ->
        Type = fxml:get_tag_attr_s(<<"type">>, NPacket),
        case  Type =:= <<"error">> orelse Type =:= <<"headline">> orelse Type =:= <<"subscription">> of
        true ->
            NPacket;
        _ ->
            case catch fxml:get_tag_attr_s(<<"id">>,fxml:get_subtag(NPacket,<<"body">>)) of
            <<"">> ->
                Bid = list_to_binary("add_" ++ integer_to_list(random:uniform(65536)) ++ integer_to_list(qtalk_public:get_exact_timestamp())),
                RePacket = make_sent_packet(From,Bid, Now),
                Mod:send_element(StateData, RePacket),
                Packet222 = add_body_id(NPacket,Bid),
                Packet222;
            ID ->
                RePacket = make_sent_packet(From,ID, Now),
                Mod:send_element(StateData, RePacket),
                NPacket
            end
        end
    end.

get_errcode_by_reason(Reason)  ->
    case catch Reason of
    <<"User pid carsh">> ->
        100;
    <<"Hash postion error">> ->
        101;
    <<"Kicked by admin">> ->
        200;
    _ ->  
        201
    end.

get_reason_by_errcode(Code) ->
    case Code of
    100 ->
        <<"User pid carsh">>;
    201 ->
        <<"Kicked by admin">>;
    101 ->
        <<"Hash postion error">>;
    _ ->
        <<"Unknown error">>
    end.
        
kick_error_login_user(Error,Pid) ->
        case Error of
        <<"login_error_mac">> ->
            Pid ! {kick, 101, []},
            true;
        _ ->
            false
        end.


send_navversion(User, Server, Res, _Version) ->
    Ver = 
    case catch mod_redis:str_get(15,<<"navversion">>) of
    {ok,undefined} ->
        <<"10000">>;
    {ok,B} when is_binary(B)  ->
        B;
    _ ->
        <<"10000">>
    end,
        
    From = jlib:jid_to_string({User,Server,<<"">>}),
    To = jlib:jid_to_string({User,Server,Res}),
    Data = rfc4627:encode({obj, [{<<"navversion">>,Ver}]}),
    ejabberd_rpc_presence:get_notify_presence(From, To, <<"3">>, list_to_binary(Data)).  

get_default_host() ->
    ejabberd_config:get_option(default_host, fun(Host)-> Host end, <<"localhost">>). 

get_default_domain() ->
    ejabberd_config:get_option(default_domain, fun(Domain)-> Domain end, <<"conference.localhost">>). 
