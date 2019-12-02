-module(revoke).

-export([revoke_message/3,revoke_groupchat_message/5]).

-include("jlib.hrl").
-include("logger.hrl").
revoke_message(From,To,Packet) ->
    case qtalk_public:is_conference_server(From#jid.lserver) of
        true -> ok;
        _ ->
            revoke_chat_message(From,To,Packet)
    end.

revoke_chat_message(From,To,Packet) ->
    Args = get_revoke_message_args(Packet),
    Msg_id = proplists:get_value("messageId",Args),
    Msec = fxml:get_tag_attr_s(<<"msec_times">>, Packet),        
    Revoke_pkt = qtalk_public:make_revoke_packet(Msg_id,From,To,Msec),
    update_msg_by_id(From#jid.lserver,From,To,Revoke_pkt,Msg_id).


revoke_groupchat_message(Server,From,To,Packet, _UL) ->
    Args = get_revoke_message_args(Packet),
    Msg_id = proplists:get_value("messageId",Args),
    Msec = fxml:get_tag_attr_s(<<"msec_times">>, Packet),
    Revoke_pkt = qtalk_public:make_revoke_packet(Msg_id,From,To,Msec),
    ?DEBUG("revoke muc message ~p,Msg_id ~p  ~n",[Revoke_pkt,Msg_id]),
    update_muc_msg_by_id(Server,From,To,Packet,Revoke_pkt,Msg_id).

get_revoke_message_args(Packet) ->
    Body = fxml:get_subtag_cdata(Packet, <<"body">>),
    case rfc4627:decode(Body) of  
        {ok,{obj,Args},[]} -> Args;
        _ -> []
    end.

update_msg_by_id(Server,From,To,Packet,Msg_id) ->
    case catch ejabberd_sql:sql_query(Server, [<<"select m_from,m_to,m_body ,msg_id ,extract(epoch from create_time)::bigint from msg_history where msg_id = '">>,Msg_id,<<"';">>]) of
        {selected, _ , [[F,T,B,ID,Time]]}  ->
            Msec = fxml:get_tag_attr_s(<<"msec_times">>, Packet),
            Time1 = binary_to_integer(Msec),
            Time2 = binary_to_integer(Time),
            if (Time1/1000 - Time2 < 120) ->
                case catch ejabberd_sql:sql_query(Server, [<<"insert into revoke_msg_history(m_from,m_to,m_body,msg_id) values ('">>,F,<<"','">>,T,<<"','">>, ejabberd_sql:escape(B),<<"','">>,ID,<<"');">>]) of
                    {updated,_} ->
                         case catch ejabberd_sql:sql_query(Server,
                             [<<"update msg_history set m_from = '">>,ejabberd_sql:escape(From#jid.luser),
                              <<"',from_host = '">>,ejabberd_sql:escape(From#jid.lserver),
                              <<"',m_to = '">>,ejabberd_sql:escape(To#jid.luser),
                              <<"',to_host = '">>,ejabberd_sql:escape(To#jid.lserver),
                              <<"',m_body = '">>,ejabberd_sql:escape( fxml:element_to_binary(Packet)),
                              <<"',create_time = ">>,qtalk_public:pg2timestamp(Time1),
                              <<" where msg_id = '">>,Msg_id,<<"';">>]) of
                             {updated,_} -> ok;
                             _ -> ok
                         end;
                    _ -> ok
                end;
            true -> ok
            end;
        _ -> ok
    end.

update_muc_msg_by_id(Server, From, _To, _SPacket, Packet, Msg_id) ->
    case catch ejabberd_sql:sql_query(Server, [<<"select nick,muc_room_name,packet,msg_id,extract(epoch from create_time)::bigint from muc_room_history where msg_id = '">>, Msg_id,<<"';">>]) of
        {selected, _ , [[N,M,P,ID,Time]]}  ->
            Msec = fxml:get_tag_attr_s(<<"msec_times">>, Packet),
            Time1 = binary_to_integer(Msec),
            Time2 = binary_to_integer(Time),
            Nick1 = qtalk_public:get_nick(From#jid.luser),
            FromLUser = From#jid.luser,
            FromLServer = From#jid.lserver,
            FNick =  <<FromLUser/binary, "_", FromLServer/binary>>,
            case  (Time1/1000 - Time2 < 120 andalso (N =:= FNick orelse Nick1 =:= N)) of
                true ->
                     %%send_kafka_msg(From,To,SPacket, <<"groupchat">>),
                     %%true;
                    case catch ejabberd_sql:sql_query(Server, [<<"insert into revoke_msg_history(m_from,m_to,m_body,msg_id) values ('">>,N,<<"','">>,M,<<"','">>, ejabberd_sql:escape(P),<<"','">>,ID,<<"');">>]) of
                        {updated,_} ->
                            catch  ejabberd_sql:sql_query(Server, 
                                                [<<"update muc_room_history set packet = '">>,
                                                    ejabberd_sql:escape( fxml:element_to_binary(Packet)), <<"' , create_time = ">>,
                                                    qtalk_public:pg2timestamp(Time1),<<" where msg_id = '">>,Msg_id,<<"';">>]),
                            true;
                        _ -> true
                    end;
                _  -> false
            end;
        _ ->
            false
    end.
