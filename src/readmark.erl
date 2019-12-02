-module(readmark).

-export([readmark_message/3]).

-include("jlib.hrl").
-include("logger.hrl").

readmark_message(From, To, Packet) ->
        send_push_message(From, To, Packet),
        case fxml:get_tag_attr_s(<<"read_type">>, Packet) of                                                                                                                                                                                                                 
        <<"0">> ->
                mark_all_msg(From,To,Packet);
        <<"1">> ->
                chat_readmark_msg(From,To,Packet, <<"3">>);
        <<"2">> ->
                groupchat_readmark_msg(From,To,Packet);
        <<"3">> ->
                chat_readmark_msg(From,To,Packet, <<"1">>);
        <<"4">> ->
                chat_readmark_msg(From,To,Packet, <<"3">>);
        _ ->
                ok
        end.

mark_all_msg(From,_To,Packet) ->
  Body = fxml:get_subtag_cdata(Packet, <<"body">>),
  case rfc4627:decode(Body) of    
  {ok,{obj,Args},[]} ->
    Time = 
      case proplists:get_value("T",Args) of
      undefined ->
        proplists:get_value("t",Args);
      V ->
        V
      end,
    Time_msg = erlang:trunc(Time / 1000),
    update_all_msg(From,Time,Time_msg);
  _ ->
    ok
  end.

update_all_msg(From,Time,TimeStamp) ->
  Time_msg = qtalk_public:format_time(TimeStamp),
  Sql1 = [<<"update msg_history set read_flag = 3 where m_to = '">>,From#jid.luser, 
      <<"' and read_flag < 3 and create_time < '">>,Time_msg,<<"';">>],
  Sql2 = [<<"update muc_room_users set date = ">>,integer_to_binary(Time),<<" where username = '">>,From#jid.luser, <<"' and date < ">>,integer_to_binary(Time),<<";">>],
  catch ejabberd_sql:sql_query(From#jid.lserver,Sql1),
  catch ejabberd_sql:sql_query(From#jid.lserver,Sql2).

chat_readmark_msg(From, _To, Packet, Flag) ->
  Body =  fxml:get_subtag_cdata(Packet, <<"body">>),
  LServer = From#jid.lserver,
  T = fxml:get_tag_attr_s(<<"msec_times">>, Packet),
  IDs = case rfc4627:decode(Body) of  
  {ok,Json,[]} ->
    lists:map(fun({obj,Args}) ->
        proplists:get_value("id", Args, "http_test")
    end,Json);
  _ ->
    []
  end,
  do_update_chat_readmark_msg(LServer, IDs, Flag, T).

do_update_chat_readmark_msg(LServer, IDs, Flag, T) ->
    Updates  = lists:foldl(fun(ID,Acc) ->
        case Acc of
            [] -> [<<" msg_id = '">> ,ID, <<"'">>] ;
            _ -> [<<" msg_id = '">> ,ID, <<"' or ">>] ++ Acc 
        end
    end, [], IDs),

    case catch ejabberd_sql:sql_query(LServer, [<<"update msg_history set read_flag = '">>, Flag, <<"', update_time = ">>,  qtalk_public:pg2timestamp(T), <<" where (">>,Updates,<<") and read_flag < '">>, Flag, <<"';">>]) of 
        {updated,1} -> ok;
        Error -> ?DEBUG("Error ~p ~n",[Error])
    end.

groupchat_readmark_msg(_From,To,Packet) ->
  Body = fxml:get_subtag_cdata(Packet, <<"body">>),
    LServer = To#jid.lserver,
    User = To#jid.user,
  T = fxml:get_tag_attr_s(<<"msec_times">>, Packet),
    case rfc4627:decode(Body) of
  {ok,Json,[]} ->
      lists:foreach(fun({obj,Args}) ->
          Muc  = proplists:get_value("id",Args),
            Time = proplists:get_value("t",Args),
            update_muc_readmark(LServer,Muc,User,Time, T) end,Json);
     _ ->
            ok
    end.

update_muc_readmark(Host,Muc,User,Time, T) when is_integer(Time) ->
    ITime = case catch Time < 1800000000 of
        true ->  Time * 1000;
        _ ->   Time
    end,

    T_Time = ITime div 100 *100,
    Bin_T = integer_to_binary(ITime),
    Bin_T2 = integer_to_binary(T_Time),
    case catch ejabberd_sql:sql_query(Host, [<<"update muc_room_users set date = ">>,Bin_T,<<", update_time = ">>,  qtalk_public:pg2timestamp(T), <<" where muc_name = '">>, Muc, <<"' and username = '">>,User,<<"' and  date < ">>,Bin_T2,<<";">>]) of 
        {updated,1} -> ok;
        A ->
            Sql = [<<"update muc_room_users set date = ">>,Bin_T,<<", update_time = ">>, T, <<" where muc_name = '">>,Muc,<<"' and username = '">>,User,<<"' and  date < ">>,Bin_T2,<<";">>],
            ?DEBUG("A ~p,~p ~n",[A,list_to_binary(Sql)])
    end;
update_muc_readmark(_Host, Muc, User, Time, _) ->
    ?INFO_MSG("update_readmark_date Muc ~p,User ~p, Time is ~p ~n",[Muc,User,Time]).

send_push_message(From, To, Packet) ->
    PushUrl = ejabberd_config:get_option(push_url, fun(Url)-> Url end, undefined),
    case PushUrl of
        undefined -> ok;
        _ -> do_send_push_message(From, To, Packet, PushUrl)
    end.

do_send_push_message(From, To, Packet, PushUrl) ->
    Type = fxml:get_tag_attr_s(<<"type">>, Packet),
    LFrom = From#jid.user,
    LTo = To#jid.user,
    From_host = From#jid.lserver,
    To_host = To#jid.lserver,
    LBody = fxml:element_to_binary(Packet),
    LId = fxml:get_tag_attr_s(<<"id">>, fxml:get_subtag(Packet,<<"body">>)),
    Time = binary_to_integer(fxml:get_tag_attr_s(<<"msec_times">>, Packet)),
    MsgContent = rfc4627:encode({obj, [{"m_from", LFrom},
                                       {"from_host", From_host},
                                       {"m_to", LTo},
                                       {"to_host", To_host},
                                       {"m_body", LBody},
                                       {"create_time", Time},
                                       {"type", <<"readmark">>},
                                       {"msg_id", LId}]}),
    ok.
