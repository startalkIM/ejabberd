-module(ejabberd_xml2pb_message).

-include("message_pb.hrl").
-include("jlib.hrl").
-include("logger.hrl").

-export([xml2pb_msg/3,get_packet_time/2]).

encode_pb_xmpp_msg(Msg_Type,Client_Type,Read_Type,Client_Version,Channel_ID,Ex_INFO,Backup_Info,Carbon,Message,ID,Auto_Reply,Qchat_ID,Time, ErrCode) ->
    Msg_Body = #messagebody{headers = ejabberd_xml2pb_public:encode_pb_stringheaders(
                    [{<<"qchatid">>,Qchat_ID},
                     {<<"channelid">>,Channel_ID},
                     {<<"extendInfo">>,Ex_INFO},
                     {<<"backupinfo">>,Backup_Info},
                     {<<"read_type">>,Read_Type},
             {<<"auto_reply">>,Auto_Reply},
             {<<"errcode">>,ErrCode},
                     {<<"carbon_message">>,Carbon}]),
                 value  = Message},
    
    Xmpp = #xmppmessage{
        messagetype = Msg_Type,
          clienttype = Client_Type,
          clientversion = Client_Version,
          messageid = ID,
           body = Msg_Body,
           receivedtime = Time},
    message_pb:encode_xmppmessage(Xmpp).

struct_pb_xmpp_msg(BodyFlag,From,To,Type,PAttrs,BAttrs,Message,ID,Time, Packet) ->
    Msg = list_to_binary(do_struct_pb_xmpp_msg(BodyFlag,PAttrs,BAttrs,Message,ID,Time, Packet, Type)),
    RealFrom = proplists:get_value(<<"realfrom">>,PAttrs,undefined),
    RealTo = proplists:get_value(<<"realto">>,PAttrs,undefined),
    OriginFrom = proplists:get_value(<<"originfrom">>,PAttrs,undefined),
    OriginTo = proplists:get_value(<<"originto">>,PAttrs,undefined),
    OriginType = proplists:get_value(<<"origintype">>,PAttrs,undefined),
    SendJid = proplists:get_value(<<"sendjid">>,PAttrs,undefined),
    Pb_Msg = list_to_binary(ejabberd_xml2pb_public:encode_pb_protomessage(From,To,RealFrom,RealTo,OriginFrom, OriginTo, OriginType, ejabberd_xml2pb_public:set_type(Type),0,Msg, SendJid)),
        
    Opt = ejabberd_xml2pb_public:get_proto_header_opt(Pb_Msg),
    list_to_binary(ejabberd_xml2pb_public:encode_pb_protoheader(Opt,Pb_Msg)).


do_struct_pb_xmpp_msg(<<"nbodyStat">>,PAttrs,_BAttrs,_Message,_ID,Time, _Packet, _Type) ->
    Client_Ver = proplists:get_value(<<"client_ver">>,PAttrs,<<"0">>),
    encode_pb_xmpp_msg(1, message_pb:enum_to_int(clienttype, ejabberd_xml2pb_public:set_client_type(<<"">>)),
                           <<"">>, binary_to_integer(Client_Ver),
                           <<"">>,<<"">>,<<"">>, <<"">>,<<"">>,<<"">>,<<"">>,<<"">>,Time, <<"0">>);
do_struct_pb_xmpp_msg(_,PAttrs,BAttrs,Message,ID,Time, Packet, Type)  ->
    Qchat_ID = handle_msg_id(PAttrs),
    Client_Type = proplists:get_value(<<"client_type">>,PAttrs),
    Client_Ver = proplists:get_value(<<"client_ver">>,PAttrs,<<"0">>),
    Carbon_Flag = proplists:get_value(<<"carbon_message">>,PAttrs,<<"">>),
    Read_Type = proplists:get_value(<<"read_type">>,PAttrs,<<"">>),
    Auto_Reply = proplists:get_value(<<"auto_reply">>,PAttrs,<<"">>),
    
    Msg_Type = proplists:get_value(<<"msgType">>,BAttrs,<<"1">>),
    Ma_Type = proplists:get_value(<<"maType">>,BAttrs,<<"3">>),
    Channel_ID = proplists:get_value(<<"channelid">>,BAttrs,<<"">>),
    Ex_INFO = proplists:get_value(<<"extendInfo">>,BAttrs,<<"">>),
    Backup_Info = proplists:get_value(<<"backupinfo">>,BAttrs,<<"">>),
    ErrCode = case Type of
        <<"error">> ->
            case fxml:get_subtag(Packet,<<"error">>) of
                false -> <<"0">>;
                Error ->
                case proplists:get_value(<<"code">>, Error#xmlel.attrs) of
                    undefined -> <<"0">>;
                    Code -> Code
                end
            end;
        _ -> <<"0">>
    end, 
    
    %% 兼容老客户端
    ClientType = case Client_Type of
        undefined -> Ma_Type;
        _ -> Client_Type
    end,
    encode_pb_xmpp_msg(binary_to_integer(Msg_Type),
                       message_pb:enum_to_int(clienttype,ejabberd_xml2pb_public:set_client_type(ClientType)),
                       Read_Type, binary_to_integer(Client_Ver), 
                       Channel_ID, Ex_INFO, Backup_Info, 
                       Carbon_Flag, Message, ID, Auto_Reply, Qchat_ID,Time, ErrCode).
        
xml2pb_msg(From,To,Packet) ->
    case fxml:get_attr(<<"type">>,Packet#xmlel.attrs) of
    false ->
        <<"">>;
    {_Value, Type} ->
        case fxml:get_subtag(Packet,<<"body">>) of
        false ->
            case Type of 
            <<"stat">> ->
                struct_pb_xmpp_msg(<<"nbodyStat">>,From,To,<<"stat">>,Packet#xmlel.attrs,[],<<"">>,<<"">>,qtalk_public:get_exact_timestamp(), Packet); 
            <<"typing">> ->
                struct_pb_xmpp_msg(<<"typing">>,From,To,<<"typing">>,Packet#xmlel.attrs,[],<<"">>,<<"">>,qtalk_public:get_exact_timestamp(), Packet); 
            _ ->
                <<"">>
            end;
        Body -> 
            ID = case proplists:get_value(<<"id">>,Body#xmlel.attrs) of
                I when is_binary(I) ->
                    I;
                _ ->
                    list_to_binary("http_" ++ integer_to_list(random:uniform(65536)) ++ integer_to_list(qtalk_public:get_exact_timestamp())) 
                end,
            Message = fxml:get_subtag_cdata(Packet,<<"body">>),
            Time = get_packet_time(Packet,Packet#xmlel.attrs), 
            struct_pb_xmpp_msg(Type,From,To,Type,Packet#xmlel.attrs,Body#xmlel.attrs,Message,ID,Time, Packet)
        end
    end.


handle_msg_id(PAttrs) ->
    Qchat_ID = proplists:get_value(<<"qchatid">>,PAttrs,<<"">>),
    case Qchat_ID of
        <<"">> -> <<"">>;
        V when is_binary(V) -> V;
        _ -> <<"">>
    end.

get_packet_time(_Packet, PAttrs) ->
    case proplists:get_value(<<"msec_times">>,PAttrs) of
        undefined -> qtalk_public:get_exact_timestamp();
        T -> binary_to_integer(T)
    end.
