-module(ejabberd_pb2xml_message).

-include("message_pb.hrl").
-include("jlib.hrl").
-include("logger.hrl").

-export([parse_xmpp_message/1]).

parse_xmpp_message(Pb_message) ->
    case catch message_pb:decode_xmppmessage(Pb_message#protomessage.message) of
     Xmpp_msg when is_record(Xmpp_msg,xmppmessage)  ->
        Body = Xmpp_msg#xmppmessage.body,
        Headers = get_xmppmessage_headers(Body),
        Cdata = get_xmppmessage_value(Body),
        ?DEBUG("Pb_message#protomessage.signaltype ~p ~n",[Pb_message#protomessage.signaltype]), 
        make_xmpp_message(
                    Pb_message#protomessage.from,
                    Pb_message#protomessage.to,
                    Pb_message#protomessage.realfrom,
                    Pb_message#protomessage.realto,
                    message_pb:int_to_enum(signaltype,Pb_message#protomessage.signaltype),
                    Xmpp_msg#xmppmessage.messagetype,
                    message_pb:int_to_enum(clienttype,Xmpp_msg#xmppmessage.clienttype),
                    Xmpp_msg#xmppmessage.clientversion,
                    Xmpp_msg#xmppmessage.messageid,
                    Headers,
                    Cdata);
    _ -> false
    end.

get_xmppmessage_headers(Body) when is_record(Body,messagebody) ->
    Body#messagebody.headers;
get_xmppmessage_headers(_) ->
    [].

get_xmppmessage_value(Body) when is_record(Body,messagebody) ->
    Body#messagebody.value;
get_xmppmessage_value(_) ->
    <<"">>.

make_xmpp_message(From,To,RealFrom,RealTo,Type,Msg_type,Client_type,Client_ver,ID,Headers,Cdata) ->
    Channel_id =    get_header_definedkey('binary','StringHeaderTypeChannelId',Headers), 
    Ex_Info =       get_header_definedkey('unicode','StringHeaderTypeExtendInfo',Headers), 
    Backup_Info =   get_header_definedkey('unicode','StringHeaderTypeBackupInfo',Headers), 
    Read_Type =     get_header_definedkey('unicode','StringHeaderTypeReadType',Headers), 
    Auto_reply =    get_header_key("auto_reply",Headers),
    QchatID =    get_header_key("qchatid",Headers),
    ?DEBUG("the auto reply is ~p, the qchatid is ~p~n", [Auto_reply, QchatID]),

    Attrs = make_xmpp_message_attrs(ejabberd_pb2xml_public:list_and_character_to_binary(From),
            ejabberd_pb2xml_public:list_and_character_to_binary(To), 
            ejabberd_pb2xml_public:list_and_character_to_binary(RealFrom),
            ejabberd_pb2xml_public:list_and_character_to_binary(RealTo),
            Type,Client_type,Client_ver,list_to_binary(ID),Read_Type,Auto_reply,Channel_id,QchatID),

    Children = make_xmpp_body(Msg_type,list_to_binary(ID),Ex_Info,Backup_Info,
                unicode:characters_to_binary(Cdata)),

    {xmlstreamelement,#xmlel{name = <<"message">>, attrs = Attrs, children = Children}}.

make_xmpp_message_attrs(From, To, RealFrom, RealTo, Type, Client_type, Client_ver, _ID, Read_Type, Auto_reply, Channel_id, QchatID) ->
    Attrs = case From of 
        <<"">> ->
            [{<<"to">>,To},{<<"type">>,ejabberd_pb2xml_public:get_type(Type)},
            {<<"client_type">>,ejabberd_pb2xml_public:get_client_type(Client_type)},
            {<<"client_ver">>,integer_to_binary(Client_ver)}];
        
        _ ->    
            [{<<"from">>,From},{<<"to">>,To},{<<"type">>,ejabberd_pb2xml_public:get_type(Type)},
            {<<"client_type">>,ejabberd_pb2xml_public:get_client_type(Client_type)},
            {<<"client_ver">>,integer_to_binary(Client_ver)}]
    end,
    
    Attrs1 = add_xmpp_attrs(Attrs,<<"read_type">>,Read_Type),    
    Attrs2 = add_xmpp_attrs(Attrs1,<<"channelid">>,Channel_id),    
    Attrs3 = add_xmpp_spec_attrs(Attrs2,QchatID),  
    Attrs4 = add_xmpp_attrs(Attrs3,<<"realfrom">>,RealFrom),    
    Attrs5 = add_xmpp_attrs(Attrs4,<<"realto">>,RealTo), 
    add_xmpp_attrs(Attrs5, <<"auto_reply">>, Auto_reply).

make_xmpp_body(Msg_Type,Chat_id,Ex_Info,Backup_Info,Cdata) ->
    Attrs = [{<<"msgType">>,get_msg_type(Msg_Type)}],
    Attrs1 = add_xmpp_attrs(Attrs,<<"id">>,Chat_id),    
    Attrs2 = add_xmpp_attrs(Attrs1,<<"extendInfo">>,Ex_Info),    
    Attrs3 = add_xmpp_attrs(Attrs2,<<"backupinfo">>,Backup_Info),    

    [#xmlel{name = <<"body">>, attrs = Attrs3, children = [{'xmlcdata',Cdata}]}].

get_header_definedkey('binary',Key,Headers) ->    
    case lists:filter(fun(Str) ->    Str#stringheader.definedkey =:= Key end,Headers) of
       [] -> 'undefined';
       [H] -> list_to_binary(H#stringheader.value)
    end;
get_header_definedkey('unicode',Key,Headers) ->    
    case lists:filter(fun(Str) -> Str#stringheader.definedkey =:= Key end,Headers) of
       [] -> 'undefined';
       [H] -> unicode:characters_to_binary(H#stringheader.value)
    end.

get_header_key(Key,Headers) ->
    case lists:filter(fun(Str) -> Str#stringheader.key =:= Key end,Headers) of
    [Value] ->
        list_to_binary(Value#stringheader.value);
    [] -> undefined
    end.

add_xmpp_attrs(Attrs,Key,Value) ->
    case Value of
        'undefined' -> Attrs;
        _ -> [{Key,Value}] ++ Attrs
    end.

add_xmpp_spec_attrs(Attrs,QchatID) ->
    case QchatID of
        'undefined'-> Attrs;
        ID when ID =:= <<"4">>; ID =:= <<"5">> -> [{<<"qchatid">>, ID}| Attrs];
         _ -> Attrs
    end.

get_msg_type(Type) when is_integer(Type) ->
    integer_to_binary(Type);
get_msg_type(Type) when is_binary(Type) ->
    Type;
get_msg_type(_) -> <<"1">>. 
