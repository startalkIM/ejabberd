-module(ejabberd_xml2pb_public).

-include("message_pb.hrl").
-include("jlib.hrl").
-include("logger.hrl").
-export([encode_messagebody/2,encode_pb_protomessage/5,encode_pb_protomessage/7,encode_pb_protomessage/10, encode_pb_protomessage/11, encode_pb_protoheader/2,encode_pb_stringheaders/1]).
-export([set_type/1,set_client_type/1,set_msg_type/1,get_proto_header_opt/1]).
-export([set_presenceKey_type/1,set_iqKey_type/1]).


encode_pb_protoheader(Opt,Pro_msg) ->
     Msg = case Opt of
         1 -> zlib:gzip(Pro_msg);
         5 -> tea_crypto:encrypt(Pro_msg,<<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>);
         _ -> Pro_msg
     end,
     message_pb:encode_protoheader(#protoheader{options = Opt,message = Msg}).

encode_pb_protomessage(From,To,Type,Opts,Msg) ->
    Message = #protomessage{options = Opts,signaltype = message_pb:enum_to_int(signaltype,Type),from = From,to = To,message = Msg},
    message_pb:encode_protomessage(Message).

encode_pb_protomessage(From,To,RealFrom,RealTo,Type,Opts,Msg) ->
    Message = #protomessage{options = Opts,signaltype = message_pb:enum_to_int(signaltype,Type),from = From,to = To,realfrom = RealFrom,realto = RealTo,message = Msg},
    message_pb:encode_protomessage(Message).

encode_pb_protomessage(From,To,RealFrom,RealTo,OriginFrom, OriginTo, OriginType, Type,Opts,Msg) ->
    Message = #protomessage{options = Opts,signaltype = message_pb:enum_to_int(signaltype,Type),from = From,to = To,realfrom = RealFrom,realto = RealTo, originfrom = OriginFrom, originto = OriginTo, origintype = OriginType, message = Msg},
    message_pb:encode_protomessage(Message).

encode_pb_protomessage(From,To,RealFrom,RealTo,OriginFrom, OriginTo, OriginType, Type,Opts,Msg, SendJid) ->
    Message = #protomessage{options = Opts,signaltype = message_pb:enum_to_int(signaltype,Type),from = From,to = To,realfrom = RealFrom,realto = RealTo, originfrom = OriginFrom, originto = OriginTo, sendjid = SendJid, origintype = OriginType, message = Msg},
    message_pb:encode_protomessage(Message).

encode_pb_stringheader([]) ->
    [];
encode_pb_stringheader({_,<<"">>}) ->
    [];
encode_pb_stringheader({<<"">>,_}) ->
    [];
encode_pb_stringheader({_,undefined}) ->
    [];
encode_pb_stringheader({K,V}) ->
    case set_header_definedkey(K) of
        'none' -> [#stringheader{key = K,value = V}];
        Dv -> [#stringheader{definedkey = Dv,value = V}]
    end.

encode_pb_stringheaders(Headers) when is_list(Headers) ->
    lists:flatmap(fun(Header) ->
            encode_pb_stringheader(Header) end, Headers);
encode_pb_stringheaders(_Headers) ->
    [].

encode_messagebody(Headers,Value) ->
    Pb_headers = encode_pb_stringheaders(Headers), 
    #messagebody{headers = Pb_headers,value = Value}.
    
set_msg_type(Type) ->
    case catch message_pb:int_to_enum(messagetype,binary_to_integer(Type)) of
        E when is_atom(E); is_integer(E) -> E;
        _ -> 'MessageTypeText'
    end. 

set_client_type(Type) ->
    case Type of
        T when T =:= <<"ClientTypeMac">>; T =:= <<"1">> -> 'ClientTypeMac';
        T when T =:= <<"ClientTypeiOS">>; T =:= <<"2">> -> 'ClientTypeiOS';
        T when T =:= <<"ClientTypePC">>; T =:= <<"3">> -> 'ClientTypePC';
        T when T =:= <<"ClientTypeAndroid">>; T =:= <<"4">> -> 'ClientTypeAndroid';
        T when T =:= <<"ClientTypeLinux">>; T =:= <<"5">> -> 'ClientTypeLinux';
        T when T =:= <<"ClientTypeWeb">>; T =:= <<"6">> -> 'ClientTypeWeb';
        _ -> 'ClientTypePC'
    end.

set_type(Type) ->
    case Type of 
        <<"chat">> -> 'SignalTypeChat';
        <<"groupchat">> -> 'SignalTypeGroupChat';
        <<"note">> -> 'SignalTypeNote';
        <<"readmark">> -> 'SignalTypeReadmark';
        <<"mstat">> -> 'SignalTypeMState';
        <<"carbon">> -> 'SignalTypeCarbon';
        <<"subscription">> -> 'SignalTypeSubscription';
        <<"headline">> -> 'SignalTypeHeadline';
        <<"revoke">> -> 'SignalTypeRevoke';
        <<"webrtc">> -> 'SignalTypeWebRtc';
        <<"consult">> -> 'SignalTypeConsult';
        <<"typing">> -> 'SignalTypeTyping';
        <<"encrypt">> -> 'SignalTypeEncryption';
        <<"error">> -> 'SignalTypeError';
        <<"collection">> -> 'SignalTypeCollection';
        _ -> 'SignalTypeChat'
    end.

set_header_definedkey(Key) ->
    case Key of
        <<"chatid">> -> 'StringHeaderTypeChatId';
        <<"channelid">> -> 'StringHeaderTypeChannelId';
        <<"extendInfo">> -> 'StringHeaderTypeExtendInfo';
        <<"backupinfo">> -> 'StringHeaderTypeBackupInfo';
        <<"read_type">> -> 'StringHeaderTypeReadType';
        <<"jid">> -> 'StringHeaderTypeJid';
        <<"real_jid">> -> 'StringHeaderTypeRealJid';
        <<"invite_jid">> -> 'StringHeaderTypeInviteJid';
        <<"del_jid">> -> 'StringHeaderTypeDeleleJid';
        <<"nick">> -> 'StringHeaderTypeNick';
        <<"title">> -> 'StringHeaderTypeTitle';
        <<"pic">> -> 'StringHeaderTypePic';
        <<"version">> -> 'StringHeaderTypeVersion';
        <<"method">> -> 'StringHeaderTypeMethod';
        <<"body">> -> 'StringHeaderTypeBody';
        <<"affiliation">> -> 'StringHeaderTypeAffiliation';
        <<"type">> -> 'StringHeaderTypeType';
        <<"result">> -> 'StringHeaderTypeResult';
        <<"reason">> -> 'StringHeaderTypeReason';
        <<"role">> -> 'StringHeaderTypeRole';
        <<"domain">> -> 'StringHeaderTypeDomain';
        <<"status">> -> 'StringHeaderTypeStatus';
        <<"code">> -> 'StringHeaderTypeCode';
        <<"cdata">> -> 'StringHeaderTypeCdata';
        <<"time_value">> -> 'StringHeaderTypeTimeValue';
        <<"key_value">> -> 'StringHeaderTypeKeyValue';
        <<"name">> -> 'StringHeaderTypeName';
        <<"host">> -> 'StringHeaderTypeHost';
        <<"question">> -> 'StringHeaderTypeQuestion';
        <<"answer">> -> 'StringHeaderTypeAnswer';
        <<"friends">> -> 'StringHeaderTypeFriends';
        <<"value">> -> 'StringHeaderTypeValue';
        <<"masked_user">> -> 'StringHeaderTypeMaskedUuser';
        <<"key">> -> 'StringHeaderTypeKey';
        <<"mode">> -> 'StringHeaderTypeMode';
        <<"carbon_message">> -> 'StringHeaderTypeCarbon';
        _ -> 'none'
    end.
    
get_proto_header_opt(Pro_msg) ->
    if size(Pro_msg) > 200 -> 1;
    true -> 5
    end.

        
set_iqKey_type(Key) ->
    case Key of
        <<"result">> -> 'IQKeyResult';
        <<"error">> -> 'IQKeyError';
        _ -> 'none'
    end.

set_presenceKey_type(Key) ->
    case Key of
        <<"result">> -> 'PresenceKeyResult';
        _ -> 'none'
    end.
