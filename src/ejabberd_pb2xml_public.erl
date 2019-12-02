-module(ejabberd_pb2xml_public).

-include("message_pb.hrl").
-include("jlib.hrl").

-export([get_potoheader_base_pb/1,get_potomessage_base_pbheader/1,get_potomessage/1]).
-export([get_client_type/1,get_msg_type/1,get_type/1]).
-export([make_attrs_xmlels/1,make_attrs_xmlel/4,make_cdata_xmlels/1,make_cdata_xmlel/1]).
-export([list_and_character_to_binary/1]).

-export([encode_pb_size/1,get_presenceKey_type/1,get_iqKey_type/1,get_header_definedkey/1]).

get_potoheader_base_pb(Pb) ->
	case catch message_pb:decode_protoheader(Pb) of
	   Pb_header when is_record(Pb_header,protoheader) -> Pb_header;
	   _ -> false
	end.

get_potomessage_base_pbheader(Pb_header) ->
	case catch Pb_header#protoheader.options of
	1 ->
		%%zlib
		Pb_Msg = zlib:gunzip(Pb_header#protoheader.message),
		decode_protomessage(Pb_Msg);
	5 ->
		%%tea
		Pb_Msg = tea_crypto:decrypt(Pb_header#protoheader.message,<<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>),
		decode_protomessage(Pb_Msg);
	_ ->
		decode_protomessage(Pb_header#protoheader.message)
	end.
		
decode_protomessage(Pb_Msg) ->
	case catch message_pb:decode_protomessage(Pb_Msg) of
	   Proto_message when is_record(Proto_message,protomessage) -> Proto_message;
	   _ -> false
	end.

get_potomessage(Pb) ->
	case catch get_potoheader_base_pb(Pb) of
        false ->false;
        Pb_header ->
            case catch get_potomessage_base_pbheader(Pb_header) of
            false -> false;
            Pb_message -> Pb_message
            end
	end.

get_msg_type(Type) ->
    case catch message_pb:enum_to_int(messagetype,Type) of
    I when is_integer(I) ->
        integer_to_binary(I);
    _ ->
        <<"1">>
    end. 

get_client_type(Type) ->
	case Type of
        'ClientTypeMac' -> <<"ClientTypeMac">>;
        'ClientTypeiOS' -> <<"ClientTypeiOS">>;
        'ClientTypePC' -> <<"ClientTypePC">>;
        'ClientTypeAndroid' -> <<"ClientTypeAndroid">>;
        'ClientTypeLinux' -> <<"ClientTypeLinux">>;
        'ClientTypeWeb' -> <<"ClientTypeWeb">>;
        _ -> <<"ClientTypePC">>
	end.

get_type(Type) ->
	case Type of 
	   'SignalTypeChat' -> <<"chat">>;
	   'SignalTypeGroupChat' -> <<"groupchat">>;
        'SignalTypeMState'-> <<"mstat">>;
        'SignalTypeReadmark'-> <<"readmark">>;
        'SignalTypeTyping' -> <<"typing">>;
        'SignalTypeHeadline' -><<"headline">>;
        'SignalTypeWebRtc' -> <<"webrtc">>;
        'SignalTypeSubscription' -> <<"subscription">>;
        'SignalTypeRevoke' -> <<"revoke">>;
        'SignalTypeConsult' -> <<"consult">>;
        'SignalTypeEncryption' -> <<"encrypt">>;
        _ -> <<"normal">>
	end.

make_attrs_xmlel(Name,Headers,XMLNS,Child) ->
	Attrs = lists:flatmap(fun(Header) ->
		case is_record(Header,stringheader) of
		true -> handle_header_attrs(Header);
		_ -> []
		end
    end, Headers),

	Attrs1 = case XMLNS of 
		<<"">> -> Attrs;
		_ -> [{<<"xmlns">>,XMLNS}] ++ Attrs
    end,

	#xmlel{name = Name, attrs = Attrs1,children = Child}.

handle_header_attrs(Header) ->
    case get_header_definedkey(Header#stringheader.definedkey) of
        'none' -> [{list_to_binary(Header#stringheader.key), list_and_character_to_binary(Header#stringheader.value)}];
        V -> [{V,list_and_character_to_binary(Header#stringheader.value)}]
    end.

make_attrs_xmlels(Bodys) ->
	lists:flatmap(fun(Body) ->
        case is_record(Body,messagebody) of
			true -> [make_attrs_xmlel(list_to_binary(Body#messagebody.value),Body#messagebody.headers,<<"">>,[])];
			_ -> []
        end
    end, Bodys).

make_cdata_xmlels(Headers) ->
	lists:flatmap(fun(Header) ->
			make_cdata_xmlel(Header)
    end, Headers).

make_cdata_xmlel(Header) when is_record(Header,stringheader) ->
    make_header_xmlel(Header);
make_cdata_xmlel(_) ->
	[].

make_header_xmlel(Header) ->
    case get_header_definedkey(Header#stringheader.definedkey) of
        'none' ->   
	        [#xmlel{name = list_to_binary(Header#stringheader.key),attrs = [],children =  [{'xmlcdata',
                    list_and_character_to_binary(Header#stringheader.value)}]}];
        V when is_binary(V)->
	        [#xmlel{name = V,attrs = [],children =  [{'xmlcdata',
                    list_and_character_to_binary(Header#stringheader.value)}]}];
        _ -> []
    end.
            
list_and_character_to_binary(Value) when is_list(Value) ->              
    case catch  re:run(Value, "[\x{4e00}-\x{9fff}]+", [unicode]) of
        {match,_} -> unicode:characters_to_binary(Value);
        _ -> iolist_to_binary(Value)
    end;
list_and_character_to_binary(Value) ->
    Value.

encode_pb_size(Size) ->
    if Size > 128 ->
        T1 = Size bor  16#80,
        S1 = Size bsr 7,
        if S1 > 128 ->
            T2 = S1 bor  16#80,
            S2  = S1 bsr 7,
            if S2 > 128 ->
                T3 = S2 bor 16#80,
                S3  = S2 bsr 7,
                if  S3 > 128 ->
                    T4 = S3 bor 16#80,
                    S4  = S3 bsr 7,
                    <<T1,T2,T3,T4,S4>>;
                true ->
                    <<T1,T2,T3,S3>>
                end;
            true ->
                <<T1,T2,S2>>
            end;
        true ->
            <<T1,S1>>
        end;
    true ->
        <<Size>>
    end.

get_header_definedkey(Key) ->
    case Key of
        'StringHeaderTypeChatId' -> <<"chatid">>;
        'StringHeaderTypeChannelId' -> <<"channelid">>;
        'StringHeaderTypeExtendInfo' -> <<"extendInfo">>;
        'StringHeaderTypeBackupInfo' -> <<"backupinfo">>;
        'StringHeaderTypeReadType' -> <<"read_type">>;
        'StringHeaderTypeJid' -> <<"jid">>;
        'StringHeaderTypeRealJid' -> <<"real_jid">>;
        'StringHeaderTypeInviteJid' -> <<"invite_jid">>;
        'StringHeaderTypeDeleleJid' -> <<"del_jid">>;
        'StringHeaderTypeNick' -> <<"nick">>;
        'StringHeaderTypeTitle' -> <<"title">>;
        'StringHeaderTypePic' -> <<"pic">>;
        'StringHeaderTypeVersion' -> <<"version">>;
        'StringHeaderTypeMethod' -> <<"method">>;
        'StringHeaderTypeBody' -> <<"body">>;
        'StringHeaderTypeAffiliation' -> <<"affiliation">>;
        'StringHeaderTypeType' -> <<"type">>;
        'StringHeaderTypeResult' -> <<"result">>;
        'StringHeaderTypeReason' -> <<"reason">>;
        'StringHeaderTypeRole' -> <<"role">>;
        'StringHeaderTypeDomain' -> <<"domain">>;
        'StringHeaderTypeStatus' -> <<"status">>;
        'StringHeaderTypeCode' -> <<"code">>;
        'StringHeaderTypeCdata' -> <<"cdata">>;
        'StringHeaderTypeTimeValue' -> <<"time_value">>;
        'StringHeaderTypeKeyValue' -> <<"key_value">>;
        'StringHeaderTypeName' -> <<"name">>;
        'StringHeaderTypeHost' -> <<"host">>;
        'StringHeaderTypeQuestion' -> <<"question">>;
        'StringHeaderTypeAnswer' -> <<"answer">>;
        'StringHeaderTypeFriends' -> <<"friends">>;
        'StringHeaderTypeValue' -> <<"value">>;
        'StringHeaderTypeMaskedUuser' -> <<"masked_user">>;
        'StringHeaderTypeKey' -> <<"key">>;
        'StringHeaderTypeMode' -> <<"mode">>;
        'StringHeaderTypeCarbon' -> <<"carbon_message">>;
        _ -> 'none'
    end.

get_iqKey_type(Key) ->
    case Key of
        'IQKeyBind' -> "BIND";
        'IQKeyMucCreate' -> "CREATE_MUC";
        'IQKeyMucCreateV2' -> "MUC_CREATE";
        'IQKeyMucInviteV2' -> "MUC_INVITE_V2";
        'IQKeyGetMucUser' -> "GET_MUC_USER";
        'IQKeySetMucUser' -> "SET_MUC_USER";
        'IQKeyDelMucUser' -> "DEL_MUC_USER";
        'IQKeyAddUserSubscribe' -> "ADD_USER_SUBSCRIBE";
        'IQKeyDelUserSubscribe' -> "DEL_USER_SUBSCRIBE";
        'IQKeyGetUserSubScribe' -> "GET_USER_SUBSCRIBE";
        'IQKeySetUserSubScribeV2' -> "SET_USER_SUBSCRIBE_V2";
        'IQKeyGetUserSubScribeV2' -> "GET_USER_SUBSCRIBE_V2";
        'IQKeyGetVerifyFriendOpt' -> "GET_USER_OPT";
        'IQKeySetVerifyFriendOpt' -> "SET_USER_OPT";    
        'IQKeyGetUserFriend' -> "GET_USER_FRIEND";
        'IQKeyDelUserFriend' -> "DEL_USER_FRIEND";
        'IQKeyGetUserKey' -> "GET_USER_KEY";
        'IQKeyGetUserMask' -> "GET_USER_MASK";
        'IQKeySetUserMask' -> "SET_USER_MASK";
        'IQKeyCancelUSerMask' -> "CANCEL_USER_MASK";
        'IQKeySetAdmin' -> "SET_ADMIN";
        'IQKeySetMember' -> "SET_MEMBER";
        'IQKeyCancelMember' -> "CANCEL_MEMBER";
        'IQKeyGetUserMucs' -> "USER_MUCS";
        'IQKeyDestroyMuc' -> "DESTROY_MUC";
        'IQKeyPing' -> "PING";
        'IQKeyAddPush' -> "ADD_PUSH";
        'IQKeyCancelPush' -> "CANCEL_PUSH";
        'IQKeyResult' -> "result";
        'IQKeyError' -> "error";
        'IQKeyGetVUser' -> "GET_VUSER";
        'IQKeyGetVUserRole' -> "GET_VUSER_ROLE";
        'IQKeyStartSession' -> "RUSER_START_SESSION";
        'IQKeyEndSession' -> "RUSER_END_SESSION";
        _ -> 'none'
    end.

get_presenceKey_type(Key) ->
    case Key of
        'PresenceKeyPriority' -> "priority";
        'PresenceKeyVerifyFriend' -> "verify_friend";
        'PresenceKeyManualAuthenticationConfirm' -> "manual_authentication_confirm";
        'PresenceKeyResult' -> "result";
        'PresenceKeyError' -> "error";
        'PresenceKeyNotify' -> 'PresenceKeyNotify';
        _ -> 'none'
    end.
