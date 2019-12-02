-module(ejabberd_pb2xml_presence).

-include("message_pb.hrl").
-include("jlib.hrl").
-include("logger.hrl").

-export([parse_presence_message/1,make_presence_message/7]).

parse_presence_message(Pb_message) ->
    case catch message_pb:decode_presencemessage(Pb_message#protomessage.message) of
    Presence when is_record(Presence,presencemessage)  ->
    case ejabberd_pb2xml_public:get_presenceKey_type(Presence#presencemessage.definedkey) of
    'none' ->
        make_presence_message(Presence#presencemessage.key,Presence#presencemessage.value,
            Pb_message#protomessage.from,Pb_message#protomessage.to,message_pb:int_to_enum(signaltype,Pb_message#protomessage.signaltype),
            Presence#presencemessage.messageid,Presence#presencemessage.body);
       'PresenceKeyNotify' ->
        Xml = #xmlel{name = <<"presence">>, attrs = [
                 {<<"from">>, ejabberd_pb2xml_public:list_and_character_to_binary(Pb_message#protomessage.from)},
                 {<<"to">>, ejabberd_pb2xml_public:list_and_character_to_binary(Pb_message#protomessage.to)},
                             {<<"category">>, integer_to_binary(Presence#presencemessage.categorytype)},
                             {<<"data">>, ejabberd_pb2xml_public:list_and_character_to_binary((Presence#presencemessage.body)#messagebody.value)},
                             {<<"type">>, <<"notify">>}], 
                children = [#xmlel{name = <<"notify">>, attrs = [{<<"xmlns">>, <<"jabber:x:presence_notify">>}], children = []}]},
        {xmlstreamelement, Xml};
    V ->
        make_presence_message(V,
            Presence#presencemessage.value,
            Pb_message#protomessage.from,
            Pb_message#protomessage.to,
            Pb_message#protomessage.signaltype,
            Presence#presencemessage.messageid,
            Presence#presencemessage.body)
    end;
    _ -> false
    end.

make_presence_message("priority", Value, _From, _To, _Type, _ID, _Body) ->
    Xml = #xmlel{name = <<"presence">>, 
            attrs = [], 
            children = [#xmlel{name = <<"priority">>,attrs = [],children = [{'xmlcdata',list_to_binary(Value)}]}]},
    {xmlstreamelement, Xml};
make_presence_message("status", _Value, _From, _To, _Type, _ID, Body) ->
    Xml = #xmlel{name = <<"presence">>, attrs = [], 
                children = ejabberd_pb2xml_public:make_cdata_xmlels(Body#messagebody.headers)},

    {xmlstreamelement, Xml};
make_presence_message("verify_friend", _Value, _From, To, _Type, _ID, Body) ->
    Attrs = [{<<"xmlns">>,<<"jabber:x:verify_friend">>},
                {<<"to">>,ejabberd_pb2xml_public:list_and_character_to_binary(To)},
                {<<"type">>,<<"verify_friend">>}],
    Headers = Body#messagebody.headers,
    Attrs1 = lists:flatmap(fun(Header) ->
        case is_record(Header,stringheader) of
            true ->[{check_pb_stringHeader(Header), ejabberd_pb2xml_public:list_and_character_to_binary(Header#stringheader.value)}];
            _ -> []
        end
    end,Headers), 
    Xml = #xmlel{name = <<"presence">>, attrs = Attrs ++ Attrs1, children = []},
    {xmlstreamelement, Xml};
make_presence_message("manual_authentication_confirm", _Value, _From, To, _Type, _ID, Body) ->
    Attrs = [{<<"xmlns">>,<<"jabber:x:manual_authentication">>},
                {<<"to">>,ejabberd_pb2xml_public:list_and_character_to_binary(To)},
                {<<"type">>,<<"manual_authentication_confirm">>}],
    Headers = Body#messagebody.headers,
    Attrs1 = lists:flatmap(fun(Header) ->
        case is_record(Header,stringheader) of
            true -> [{check_pb_stringHeader(Header), ejabberd_pb2xml_public:list_and_character_to_binary(Header#stringheader.value)}];
            _ -> []
        end
    end,Headers), 
    Xml = #xmlel{name = <<"presence">>, attrs = Attrs ++ Attrs1, children = []},

    {xmlstreamelement, Xml};
make_presence_message(_,_,_,_,_,_,_) ->
    false.

check_pb_stringHeader(Header)  ->
    case ejabberd_pb2xml_public:get_header_definedkey(Header#stringheader.definedkey) of
        'none' -> list_to_binary(Header#stringheader.key);
        V -> V
    end.
