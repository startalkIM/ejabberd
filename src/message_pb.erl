-file("src/message_pb.erl", 1).

-module(message_pb).

-export([encode_xmppmessage/1, decode_xmppmessage/1,
	 delimited_decode_xmppmessage/1,
	 encode_presencemessage/1, decode_presencemessage/1,
	 delimited_decode_presencemessage/1, encode_iqmessage/1,
	 decode_iqmessage/1, delimited_decode_iqmessage/1,
	 encode_messagebody/1, decode_messagebody/1,
	 delimited_decode_messagebody/1, encode_protomessage/1,
	 decode_protomessage/1, delimited_decode_protomessage/1,
	 encode_responsefailure/1, decode_responsefailure/1,
	 delimited_decode_responsefailure/1,
	 encode_responsesucceeded/1, decode_responsesucceeded/1,
	 delimited_decode_responsesucceeded/1,
	 encode_capability/1, decode_capability/1,
	 delimited_decode_capability/1, encode_userconnect/1,
	 decode_userconnect/1, delimited_decode_userconnect/1,
	 encode_streamend/1, decode_streamend/1,
	 delimited_decode_streamend/1, encode_proceedtls/1,
	 decode_proceedtls/1, delimited_decode_proceedtls/1,
	 encode_starttls/1, decode_starttls/1,
	 delimited_decode_starttls/1, encode_streambegin/1,
	 decode_streambegin/1, delimited_decode_streambegin/1,
	 encode_welcomemessage/1, decode_welcomemessage/1,
	 delimited_decode_welcomemessage/1, encode_authmessage/1,
	 decode_authmessage/1, delimited_decode_authmessage/1,
	 encode_protoheader/1, decode_protoheader/1,
	 delimited_decode_protoheader/1, encode_packagelength/1,
	 decode_packagelength/1,
	 delimited_decode_packagelength/1, encode_stringheader/1,
	 decode_stringheader/1, delimited_decode_stringheader/1,
	 encode_messagekeyvalue/1, decode_messagekeyvalue/1,
	 delimited_decode_messagekeyvalue/1]).

-export([has_extension/2, extension_size/1,
	 get_extension/2, set_extension/3]).

-export([decode_extensions/1]).

-export([encode/1, decode/2, delimited_decode/2]).

-export([int_to_enum/2, enum_to_int/2]).

-record(xmppmessage,
	{messagetype, clienttype, clientversion, namespace, key,
	 value, messageid, header, body, receivedtime,
	 transfertime, headers, bodys}).

-record(presencemessage,
	{namespace, key, value, messageid, header, body,
	 receivedtime, transfertime, headers, bodys, definedkey,
	 categorytype}).

-record(iqmessage,
	{namespace, key, value, messageid, header, body,
	 receivedtime, transfertime, headers, bodys,
	 definedkey}).

-record(messagebody, {headers, value, bodys}).

-record(protomessage,
	{options, signaltype, from, to, message, realfrom,
	 realto, originfrom, originto, origintype, sendjid}).

-record(responsefailure, {code, msgid, error, body}).

-record(responsesucceeded, {code, msgid, info, body}).

-record(capability, {version, bodys}).

-record(userconnect, {domain, version}).

-record(streamend, {reason, code}).

-record(proceedtls, {}).

-record(starttls, {}).

-record(streambegin, {domain, version, bodys}).

-record(welcomemessage,
	{domain, version, user, sockmod}).

-record(authmessage,
	{mechanism, method, msgid, authkey, otherbody}).

-record(protoheader,
	{version, options, optionlist, length, content,
	 message}).

-record(packagelength, {length}).

-record(stringheader, {params, key, value, definedkey}).

-record(messagekeyvalue, {key, value}).

encode([]) -> [];
encode(Records) when is_list(Records) ->
    delimited_encode(Records);
encode(Record) -> encode(element(1, Record), Record).

encode_xmppmessage(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_xmppmessage(Record)
    when is_record(Record, xmppmessage) ->
    encode(xmppmessage, Record).

encode_presencemessage(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_presencemessage(Record)
    when is_record(Record, presencemessage) ->
    encode(presencemessage, Record).

encode_iqmessage(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_iqmessage(Record)
    when is_record(Record, iqmessage) ->
    encode(iqmessage, Record).

encode_messagebody(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_messagebody(Record)
    when is_record(Record, messagebody) ->
    encode(messagebody, Record).

encode_protomessage(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_protomessage(Record)
    when is_record(Record, protomessage) ->
    encode(protomessage, Record).

encode_responsefailure(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_responsefailure(Record)
    when is_record(Record, responsefailure) ->
    encode(responsefailure, Record).

encode_responsesucceeded(Records)
    when is_list(Records) ->
    delimited_encode(Records);
encode_responsesucceeded(Record)
    when is_record(Record, responsesucceeded) ->
    encode(responsesucceeded, Record).

encode_capability(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_capability(Record)
    when is_record(Record, capability) ->
    encode(capability, Record).

encode_userconnect(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_userconnect(Record)
    when is_record(Record, userconnect) ->
    encode(userconnect, Record).

encode_streamend(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_streamend(Record)
    when is_record(Record, streamend) ->
    encode(streamend, Record).

encode_proceedtls(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_proceedtls(Record)
    when is_record(Record, proceedtls) ->
    encode(proceedtls, Record).

encode_starttls(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_starttls(Record)
    when is_record(Record, starttls) ->
    encode(starttls, Record).

encode_streambegin(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_streambegin(Record)
    when is_record(Record, streambegin) ->
    encode(streambegin, Record).

encode_welcomemessage(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_welcomemessage(Record)
    when is_record(Record, welcomemessage) ->
    encode(welcomemessage, Record).

encode_authmessage(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_authmessage(Record)
    when is_record(Record, authmessage) ->
    encode(authmessage, Record).

encode_protoheader(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_protoheader(Record)
    when is_record(Record, protoheader) ->
    encode(protoheader, Record).

encode_packagelength(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_packagelength(Record)
    when is_record(Record, packagelength) ->
    encode(packagelength, Record).

encode_stringheader(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_stringheader(Record)
    when is_record(Record, stringheader) ->
    encode(stringheader, Record).

encode_messagekeyvalue(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_messagekeyvalue(Record)
    when is_record(Record, messagekeyvalue) ->
    encode(messagekeyvalue, Record).

encode(messagekeyvalue, Records)
    when is_list(Records) ->
    delimited_encode(Records);
encode(messagekeyvalue, Record) ->
    [iolist(messagekeyvalue, Record)
     | encode_extensions(Record)];
encode(stringheader, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(stringheader, Record) ->
    [iolist(stringheader, Record)
     | encode_extensions(Record)];
encode(packagelength, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(packagelength, Record) ->
    [iolist(packagelength, Record)
     | encode_extensions(Record)];
encode(protoheader, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(protoheader, Record) ->
    [iolist(protoheader, Record)
     | encode_extensions(Record)];
encode(authmessage, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(authmessage, Record) ->
    [iolist(authmessage, Record)
     | encode_extensions(Record)];
encode(welcomemessage, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(welcomemessage, Record) ->
    [iolist(welcomemessage, Record)
     | encode_extensions(Record)];
encode(streambegin, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(streambegin, Record) ->
    [iolist(streambegin, Record)
     | encode_extensions(Record)];
encode(starttls, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(starttls, Record) ->
    [iolist(starttls, Record) | encode_extensions(Record)];
encode(proceedtls, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(proceedtls, Record) ->
    [iolist(proceedtls, Record)
     | encode_extensions(Record)];
encode(streamend, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(streamend, Record) ->
    [iolist(streamend, Record) | encode_extensions(Record)];
encode(userconnect, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(userconnect, Record) ->
    [iolist(userconnect, Record)
     | encode_extensions(Record)];
encode(capability, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(capability, Record) ->
    [iolist(capability, Record)
     | encode_extensions(Record)];
encode(responsesucceeded, Records)
    when is_list(Records) ->
    delimited_encode(Records);
encode(responsesucceeded, Record) ->
    [iolist(responsesucceeded, Record)
     | encode_extensions(Record)];
encode(responsefailure, Records)
    when is_list(Records) ->
    delimited_encode(Records);
encode(responsefailure, Record) ->
    [iolist(responsefailure, Record)
     | encode_extensions(Record)];
encode(protomessage, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(protomessage, Record) ->
    [iolist(protomessage, Record)
     | encode_extensions(Record)];
encode(messagebody, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(messagebody, Record) ->
    [iolist(messagebody, Record)
     | encode_extensions(Record)];
encode(iqmessage, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(iqmessage, Record) ->
    [iolist(iqmessage, Record) | encode_extensions(Record)];
encode(presencemessage, Records)
    when is_list(Records) ->
    delimited_encode(Records);
encode(presencemessage, Record) ->
    [iolist(presencemessage, Record)
     | encode_extensions(Record)];
encode(xmppmessage, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(xmppmessage, Record) ->
    [iolist(xmppmessage, Record)
     | encode_extensions(Record)].

encode_extensions(_) -> [].

delimited_encode(Records) ->
    lists:map(fun (Record) ->
		      IoRec = encode(Record),
		      Size = iolist_size(IoRec),
		      [protobuffs:encode_varint(Size), IoRec]
	      end,
	      Records).

iolist(messagekeyvalue, Record) ->
    [pack(1, optional,
	  with_default(Record#messagekeyvalue.key, none), string,
	  []),
     pack(2, optional,
	  with_default(Record#messagekeyvalue.value, none),
	  string, [])];
iolist(stringheader, Record) ->
    [pack(1, repeated,
	  with_default(Record#stringheader.params, none),
	  messagekeyvalue, []),
     pack(2, optional,
	  with_default(Record#stringheader.key, none), string,
	  []),
     pack(3, optional,
	  with_default(Record#stringheader.value, none), string,
	  []),
     pack(4, optional,
	  with_default(Record#stringheader.definedkey, none),
	  stringheadertype, [])];
iolist(packagelength, Record) ->
    [pack(1, optional,
	  with_default(Record#packagelength.length, none), int32,
	  [])];
iolist(protoheader, Record) ->
    [pack(1, optional,
	  with_default(Record#protoheader.version, none), int32,
	  []),
     pack(2, optional,
	  with_default(Record#protoheader.options, none), int32,
	  []),
     pack(3, repeated,
	  with_default(Record#protoheader.optionlist, none),
	  int32, []),
     pack(4, optional,
	  with_default(Record#protoheader.length, none), int32,
	  []),
     pack(5, optional,
	  with_default(Record#protoheader.content, none), string,
	  []),
     pack(6, optional,
	  with_default(Record#protoheader.message, none), bytes,
	  [])];
iolist(authmessage, Record) ->
    [pack(1, optional,
	  with_default(Record#authmessage.mechanism, none),
	  string, []),
     pack(2, optional,
	  with_default(Record#authmessage.method, none), string,
	  []),
     pack(3, optional,
	  with_default(Record#authmessage.msgid, none), string,
	  []),
     pack(4, optional,
	  with_default(Record#authmessage.authkey, none), string,
	  []),
     pack(5, optional,
	  with_default(Record#authmessage.otherbody, none),
	  messagebody, [])];
iolist(welcomemessage, Record) ->
    [pack(1, optional,
	  with_default(Record#welcomemessage.domain, none),
	  string, []),
     pack(2, optional,
	  with_default(Record#welcomemessage.version, none),
	  string, []),
     pack(3, optional,
	  with_default(Record#welcomemessage.user, none), string,
	  []),
     pack(4, optional,
	  with_default(Record#welcomemessage.sockmod, none),
	  string, [])];
iolist(streambegin, Record) ->
    [pack(1, optional,
	  with_default(Record#streambegin.domain, none), string,
	  []),
     pack(2, optional,
	  with_default(Record#streambegin.version, none), string,
	  []),
     pack(3, repeated,
	  with_default(Record#streambegin.bodys, none),
	  messagebody, [])];
iolist(starttls, _Record) -> [];
iolist(proceedtls, _Record) -> [];
iolist(streamend, Record) ->
    [pack(1, optional,
	  with_default(Record#streamend.reason, none), string,
	  []),
     pack(2, optional,
	  with_default(Record#streamend.code, none), int32, [])];
iolist(userconnect, Record) ->
    [pack(1, optional,
	  with_default(Record#userconnect.domain, none), string,
	  []),
     pack(2, optional,
	  with_default(Record#userconnect.version, none), string,
	  [])];
iolist(capability, Record) ->
    [pack(1, optional,
	  with_default(Record#capability.version, none), string,
	  []),
     pack(2, optional,
	  with_default(Record#capability.bodys, none),
	  messagebody, [])];
iolist(responsesucceeded, Record) ->
    [pack(1, optional,
	  with_default(Record#responsesucceeded.code, none),
	  int32, []),
     pack(2, optional,
	  with_default(Record#responsesucceeded.msgid, none),
	  string, []),
     pack(3, optional,
	  with_default(Record#responsesucceeded.info, none),
	  string, []),
     pack(4, optional,
	  with_default(Record#responsesucceeded.body, none),
	  messagebody, [])];
iolist(responsefailure, Record) ->
    [pack(1, optional,
	  with_default(Record#responsefailure.code, none), int32,
	  []),
     pack(2, optional,
	  with_default(Record#responsefailure.msgid, none),
	  string, []),
     pack(3, optional,
	  with_default(Record#responsefailure.error, none),
	  string, []),
     pack(4, optional,
	  with_default(Record#responsefailure.body, none),
	  messagebody, [])];
iolist(protomessage, Record) ->
    [pack(1, optional,
	  with_default(Record#protomessage.options, none), int32,
	  []),
     pack(2, required,
	  with_default(Record#protomessage.signaltype, none),
	  int32, []),
     pack(3, optional,
	  with_default(Record#protomessage.from, none), string,
	  []),
     pack(4, optional,
	  with_default(Record#protomessage.to, none), string, []),
     pack(5, optional,
	  with_default(Record#protomessage.message, none), bytes,
	  []),
     pack(6, optional,
	  with_default(Record#protomessage.realfrom, none),
	  string, []),
     pack(7, optional,
	  with_default(Record#protomessage.realto, none), string,
	  []),
     pack(8, optional,
	  with_default(Record#protomessage.originfrom, none),
	  string, []),
     pack(9, optional,
	  with_default(Record#protomessage.originto, none),
	  string, []),
     pack(10, optional,
	  with_default(Record#protomessage.origintype, none),
	  string, []),
     pack(11, optional,
	  with_default(Record#protomessage.sendjid, none), string,
	  [])];
iolist(messagebody, Record) ->
    [pack(1, repeated,
	  with_default(Record#messagebody.headers, none),
	  stringheader, []),
     pack(2, optional,
	  with_default(Record#messagebody.value, none), string,
	  []),
     pack(3, repeated,
	  with_default(Record#messagebody.bodys, none),
	  messagebody, [])];
iolist(iqmessage, Record) ->
    [pack(1, optional,
	  with_default(Record#iqmessage.namespace, none), string,
	  []),
     pack(2, optional,
	  with_default(Record#iqmessage.key, none), string, []),
     pack(3, optional,
	  with_default(Record#iqmessage.value, none), string, []),
     pack(4, optional,
	  with_default(Record#iqmessage.messageid, none), string,
	  []),
     pack(5, optional,
	  with_default(Record#iqmessage.header, none),
	  stringheader, []),
     pack(6, optional,
	  with_default(Record#iqmessage.body, none), messagebody,
	  []),
     pack(7, optional,
	  with_default(Record#iqmessage.receivedtime, none),
	  int64, []),
     pack(8, optional,
	  with_default(Record#iqmessage.transfertime, none),
	  int64, []),
     pack(9, repeated,
	  with_default(Record#iqmessage.headers, none),
	  stringheader, []),
     pack(10, repeated,
	  with_default(Record#iqmessage.bodys, none), messagebody,
	  []),
     pack(11, optional,
	  with_default(Record#iqmessage.definedkey, none),
	  iqmessagekeytype, [])];
iolist(presencemessage, Record) ->
    [pack(1, optional,
	  with_default(Record#presencemessage.namespace, none),
	  string, []),
     pack(2, optional,
	  with_default(Record#presencemessage.key, none), string,
	  []),
     pack(3, optional,
	  with_default(Record#presencemessage.value, none),
	  string, []),
     pack(4, optional,
	  with_default(Record#presencemessage.messageid, none),
	  string, []),
     pack(5, optional,
	  with_default(Record#presencemessage.header, none),
	  stringheader, []),
     pack(6, optional,
	  with_default(Record#presencemessage.body, none),
	  messagebody, []),
     pack(7, optional,
	  with_default(Record#presencemessage.receivedtime, none),
	  int64, []),
     pack(8, optional,
	  with_default(Record#presencemessage.transfertime, none),
	  int64, []),
     pack(9, repeated,
	  with_default(Record#presencemessage.headers, none),
	  stringheader, []),
     pack(10, repeated,
	  with_default(Record#presencemessage.bodys, none),
	  messagebody, []),
     pack(11, optional,
	  with_default(Record#presencemessage.definedkey, none),
	  presencekeytype, []),
     pack(12, optional,
	  with_default(Record#presencemessage.categorytype, none),
	  int32, [])];
iolist(xmppmessage, Record) ->
    [pack(1, required,
	  with_default(Record#xmppmessage.messagetype, none),
	  int32, []),
     pack(2, required,
	  with_default(Record#xmppmessage.clienttype, none),
	  int32, []),
     pack(3, required,
	  with_default(Record#xmppmessage.clientversion, none),
	  int64, []),
     pack(4, optional,
	  with_default(Record#xmppmessage.namespace, none),
	  string, []),
     pack(5, optional,
	  with_default(Record#xmppmessage.key, none), string, []),
     pack(6, optional,
	  with_default(Record#xmppmessage.value, none), string,
	  []),
     pack(7, optional,
	  with_default(Record#xmppmessage.messageid, none),
	  string, []),
     pack(8, optional,
	  with_default(Record#xmppmessage.header, none),
	  stringheader, []),
     pack(9, optional,
	  with_default(Record#xmppmessage.body, none),
	  messagebody, []),
     pack(10, optional,
	  with_default(Record#xmppmessage.receivedtime, none),
	  int64, []),
     pack(11, optional,
	  with_default(Record#xmppmessage.transfertime, none),
	  int64, []),
     pack(12, repeated,
	  with_default(Record#xmppmessage.headers, none),
	  stringheader, []),
     pack(13, repeated,
	  with_default(Record#xmppmessage.bodys, none),
	  messagebody, [])].

with_default(Default, Default) -> undefined;
with_default(Val, _) -> Val.

pack(_, optional, undefined, _, _) -> [];
pack(_, repeated, undefined, _, _) -> [];
pack(_, repeated_packed, undefined, _, _) -> [];
pack(_, repeated_packed, [], _, _) -> [];
pack(FNum, required, undefined, Type, _) ->
    exit({error,
	  {required_field_is_undefined, FNum, Type}});
pack(_, repeated, [], _, Acc) -> lists:reverse(Acc);
pack(FNum, repeated, [Head | Tail], Type, Acc) ->
    pack(FNum, repeated, Tail, Type,
	 [pack(FNum, optional, Head, Type, []) | Acc]);
pack(FNum, repeated_packed, Data, Type, _) ->
    protobuffs:encode_packed(FNum, Data, Type);
pack(FNum, _, Data, _, _) when is_tuple(Data) ->
    [RecName | _] = tuple_to_list(Data),
    protobuffs:encode(FNum, encode(RecName, Data), bytes);
pack(FNum, _, Data, Type, _)
    when Type =:= bool;
	 Type =:= int32;
	 Type =:= uint32;
	 Type =:= int64;
	 Type =:= uint64;
	 Type =:= sint32;
	 Type =:= sint64;
	 Type =:= fixed32;
	 Type =:= sfixed32;
	 Type =:= fixed64;
	 Type =:= sfixed64;
	 Type =:= string;
	 Type =:= bytes;
	 Type =:= float;
	 Type =:= double ->
    protobuffs:encode(FNum, Data, Type);
pack(FNum, _, Data, Type, _) when is_atom(Data) ->
    protobuffs:encode(FNum, enum_to_int(Type, Data), enum).

enum_to_int(presencekeytype, 'PresenceKeyError') -> 8;
enum_to_int(presencekeytype, 'PresenceKeyNotify') -> 7;
enum_to_int(presencekeytype, 'PresenceKeyResult') -> 6;
enum_to_int(presencekeytype,
	    'PresenceKeyManualAuthenticationConfirm') ->
    3;
enum_to_int(presencekeytype,
	    'PresenceKeyVerifyFriend') ->
    2;
enum_to_int(presencekeytype, 'PresenceKeyPriority') ->
    1;
enum_to_int(categorytype,
	    'CategorySpecifyNotification') ->
    99;
enum_to_int(categorytype,
	    'CategoryGlobalNotification') ->
    98;
enum_to_int(categorytype, 'CategoryTickUser') -> 100;
enum_to_int(categorytype, 'CategoryAskLog') -> 10;
enum_to_int(categorytype, 'CategoryOPSNotification') ->
    4;
enum_to_int(categorytype, 'CategoryNavigation') -> 3;
enum_to_int(categorytype, 'CategorySessionList') -> 2;
enum_to_int(categorytype, 'CategoryOrganizational') ->
    1;
enum_to_int(streamendcode,
	    'StreamEndCodeNoReloginBase') ->
    200;
enum_to_int(streamendcode,
	    'StreamEndCodeReloginFromNav') ->
    101;
enum_to_int(streamendcode,
	    'StreamEndCodeReloginBase') ->
    100;
enum_to_int(iqmessagekeytype, 'IQKeySessionEvent') ->
    99;
enum_to_int(iqmessagekeytype, 'IQKeyEndSession') -> 98;
enum_to_int(iqmessagekeytype, 'IQKeyStartSession') ->
    96;
enum_to_int(iqmessagekeytype, 'IQKeyGetVUserRole') ->
    94;
enum_to_int(iqmessagekeytype, 'IQKeyGetVUser') -> 92;
enum_to_int(iqmessagekeytype, 'IQKeyError') -> 90;
enum_to_int(iqmessagekeytype, 'IQKeyResult') -> 80;
enum_to_int(iqmessagekeytype, 'IQKeyCancelPush') -> 60;
enum_to_int(iqmessagekeytype, 'IQKeyAddPush') -> 52;
enum_to_int(iqmessagekeytype, 'IQKeyPing') -> 50;
enum_to_int(iqmessagekeytype, 'IQKeyDestroyMuc') -> 40;
enum_to_int(iqmessagekeytype, 'IQKeyGetUserMucs') -> 36;
enum_to_int(iqmessagekeytype, 'IQKeyCancelMember') ->
    32;
enum_to_int(iqmessagekeytype, 'IQKeySetMember') -> 30;
enum_to_int(iqmessagekeytype, 'IQKeySetAdmin') -> 28;
enum_to_int(iqmessagekeytype, 'IQKeyCancelUSerMask') ->
    26;
enum_to_int(iqmessagekeytype, 'IQKeySetUserMask') -> 24;
enum_to_int(iqmessagekeytype, 'IQKeyGetUserMask') -> 22;
enum_to_int(iqmessagekeytype, 'IQKeyGetUserKey') -> 20;
enum_to_int(iqmessagekeytype, 'IQKeyDelUserFriend') ->
    18;
enum_to_int(iqmessagekeytype, 'IQKeyGetUserFriend') ->
    16;
enum_to_int(iqmessagekeytype,
	    'IQKeyGetUserSubScribeV2') ->
    14;
enum_to_int(iqmessagekeytype,
	    'IQKeySetUserSubScribeV2') ->
    13;
enum_to_int(iqmessagekeytype,
	    'IQKeySetVerifyFriendOpt') ->
    12;
enum_to_int(iqmessagekeytype,
	    'IQKeyGetVerifyFriendOpt') ->
    11;
enum_to_int(iqmessagekeytype,
	    'IQKeyGetUserSubScribe') ->
    10;
enum_to_int(iqmessagekeytype,
	    'IQKeyDelUserSubscribe') ->
    9;
enum_to_int(iqmessagekeytype,
	    'IQKeyAddUserSubscribe') ->
    8;
enum_to_int(iqmessagekeytype, 'IQKeyDelMucUser') -> 7;
enum_to_int(iqmessagekeytype, 'IQKeySetMucUser') -> 6;
enum_to_int(iqmessagekeytype, 'IQKeyGetMucUser') -> 5;
enum_to_int(iqmessagekeytype, 'IQKeyMucInviteV2') -> 4;
enum_to_int(iqmessagekeytype, 'IQKeyMucCreateV2') -> 3;
enum_to_int(iqmessagekeytype, 'IQKeyMucCreate') -> 2;
enum_to_int(iqmessagekeytype, 'IQKeyBind') -> 1;
enum_to_int(stringheadertype, 'StringHeaderTypeMode') ->
    76;
enum_to_int(stringheadertype,
	    'StringHeaderTypeCarbon') ->
    72;
enum_to_int(stringheadertype, 'StringHeaderTypeKey') ->
    70;
enum_to_int(stringheadertype,
	    'StringHeaderTypeMaskedUuser') ->
    68;
enum_to_int(stringheadertype,
	    'StringHeaderTypeValue') ->
    66;
enum_to_int(stringheadertype,
	    'StringHeaderTypeFriends') ->
    64;
enum_to_int(stringheadertype,
	    'StringHeaderTypeAnswer') ->
    62;
enum_to_int(stringheadertype,
	    'StringHeaderTypeQuestion') ->
    60;
enum_to_int(stringheadertype, 'StringHeaderTypeHost') ->
    58;
enum_to_int(stringheadertype, 'StringHeaderTypeName') ->
    56;
enum_to_int(stringheadertype,
	    'StringHeaderTypeKeyValue') ->
    54;
enum_to_int(stringheadertype,
	    'StringHeaderTypeTimeValue') ->
    52;
enum_to_int(stringheadertype,
	    'StringHeaderTypeCdata') ->
    50;
enum_to_int(stringheadertype, 'StringHeaderTypeCode') ->
    42;
enum_to_int(stringheadertype,
	    'StringHeaderTypeStatus') ->
    40;
enum_to_int(stringheadertype,
	    'StringHeaderTypeDomain') ->
    38;
enum_to_int(stringheadertype, 'StringHeaderTypeRole') ->
    36;
enum_to_int(stringheadertype,
	    'StringHeaderTypeReason') ->
    34;
enum_to_int(stringheadertype,
	    'StringHeaderTypeResult') ->
    32;
enum_to_int(stringheadertype, 'StringHeaderTypeType') ->
    30;
enum_to_int(stringheadertype,
	    'StringHeaderTypeAffiliation') ->
    28;
enum_to_int(stringheadertype, 'StringHeaderTypeBody') ->
    24;
enum_to_int(stringheadertype,
	    'StringHeaderTypeMethod') ->
    22;
enum_to_int(stringheadertype,
	    'StringHeaderTypeVersion') ->
    20;
enum_to_int(stringheadertype, 'StringHeaderTypePic') ->
    18;
enum_to_int(stringheadertype,
	    'StringHeaderTypeTitle') ->
    16;
enum_to_int(stringheadertype, 'StringHeaderTypeNick') ->
    12;
enum_to_int(stringheadertype,
	    'StringHeaderTypeDeleleJid') ->
    10;
enum_to_int(stringheadertype,
	    'StringHeaderTypeInviteJid') ->
    9;
enum_to_int(stringheadertype,
	    'StringHeaderTypeRealJid') ->
    8;
enum_to_int(stringheadertype, 'StringHeaderTypeJid') ->
    7;
enum_to_int(stringheadertype,
	    'StringHeaderTypeReadType') ->
    5;
enum_to_int(stringheadertype,
	    'StringHeaderTypeBackupInfo') ->
    4;
enum_to_int(stringheadertype,
	    'StringHeaderTypeExtendInfo') ->
    3;
enum_to_int(stringheadertype,
	    'StringHeaderTypeChannelId') ->
    2;
enum_to_int(stringheadertype,
	    'StringHeaderTypeChatId') ->
    1;
enum_to_int(messagetype,
	    'MessageTypeTransChatToCustomerService_Feedback') ->
    1004;
enum_to_int(messagetype,
	    'MessageTypeTransChatToCustomerService') ->
    1002;
enum_to_int(messagetype,
	    'MessageTypeTransChatToCustomer_Feedback') ->
    1003;
enum_to_int(messagetype,
	    'MessageTypeTransChatToCustomer') ->
    1001;
enum_to_int(messagetype, 'MessageTypeEncrypt') -> 404;
enum_to_int(messagetype, 'MediaTypeSystemLY') ->
    268435457;
enum_to_int(messagetype, 'MessageTypeSystem') ->
    268435456;
enum_to_int(messagetype, 'MessageTypeMarkdown') -> 13;
enum_to_int(messagetype,
	    'WebRTC_MsgType_VideoMeeting') ->
    5001;
enum_to_int(messagetype,
	    'MessageTypeRobotTurnToUser') ->
    65537;
enum_to_int(messagetype,
	    'MessageTypeRobotQuestionList') ->
    65536;
enum_to_int(messagetype, 'WebRTC_MsgType_Video') ->
    65535;
enum_to_int(messagetype, 'WebRTC_MsgType_Live') ->
    65501;
enum_to_int(messagetype, 'WebRTC_MsgType_Audio') ->
    131072;
enum_to_int(messagetype, 'MessageTypeNotice') ->
    134217728;
enum_to_int(messagetype, 'MessageTypeShareLocation') ->
    8192;
enum_to_int(messagetype, 'MessageTypeProduct') -> 4096;
enum_to_int(messagetype, 'MessageTypeMicroTourGuide') ->
    3001;
enum_to_int(messagetype, 'MessageTypeQCZhongbao') ->
    2005;
enum_to_int(messagetype, 'MessageTypeGrabMenuResult') ->
    2004;
enum_to_int(messagetype, 'MessageTypeGrabMenuVcard') ->
    2003;
enum_to_int(messagetype, 'MessageTypeConsultResult') ->
    2002;
enum_to_int(messagetype, 'MessageTypeConsult') -> 2001;
enum_to_int(messagetype, 'MessageTypeAAInfo') -> 1025;
enum_to_int(messagetype, 'MessageTypeRedPackInfo') ->
    1024;
enum_to_int(messagetype,
	    'MessageTypeCommonProductInfo') ->
    888;
enum_to_int(messagetype, 'MessageTypeCommonTrdInfo') ->
    666;
enum_to_int(messagetype, 'MessageTypeAA') -> 513;
enum_to_int(messagetype, 'MessageTypeRedPack') -> 512;
enum_to_int(messagetype, 'MessageTypeActivity') -> 511;
enum_to_int(messagetype, 'MessageTypeMeetingRemind') ->
    257;
enum_to_int(messagetype, 'MessageTypeCardShare') -> 256;
enum_to_int(messagetype, 'MessageTypeBurnAfterRead') ->
    128;
enum_to_int(messagetype, 'MessageTypeTime') -> 101;
enum_to_int(messagetype, 'MessageTypeSourceCode') -> 64;
enum_to_int(messagetype, 'MessageTypeSmallVideo') -> 32;
enum_to_int(messagetype, 'MessageTypeImageNew') -> 30;
enum_to_int(messagetype, 'MessageTypeWebRTCVidio') ->
    21;
enum_to_int(messagetype, 'MessageTypeWebRTCAudio') ->
    20;
enum_to_int(messagetype, 'MessageTypeLocalShare') -> 16;
enum_to_int(messagetype, 'MessageTypeGroupNotify') ->
    15;
enum_to_int(messagetype, 'MessageTypeGroupAt') -> 12;
enum_to_int(messagetype, 'MessageTypeNote') -> 11;
enum_to_int(messagetype, 'MessageTypeShock') -> 10;
enum_to_int(messagetype, 'MessageTypeReply') -> 9;
enum_to_int(messagetype, 'MessageTypeActionRichText') ->
    8;
enum_to_int(messagetype, 'MessageTypeRichText') -> 7;
enum_to_int(messagetype, 'MessageTypeTopic') -> 6;
enum_to_int(messagetype, 'MessageTypeFile') -> 5;
enum_to_int(messagetype, 'MessageTypeSogouIcon') -> 4;
enum_to_int(messagetype, 'MessageTypePhoto') -> 3;
enum_to_int(messagetype, 'MessageTypeVoice') -> 2;
enum_to_int(messagetype, 'MessageTypeText') -> 1;
enum_to_int(messagetype, 'MessageTypeRevoke') -> -1;
enum_to_int(messagetype, 'MessageTypePNote') -> -11;
enum_to_int(clienttype, 'ClientTypeWeb') -> 6;
enum_to_int(clienttype, 'ClientTypeLinux') -> 5;
enum_to_int(clienttype, 'ClientTypeAndroid') -> 4;
enum_to_int(clienttype, 'ClientTypePC') -> 3;
enum_to_int(clienttype, 'ClientTypeiOS') -> 2;
enum_to_int(clienttype, 'ClientTypeMac') -> 1;
enum_to_int(signaltype, 'SignalTypeCollection') -> 140;
enum_to_int(signaltype, 'SignalTypeEncryption') -> 136;
enum_to_int(signaltype, 'SignalTypeConsult') -> 132;
enum_to_int(signaltype, 'SignalTypeCarbon') -> 128;
enum_to_int(signaltype, 'SignalTypeWebRtc') -> 110;
enum_to_int(signaltype, 'SignalProceedTLS') -> 108;
enum_to_int(signaltype, 'SignalStartTLS') -> 106;
enum_to_int(signaltype, 'SignalTypeChallenge') -> 102;
enum_to_int(signaltype, 'SignalTypeUserConnect') -> 101;
enum_to_int(signaltype, 'SignalTypeWelcome') -> 100;
enum_to_int(signaltype, 'SignalTypeStreamEnd') -> 51;
enum_to_int(signaltype, 'SignalTypeStreamBegin') -> 50;
enum_to_int(signaltype, 'SignalTypeAuth') -> 45;
enum_to_int(signaltype, 'SignalTypeHeartBeat') -> 30;
enum_to_int(signaltype, 'SignalTypeShareLocation') ->
    20;
enum_to_int(signaltype, 'SignalTypeHeadline') -> 17;
enum_to_int(signaltype, 'SignalTypeMState') -> 16;
enum_to_int(signaltype, 'SignalTypeSubscription') -> 15;
enum_to_int(signaltype, 'SignalTypeRevoke') -> 14;
enum_to_int(signaltype, 'SignalTypeReadmark') -> 13;
enum_to_int(signaltype, 'SignalTypeTransfor') -> 12;
enum_to_int(signaltype, 'SignalTypeNote') -> 11;
enum_to_int(signaltype, 'SignalTypeTyping') -> 10;
enum_to_int(signaltype, 'SignalTypeError') -> 9;
enum_to_int(signaltype, 'SignalTypeNormal') -> 8;
enum_to_int(signaltype, 'SignalTypeGroupChat') -> 7;
enum_to_int(signaltype, 'SignalTypeChat') -> 6;
enum_to_int(signaltype, 'SignalTypeFailureResponse') ->
    5;
enum_to_int(signaltype,
	    'SignalTypeSucceededResponse') ->
    4;
enum_to_int(signaltype, 'SignalTypeIQResponse') -> 3;
enum_to_int(signaltype, 'SignalTypeIQ') -> 2;
enum_to_int(signaltype, 'SignalTypePresence') -> 1.

int_to_enum(presencekeytype, 8) -> 'PresenceKeyError';
int_to_enum(presencekeytype, 7) -> 'PresenceKeyNotify';
int_to_enum(presencekeytype, 6) -> 'PresenceKeyResult';
int_to_enum(presencekeytype, 3) ->
    'PresenceKeyManualAuthenticationConfirm';
int_to_enum(presencekeytype, 2) ->
    'PresenceKeyVerifyFriend';
int_to_enum(presencekeytype, 1) ->
    'PresenceKeyPriority';
int_to_enum(categorytype, 99) ->
    'CategorySpecifyNotification';
int_to_enum(categorytype, 98) ->
    'CategoryGlobalNotification';
int_to_enum(categorytype, 100) -> 'CategoryTickUser';
int_to_enum(categorytype, 10) -> 'CategoryAskLog';
int_to_enum(categorytype, 4) ->
    'CategoryOPSNotification';
int_to_enum(categorytype, 3) -> 'CategoryNavigation';
int_to_enum(categorytype, 2) -> 'CategorySessionList';
int_to_enum(categorytype, 1) ->
    'CategoryOrganizational';
int_to_enum(streamendcode, 200) ->
    'StreamEndCodeNoReloginBase';
int_to_enum(streamendcode, 101) ->
    'StreamEndCodeReloginFromNav';
int_to_enum(streamendcode, 100) ->
    'StreamEndCodeReloginBase';
int_to_enum(iqmessagekeytype, 99) ->
    'IQKeySessionEvent';
int_to_enum(iqmessagekeytype, 98) -> 'IQKeyEndSession';
int_to_enum(iqmessagekeytype, 96) ->
    'IQKeyStartSession';
int_to_enum(iqmessagekeytype, 94) ->
    'IQKeyGetVUserRole';
int_to_enum(iqmessagekeytype, 92) -> 'IQKeyGetVUser';
int_to_enum(iqmessagekeytype, 90) -> 'IQKeyError';
int_to_enum(iqmessagekeytype, 80) -> 'IQKeyResult';
int_to_enum(iqmessagekeytype, 60) -> 'IQKeyCancelPush';
int_to_enum(iqmessagekeytype, 52) -> 'IQKeyAddPush';
int_to_enum(iqmessagekeytype, 50) -> 'IQKeyPing';
int_to_enum(iqmessagekeytype, 40) -> 'IQKeyDestroyMuc';
int_to_enum(iqmessagekeytype, 36) -> 'IQKeyGetUserMucs';
int_to_enum(iqmessagekeytype, 32) ->
    'IQKeyCancelMember';
int_to_enum(iqmessagekeytype, 30) -> 'IQKeySetMember';
int_to_enum(iqmessagekeytype, 28) -> 'IQKeySetAdmin';
int_to_enum(iqmessagekeytype, 26) ->
    'IQKeyCancelUSerMask';
int_to_enum(iqmessagekeytype, 24) -> 'IQKeySetUserMask';
int_to_enum(iqmessagekeytype, 22) -> 'IQKeyGetUserMask';
int_to_enum(iqmessagekeytype, 20) -> 'IQKeyGetUserKey';
int_to_enum(iqmessagekeytype, 18) ->
    'IQKeyDelUserFriend';
int_to_enum(iqmessagekeytype, 16) ->
    'IQKeyGetUserFriend';
int_to_enum(iqmessagekeytype, 14) ->
    'IQKeyGetUserSubScribeV2';
int_to_enum(iqmessagekeytype, 13) ->
    'IQKeySetUserSubScribeV2';
int_to_enum(iqmessagekeytype, 12) ->
    'IQKeySetVerifyFriendOpt';
int_to_enum(iqmessagekeytype, 11) ->
    'IQKeyGetVerifyFriendOpt';
int_to_enum(iqmessagekeytype, 10) ->
    'IQKeyGetUserSubScribe';
int_to_enum(iqmessagekeytype, 9) ->
    'IQKeyDelUserSubscribe';
int_to_enum(iqmessagekeytype, 8) ->
    'IQKeyAddUserSubscribe';
int_to_enum(iqmessagekeytype, 7) -> 'IQKeyDelMucUser';
int_to_enum(iqmessagekeytype, 6) -> 'IQKeySetMucUser';
int_to_enum(iqmessagekeytype, 5) -> 'IQKeyGetMucUser';
int_to_enum(iqmessagekeytype, 4) -> 'IQKeyMucInviteV2';
int_to_enum(iqmessagekeytype, 3) -> 'IQKeyMucCreateV2';
int_to_enum(iqmessagekeytype, 2) -> 'IQKeyMucCreate';
int_to_enum(iqmessagekeytype, 1) -> 'IQKeyBind';
int_to_enum(stringheadertype, 76) ->
    'StringHeaderTypeMode';
int_to_enum(stringheadertype, 72) ->
    'StringHeaderTypeCarbon';
int_to_enum(stringheadertype, 70) ->
    'StringHeaderTypeKey';
int_to_enum(stringheadertype, 68) ->
    'StringHeaderTypeMaskedUuser';
int_to_enum(stringheadertype, 66) ->
    'StringHeaderTypeValue';
int_to_enum(stringheadertype, 64) ->
    'StringHeaderTypeFriends';
int_to_enum(stringheadertype, 62) ->
    'StringHeaderTypeAnswer';
int_to_enum(stringheadertype, 60) ->
    'StringHeaderTypeQuestion';
int_to_enum(stringheadertype, 58) ->
    'StringHeaderTypeHost';
int_to_enum(stringheadertype, 56) ->
    'StringHeaderTypeName';
int_to_enum(stringheadertype, 54) ->
    'StringHeaderTypeKeyValue';
int_to_enum(stringheadertype, 52) ->
    'StringHeaderTypeTimeValue';
int_to_enum(stringheadertype, 50) ->
    'StringHeaderTypeCdata';
int_to_enum(stringheadertype, 42) ->
    'StringHeaderTypeCode';
int_to_enum(stringheadertype, 40) ->
    'StringHeaderTypeStatus';
int_to_enum(stringheadertype, 38) ->
    'StringHeaderTypeDomain';
int_to_enum(stringheadertype, 36) ->
    'StringHeaderTypeRole';
int_to_enum(stringheadertype, 34) ->
    'StringHeaderTypeReason';
int_to_enum(stringheadertype, 32) ->
    'StringHeaderTypeResult';
int_to_enum(stringheadertype, 30) ->
    'StringHeaderTypeType';
int_to_enum(stringheadertype, 28) ->
    'StringHeaderTypeAffiliation';
int_to_enum(stringheadertype, 24) ->
    'StringHeaderTypeBody';
int_to_enum(stringheadertype, 22) ->
    'StringHeaderTypeMethod';
int_to_enum(stringheadertype, 20) ->
    'StringHeaderTypeVersion';
int_to_enum(stringheadertype, 18) ->
    'StringHeaderTypePic';
int_to_enum(stringheadertype, 16) ->
    'StringHeaderTypeTitle';
int_to_enum(stringheadertype, 12) ->
    'StringHeaderTypeNick';
int_to_enum(stringheadertype, 10) ->
    'StringHeaderTypeDeleleJid';
int_to_enum(stringheadertype, 9) ->
    'StringHeaderTypeInviteJid';
int_to_enum(stringheadertype, 8) ->
    'StringHeaderTypeRealJid';
int_to_enum(stringheadertype, 7) ->
    'StringHeaderTypeJid';
int_to_enum(stringheadertype, 5) ->
    'StringHeaderTypeReadType';
int_to_enum(stringheadertype, 4) ->
    'StringHeaderTypeBackupInfo';
int_to_enum(stringheadertype, 3) ->
    'StringHeaderTypeExtendInfo';
int_to_enum(stringheadertype, 2) ->
    'StringHeaderTypeChannelId';
int_to_enum(stringheadertype, 1) ->
    'StringHeaderTypeChatId';
int_to_enum(messagetype, 1004) ->
    'MessageTypeTransChatToCustomerService_Feedback';
int_to_enum(messagetype, 1002) ->
    'MessageTypeTransChatToCustomerService';
int_to_enum(messagetype, 1003) ->
    'MessageTypeTransChatToCustomer_Feedback';
int_to_enum(messagetype, 1001) ->
    'MessageTypeTransChatToCustomer';
int_to_enum(messagetype, 404) -> 'MessageTypeEncrypt';
int_to_enum(messagetype, 268435457) ->
    'MediaTypeSystemLY';
int_to_enum(messagetype, 268435456) ->
    'MessageTypeSystem';
int_to_enum(messagetype, 13) -> 'MessageTypeMarkdown';
int_to_enum(messagetype, 5001) ->
    'WebRTC_MsgType_VideoMeeting';
int_to_enum(messagetype, 65537) ->
    'MessageTypeRobotTurnToUser';
int_to_enum(messagetype, 65536) ->
    'MessageTypeRobotQuestionList';
int_to_enum(messagetype, 65535) ->
    'WebRTC_MsgType_Video';
int_to_enum(messagetype, 65501) ->
    'WebRTC_MsgType_Live';
int_to_enum(messagetype, 131072) ->
    'WebRTC_MsgType_Audio';
int_to_enum(messagetype, 134217728) ->
    'MessageTypeNotice';
int_to_enum(messagetype, 8192) ->
    'MessageTypeShareLocation';
int_to_enum(messagetype, 4096) -> 'MessageTypeProduct';
int_to_enum(messagetype, 3001) ->
    'MessageTypeMicroTourGuide';
int_to_enum(messagetype, 2005) ->
    'MessageTypeQCZhongbao';
int_to_enum(messagetype, 2004) ->
    'MessageTypeGrabMenuResult';
int_to_enum(messagetype, 2003) ->
    'MessageTypeGrabMenuVcard';
int_to_enum(messagetype, 2002) ->
    'MessageTypeConsultResult';
int_to_enum(messagetype, 2001) -> 'MessageTypeConsult';
int_to_enum(messagetype, 1025) -> 'MessageTypeAAInfo';
int_to_enum(messagetype, 1024) ->
    'MessageTypeRedPackInfo';
int_to_enum(messagetype, 888) ->
    'MessageTypeCommonProductInfo';
int_to_enum(messagetype, 666) ->
    'MessageTypeCommonTrdInfo';
int_to_enum(messagetype, 513) -> 'MessageTypeAA';
int_to_enum(messagetype, 512) -> 'MessageTypeRedPack';
int_to_enum(messagetype, 511) -> 'MessageTypeActivity';
int_to_enum(messagetype, 257) ->
    'MessageTypeMeetingRemind';
int_to_enum(messagetype, 256) -> 'MessageTypeCardShare';
int_to_enum(messagetype, 128) ->
    'MessageTypeBurnAfterRead';
int_to_enum(messagetype, 101) -> 'MessageTypeTime';
int_to_enum(messagetype, 64) -> 'MessageTypeSourceCode';
int_to_enum(messagetype, 32) -> 'MessageTypeSmallVideo';
int_to_enum(messagetype, 30) -> 'MessageTypeImageNew';
int_to_enum(messagetype, 21) ->
    'MessageTypeWebRTCVidio';
int_to_enum(messagetype, 20) ->
    'MessageTypeWebRTCAudio';
int_to_enum(messagetype, 16) -> 'MessageTypeLocalShare';
int_to_enum(messagetype, 15) ->
    'MessageTypeGroupNotify';
int_to_enum(messagetype, 12) -> 'MessageTypeGroupAt';
int_to_enum(messagetype, 11) -> 'MessageTypeNote';
int_to_enum(messagetype, 10) -> 'MessageTypeShock';
int_to_enum(messagetype, 9) -> 'MessageTypeReply';
int_to_enum(messagetype, 8) ->
    'MessageTypeActionRichText';
int_to_enum(messagetype, 7) -> 'MessageTypeRichText';
int_to_enum(messagetype, 6) -> 'MessageTypeTopic';
int_to_enum(messagetype, 5) -> 'MessageTypeFile';
int_to_enum(messagetype, 4) -> 'MessageTypeSogouIcon';
int_to_enum(messagetype, 3) -> 'MessageTypePhoto';
int_to_enum(messagetype, 2) -> 'MessageTypeVoice';
int_to_enum(messagetype, 1) -> 'MessageTypeText';
int_to_enum(messagetype, -1) -> 'MessageTypeRevoke';
int_to_enum(messagetype, -11) -> 'MessageTypePNote';
int_to_enum(clienttype, 6) -> 'ClientTypeWeb';
int_to_enum(clienttype, 5) -> 'ClientTypeLinux';
int_to_enum(clienttype, 4) -> 'ClientTypeAndroid';
int_to_enum(clienttype, 3) -> 'ClientTypePC';
int_to_enum(clienttype, 2) -> 'ClientTypeiOS';
int_to_enum(clienttype, 1) -> 'ClientTypeMac';
int_to_enum(signaltype, 140) -> 'SignalTypeCollection';
int_to_enum(signaltype, 136) -> 'SignalTypeEncryption';
int_to_enum(signaltype, 132) -> 'SignalTypeConsult';
int_to_enum(signaltype, 128) -> 'SignalTypeCarbon';
int_to_enum(signaltype, 110) -> 'SignalTypeWebRtc';
int_to_enum(signaltype, 108) -> 'SignalProceedTLS';
int_to_enum(signaltype, 106) -> 'SignalStartTLS';
int_to_enum(signaltype, 102) -> 'SignalTypeChallenge';
int_to_enum(signaltype, 101) -> 'SignalTypeUserConnect';
int_to_enum(signaltype, 100) -> 'SignalTypeWelcome';
int_to_enum(signaltype, 51) -> 'SignalTypeStreamEnd';
int_to_enum(signaltype, 50) -> 'SignalTypeStreamBegin';
int_to_enum(signaltype, 45) -> 'SignalTypeAuth';
int_to_enum(signaltype, 30) -> 'SignalTypeHeartBeat';
int_to_enum(signaltype, 20) ->
    'SignalTypeShareLocation';
int_to_enum(signaltype, 17) -> 'SignalTypeHeadline';
int_to_enum(signaltype, 16) -> 'SignalTypeMState';
int_to_enum(signaltype, 15) -> 'SignalTypeSubscription';
int_to_enum(signaltype, 14) -> 'SignalTypeRevoke';
int_to_enum(signaltype, 13) -> 'SignalTypeReadmark';
int_to_enum(signaltype, 12) -> 'SignalTypeTransfor';
int_to_enum(signaltype, 11) -> 'SignalTypeNote';
int_to_enum(signaltype, 10) -> 'SignalTypeTyping';
int_to_enum(signaltype, 9) -> 'SignalTypeError';
int_to_enum(signaltype, 8) -> 'SignalTypeNormal';
int_to_enum(signaltype, 7) -> 'SignalTypeGroupChat';
int_to_enum(signaltype, 6) -> 'SignalTypeChat';
int_to_enum(signaltype, 5) ->
    'SignalTypeFailureResponse';
int_to_enum(signaltype, 4) ->
    'SignalTypeSucceededResponse';
int_to_enum(signaltype, 3) -> 'SignalTypeIQResponse';
int_to_enum(signaltype, 2) -> 'SignalTypeIQ';
int_to_enum(signaltype, 1) -> 'SignalTypePresence';
int_to_enum(_, Val) -> Val.

decode_xmppmessage(Bytes) when is_binary(Bytes) ->
    decode(xmppmessage, Bytes).

decode_presencemessage(Bytes) when is_binary(Bytes) ->
    decode(presencemessage, Bytes).

decode_iqmessage(Bytes) when is_binary(Bytes) ->
    decode(iqmessage, Bytes).

decode_messagebody(Bytes) when is_binary(Bytes) ->
    decode(messagebody, Bytes).

decode_protomessage(Bytes) when is_binary(Bytes) ->
    decode(protomessage, Bytes).

decode_responsefailure(Bytes) when is_binary(Bytes) ->
    decode(responsefailure, Bytes).

decode_responsesucceeded(Bytes) when is_binary(Bytes) ->
    decode(responsesucceeded, Bytes).

decode_capability(Bytes) when is_binary(Bytes) ->
    decode(capability, Bytes).

decode_userconnect(Bytes) when is_binary(Bytes) ->
    decode(userconnect, Bytes).

decode_streamend(Bytes) when is_binary(Bytes) ->
    decode(streamend, Bytes).

decode_proceedtls(Bytes) when is_binary(Bytes) ->
    decode(proceedtls, Bytes).

decode_starttls(Bytes) when is_binary(Bytes) ->
    decode(starttls, Bytes).

decode_streambegin(Bytes) when is_binary(Bytes) ->
    decode(streambegin, Bytes).

decode_welcomemessage(Bytes) when is_binary(Bytes) ->
    decode(welcomemessage, Bytes).

decode_authmessage(Bytes) when is_binary(Bytes) ->
    decode(authmessage, Bytes).

decode_protoheader(Bytes) when is_binary(Bytes) ->
    decode(protoheader, Bytes).

decode_packagelength(Bytes) when is_binary(Bytes) ->
    decode(packagelength, Bytes).

decode_stringheader(Bytes) when is_binary(Bytes) ->
    decode(stringheader, Bytes).

decode_messagekeyvalue(Bytes) when is_binary(Bytes) ->
    decode(messagekeyvalue, Bytes).

delimited_decode_messagekeyvalue(Bytes) ->
    delimited_decode(messagekeyvalue, Bytes).

delimited_decode_stringheader(Bytes) ->
    delimited_decode(stringheader, Bytes).

delimited_decode_packagelength(Bytes) ->
    delimited_decode(packagelength, Bytes).

delimited_decode_protoheader(Bytes) ->
    delimited_decode(protoheader, Bytes).

delimited_decode_authmessage(Bytes) ->
    delimited_decode(authmessage, Bytes).

delimited_decode_welcomemessage(Bytes) ->
    delimited_decode(welcomemessage, Bytes).

delimited_decode_streambegin(Bytes) ->
    delimited_decode(streambegin, Bytes).

delimited_decode_starttls(Bytes) ->
    delimited_decode(starttls, Bytes).

delimited_decode_proceedtls(Bytes) ->
    delimited_decode(proceedtls, Bytes).

delimited_decode_streamend(Bytes) ->
    delimited_decode(streamend, Bytes).

delimited_decode_userconnect(Bytes) ->
    delimited_decode(userconnect, Bytes).

delimited_decode_capability(Bytes) ->
    delimited_decode(capability, Bytes).

delimited_decode_responsesucceeded(Bytes) ->
    delimited_decode(responsesucceeded, Bytes).

delimited_decode_responsefailure(Bytes) ->
    delimited_decode(responsefailure, Bytes).

delimited_decode_protomessage(Bytes) ->
    delimited_decode(protomessage, Bytes).

delimited_decode_messagebody(Bytes) ->
    delimited_decode(messagebody, Bytes).

delimited_decode_iqmessage(Bytes) ->
    delimited_decode(iqmessage, Bytes).

delimited_decode_presencemessage(Bytes) ->
    delimited_decode(presencemessage, Bytes).

delimited_decode_xmppmessage(Bytes) ->
    delimited_decode(xmppmessage, Bytes).

delimited_decode(Type, Bytes) when is_binary(Bytes) ->
    delimited_decode(Type, Bytes, []).

delimited_decode(_Type, <<>>, Acc) ->
    {lists:reverse(Acc), <<>>};
delimited_decode(Type, Bytes, Acc) ->
    try protobuffs:decode_varint(Bytes) of
      {Size, Rest} when size(Rest) < Size ->
	  {lists:reverse(Acc), Bytes};
      {Size, Rest} ->
	  <<MessageBytes:Size/binary, Rest2/binary>> = Rest,
	  Message = decode(Type, MessageBytes),
	  delimited_decode(Type, Rest2, [Message | Acc])
    catch
      _What:_Why -> {lists:reverse(Acc), Bytes}
    end.

decode(enummsg_values, 1) -> value1;
decode(messagekeyvalue, Bytes) when is_binary(Bytes) ->
    Types = [{2, value, string, []}, {1, key, string, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(messagekeyvalue, Decoded);
decode(stringheader, Bytes) when is_binary(Bytes) ->
    Types = [{4, definedkey, stringheadertype, []},
	     {3, value, string, []}, {2, key, string, []},
	     {1, params, messagekeyvalue, [is_record, repeated]}],
    Defaults = [{1, params, []}],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(stringheader, Decoded);
decode(packagelength, Bytes) when is_binary(Bytes) ->
    Types = [{1, length, int32, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(packagelength, Decoded);
decode(protoheader, Bytes) when is_binary(Bytes) ->
    Types = [{6, message, bytes, []},
	     {5, content, string, []}, {4, length, int32, []},
	     {3, optionlist, int32, [repeated]},
	     {2, options, int32, []}, {1, version, int32, []}],
    Defaults = [{3, optionlist, []}],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(protoheader, Decoded);
decode(authmessage, Bytes) when is_binary(Bytes) ->
    Types = [{5, otherbody, messagebody, [is_record]},
	     {4, authkey, string, []}, {3, msgid, string, []},
	     {2, method, string, []}, {1, mechanism, string, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(authmessage, Decoded);
decode(welcomemessage, Bytes) when is_binary(Bytes) ->
    Types = [{4, sockmod, string, []},
	     {3, user, string, []}, {2, version, string, []},
	     {1, domain, string, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(welcomemessage, Decoded);
decode(streambegin, Bytes) when is_binary(Bytes) ->
    Types = [{3, bodys, messagebody, [is_record, repeated]},
	     {2, version, string, []}, {1, domain, string, []}],
    Defaults = [{3, bodys, []}],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(streambegin, Decoded);
decode(starttls, Bytes) when is_binary(Bytes) ->
    Types = [],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(starttls, Decoded);
decode(proceedtls, Bytes) when is_binary(Bytes) ->
    Types = [],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(proceedtls, Decoded);
decode(streamend, Bytes) when is_binary(Bytes) ->
    Types = [{2, code, int32, []}, {1, reason, string, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(streamend, Decoded);
decode(userconnect, Bytes) when is_binary(Bytes) ->
    Types = [{2, version, string, []},
	     {1, domain, string, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(userconnect, Decoded);
decode(capability, Bytes) when is_binary(Bytes) ->
    Types = [{2, bodys, messagebody, [is_record]},
	     {1, version, string, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(capability, Decoded);
decode(responsesucceeded, Bytes)
    when is_binary(Bytes) ->
    Types = [{4, body, messagebody, [is_record]},
	     {3, info, string, []}, {2, msgid, string, []},
	     {1, code, int32, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(responsesucceeded, Decoded);
decode(responsefailure, Bytes) when is_binary(Bytes) ->
    Types = [{4, body, messagebody, [is_record]},
	     {3, error, string, []}, {2, msgid, string, []},
	     {1, code, int32, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(responsefailure, Decoded);
decode(protomessage, Bytes) when is_binary(Bytes) ->
    Types = [{11, sendjid, string, []},
	     {10, origintype, string, []}, {9, originto, string, []},
	     {8, originfrom, string, []}, {7, realto, string, []},
	     {6, realfrom, string, []}, {5, message, bytes, []},
	     {4, to, string, []}, {3, from, string, []},
	     {2, signaltype, int32, []}, {1, options, int32, []}],
    Defaults = [{2, signaltype, 0}],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(protomessage, Decoded);
decode(messagebody, Bytes) when is_binary(Bytes) ->
    Types = [{3, bodys, messagebody, [is_record, repeated]},
	     {2, value, string, []},
	     {1, headers, stringheader, [is_record, repeated]}],
    Defaults = [{1, headers, []}, {3, bodys, []}],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(messagebody, Decoded);
decode(iqmessage, Bytes) when is_binary(Bytes) ->
    Types = [{11, definedkey, iqmessagekeytype, []},
	     {10, bodys, messagebody, [is_record, repeated]},
	     {9, headers, stringheader, [is_record, repeated]},
	     {8, transfertime, int64, []},
	     {7, receivedtime, int64, []},
	     {6, body, messagebody, [is_record]},
	     {5, header, stringheader, [is_record]},
	     {4, messageid, string, []}, {3, value, string, []},
	     {2, key, string, []}, {1, namespace, string, []}],
    Defaults = [{9, headers, []}, {10, bodys, []}],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(iqmessage, Decoded);
decode(presencemessage, Bytes) when is_binary(Bytes) ->
    Types = [{12, categorytype, int32, []},
	     {11, definedkey, presencekeytype, []},
	     {10, bodys, messagebody, [is_record, repeated]},
	     {9, headers, stringheader, [is_record, repeated]},
	     {8, transfertime, int64, []},
	     {7, receivedtime, int64, []},
	     {6, body, messagebody, [is_record]},
	     {5, header, stringheader, [is_record]},
	     {4, messageid, string, []}, {3, value, string, []},
	     {2, key, string, []}, {1, namespace, string, []}],
    Defaults = [{9, headers, []}, {10, bodys, []}],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(presencemessage, Decoded);
decode(xmppmessage, Bytes) when is_binary(Bytes) ->
    Types = [{13, bodys, messagebody,
	      [is_record, repeated]},
	     {12, headers, stringheader, [is_record, repeated]},
	     {11, transfertime, int64, []},
	     {10, receivedtime, int64, []},
	     {9, body, messagebody, [is_record]},
	     {8, header, stringheader, [is_record]},
	     {7, messageid, string, []}, {6, value, string, []},
	     {5, key, string, []}, {4, namespace, string, []},
	     {3, clientversion, int64, []},
	     {2, clienttype, int32, []},
	     {1, messagetype, int32, []}],
    Defaults = [{1, messagetype, 0}, {2, clienttype, 0},
		{12, headers, []}, {13, bodys, []}],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(xmppmessage, Decoded).

decode(<<>>, Types, Acc) ->
    reverse_repeated_fields(Acc, Types);
decode(Bytes, Types, Acc) ->
    {ok, FNum} = protobuffs:next_field_num(Bytes),
    case lists:keyfind(FNum, 1, Types) of
      {FNum, Name, Type, Opts} ->
	  {Value1, Rest1} = case lists:member(is_record, Opts) of
			      true ->
				  {{FNum, V}, R} = protobuffs:decode(Bytes,
								     bytes),
				  RecVal = decode(Type, V),
				  {RecVal, R};
			      false ->
				  case lists:member(repeated_packed, Opts) of
				    true ->
					{{FNum, V}, R} =
					    protobuffs:decode_packed(Bytes,
								     Type),
					{V, R};
				    false ->
					{{FNum, V}, R} =
					    protobuffs:decode(Bytes, Type),
					{unpack_value(V, Type), R}
				  end
			    end,
	  case lists:member(repeated, Opts) of
	    true ->
		case lists:keytake(FNum, 1, Acc) of
		  {value, {FNum, Name, List}, Acc1} ->
		      decode(Rest1, Types,
			     [{FNum, Name, [int_to_enum(Type, Value1) | List]}
			      | Acc1]);
		  false ->
		      decode(Rest1, Types,
			     [{FNum, Name, [int_to_enum(Type, Value1)]} | Acc])
		end;
	    false ->
		decode(Rest1, Types,
		       [{FNum, Name, int_to_enum(Type, Value1)} | Acc])
	  end;
      false ->
	  case lists:keyfind('$extensions', 2, Acc) of
	    {_, _, Dict} ->
		{{FNum, _V}, R} = protobuffs:decode(Bytes, bytes),
		Diff = size(Bytes) - size(R),
		<<V:Diff/binary, _/binary>> = Bytes,
		NewDict = dict:store(FNum, V, Dict),
		NewAcc = lists:keyreplace('$extensions', 2, Acc,
					  {false, '$extensions', NewDict}),
		decode(R, Types, NewAcc);
	    _ ->
		{ok, Skipped} = protobuffs:skip_next_field(Bytes),
		decode(Skipped, Types, Acc)
	  end
    end.

reverse_repeated_fields(FieldList, Types) ->
    [begin
       case lists:keyfind(FNum, 1, Types) of
	 {FNum, Name, _Type, Opts} ->
	     case lists:member(repeated, Opts) of
	       true -> {FNum, Name, lists:reverse(Value)};
	       _ -> Field
	     end;
	 _ -> Field
       end
     end
     || {FNum, Name, Value} = Field <- FieldList].

unpack_value(Binary, string) when is_binary(Binary) ->
    binary_to_list(Binary);
unpack_value(Value, _) -> Value.

to_record(messagekeyvalue, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       messagekeyvalue),
						   Record, Name, Val)
			  end,
			  #messagekeyvalue{}, DecodedTuples),
    Record1;
to_record(stringheader, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       stringheader),
						   Record, Name, Val)
			  end,
			  #stringheader{}, DecodedTuples),
    Record1;
to_record(packagelength, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       packagelength),
						   Record, Name, Val)
			  end,
			  #packagelength{}, DecodedTuples),
    Record1;
to_record(protoheader, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       protoheader),
						   Record, Name, Val)
			  end,
			  #protoheader{}, DecodedTuples),
    Record1;
to_record(authmessage, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       authmessage),
						   Record, Name, Val)
			  end,
			  #authmessage{}, DecodedTuples),
    Record1;
to_record(welcomemessage, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       welcomemessage),
						   Record, Name, Val)
			  end,
			  #welcomemessage{}, DecodedTuples),
    Record1;
to_record(streambegin, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       streambegin),
						   Record, Name, Val)
			  end,
			  #streambegin{}, DecodedTuples),
    Record1;
to_record(starttls, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       starttls),
						   Record, Name, Val)
			  end,
			  #starttls{}, DecodedTuples),
    Record1;
to_record(proceedtls, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       proceedtls),
						   Record, Name, Val)
			  end,
			  #proceedtls{}, DecodedTuples),
    Record1;
to_record(streamend, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       streamend),
						   Record, Name, Val)
			  end,
			  #streamend{}, DecodedTuples),
    Record1;
to_record(userconnect, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       userconnect),
						   Record, Name, Val)
			  end,
			  #userconnect{}, DecodedTuples),
    Record1;
to_record(capability, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       capability),
						   Record, Name, Val)
			  end,
			  #capability{}, DecodedTuples),
    Record1;
to_record(responsesucceeded, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       responsesucceeded),
						   Record, Name, Val)
			  end,
			  #responsesucceeded{}, DecodedTuples),
    Record1;
to_record(responsefailure, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       responsefailure),
						   Record, Name, Val)
			  end,
			  #responsefailure{}, DecodedTuples),
    Record1;
to_record(protomessage, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       protomessage),
						   Record, Name, Val)
			  end,
			  #protomessage{}, DecodedTuples),
    Record1;
to_record(messagebody, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       messagebody),
						   Record, Name, Val)
			  end,
			  #messagebody{}, DecodedTuples),
    Record1;
to_record(iqmessage, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       iqmessage),
						   Record, Name, Val)
			  end,
			  #iqmessage{}, DecodedTuples),
    Record1;
to_record(presencemessage, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       presencemessage),
						   Record, Name, Val)
			  end,
			  #presencemessage{}, DecodedTuples),
    Record1;
to_record(xmppmessage, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       xmppmessage),
						   Record, Name, Val)
			  end,
			  #xmppmessage{}, DecodedTuples),
    Record1.

decode_extensions(Record) -> Record.

decode_extensions(_Types, [], Acc) ->
    dict:from_list(Acc);
decode_extensions(Types, [{Fnum, Bytes} | Tail], Acc) ->
    NewAcc = case lists:keyfind(Fnum, 1, Types) of
	       {Fnum, Name, Type, Opts} ->
		   {Value1, Rest1} = case lists:member(is_record, Opts) of
				       true ->
					   {{FNum, V}, R} =
					       protobuffs:decode(Bytes, bytes),
					   RecVal = decode(Type, V),
					   {RecVal, R};
				       false ->
					   case lists:member(repeated_packed,
							     Opts)
					       of
					     true ->
						 {{FNum, V}, R} =
						     protobuffs:decode_packed(Bytes,
									      Type),
						 {V, R};
					     false ->
						 {{FNum, V}, R} =
						     protobuffs:decode(Bytes,
								       Type),
						 {unpack_value(V, Type), R}
					   end
				     end,
		   case lists:member(repeated, Opts) of
		     true ->
			 case lists:keytake(FNum, 1, Acc) of
			   {value, {FNum, Name, List}, Acc1} ->
			       decode(Rest1, Types,
				      [{FNum, Name,
					lists:reverse([int_to_enum(Type, Value1)
						       | lists:reverse(List)])}
				       | Acc1]);
			   false ->
			       decode(Rest1, Types,
				      [{FNum, Name, [int_to_enum(Type, Value1)]}
				       | Acc])
			 end;
		     false ->
			 [{Fnum,
			   {optional, int_to_enum(Type, Value1), Type, Opts}}
			  | Acc]
		   end;
	       false -> [{Fnum, Bytes} | Acc]
	     end,
    decode_extensions(Types, Tail, NewAcc).

set_record_field(Fields, Record, '$extensions',
		 Value) ->
    Decodable = [],
    NewValue = decode_extensions(element(1, Record),
				 Decodable, dict:to_list(Value)),
    Index = list_index('$extensions', Fields),
    erlang:setelement(Index + 1, Record, NewValue);
set_record_field(Fields, Record, Field, Value) ->
    Index = list_index(Field, Fields),
    erlang:setelement(Index + 1, Record, Value).

list_index(Target, List) -> list_index(Target, List, 1).

list_index(Target, [Target | _], Index) -> Index;
list_index(Target, [_ | Tail], Index) ->
    list_index(Target, Tail, Index + 1);
list_index(_, [], _) -> -1.

extension_size(_) -> 0.

has_extension(_Record, _FieldName) -> false.

get_extension(_Record, _FieldName) -> undefined.

set_extension(Record, _, _) -> {error, Record}.

