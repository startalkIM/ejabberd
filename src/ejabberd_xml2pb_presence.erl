-module(ejabberd_xml2pb_presence).

-include("message_pb.hrl").
-include("jlib.hrl").
-include("logger.hrl").

-export([encode_presence_invite_muc/3,encode_presence_muc_notice_add_user/3]).
-export([encode_presence_muc_destory/3,encode_del_muc_register/3,encode_x_user_packet/3]).
-export([encode_update_muc_vcard/3,encode_presence_del_muc_user/3,enocde_status/3]).
-export([encode_verify_friend/3,encode_delete_friend/3,encode_manual_authentication_confirm/3]).
-export([encode_mask_user/3,encode_cancel_mask_user/3,encode_presence_mask_user/3, encode_set_user_subscribe_v2/3, encode_notify_presence/3]).
-export([encode_presence_forbidden_words/3]).


encode_pb_presence_msg(Key,Val,Msg_ID,Header,Body,Headers,Bodys) ->
	Presence_PB =
		#presencemessage{
		    key = Key,
		    value = Val,
		    messageid = Msg_ID,
		    header = Header,
		    body = Body,
		    receivedtime = qtalk_public:get_timestamp(),
	   	    headers = Headers,
	   	    bodys = Bodys
		},
    FPresence_PB = handle_pb_presence_key(Key,Presence_PB),
    ?DEBUG("presence PB  ~p ~n",[FPresence_PB]),
    
	message_pb:encode_presencemessage(FPresence_PB).

handle_pb_presence_key(Key,P) ->
    case ejabberd_xml2pb_public:set_presenceKey_type(Key) of
    'none' ->
        P#presencemessage{key = Key};
    V ->
        P#presencemessage{definedkey = V}
    end.

encode_pb_presence_msg(Key,Val,Msg_ID,Header,Body,Headers,Bodys, Definedkey, Category) ->
	Presence_PB =
		#presencemessage{
		    key = Key,
		    value = Val,
		    definedkey = Definedkey,
		    messageid = Msg_ID,
		    header = Header,
		    body = Body,
		    receivedtime = qtalk_public:get_timestamp(),
	   	    headers = Headers,
		    categorytype = Category,
	   	    bodys = Bodys
		},
	?DEBUG("presence PB  ~p ~n",[Presence_PB]),

	message_pb:encode_presencemessage(Presence_PB).

struct_pb_presence_msg(From,To,Type,Key,Val,Msg_ID,Header,Body,Haeders,Bodys) ->
        Presence = list_to_binary(encode_pb_presence_msg(Key,Val,Msg_ID,Header,Body,Haeders,Bodys)),
	?DEBUG("Headers1 ~p ~n",[Presence]),
        Pb_Msg = list_to_binary(ejabberd_xml2pb_public:encode_pb_protomessage(From,To,Type,0,Presence)),
	?DEBUG("Headers2 ~p ~n",[Pb_Msg]),
        Opt = ejabberd_xml2pb_public:get_proto_header_opt(Pb_Msg),
	?DEBUG("Headers3 ~p ~n",[Opt]),
        R = list_to_binary(ejabberd_xml2pb_public:encode_pb_protoheader(Opt,Pb_Msg)),
	?DEBUG("Headers4 ~p ~n",[R]),
	R.

struct_pb_presence_msg(From,To,Type,Key,Val,Msg_ID,Header,Body,Haeders,Bodys, Definedkey, Catagory) ->
        Presence = list_to_binary(encode_pb_presence_msg(Key,Val,Msg_ID,Header,Body,Haeders,Bodys, Definedkey, Catagory)),
	?DEBUG("Headers1 ~p ~n",[Presence]),
        Pb_Msg = list_to_binary(ejabberd_xml2pb_public:encode_pb_protomessage(From,To,Type,0,Presence)),
	?DEBUG("Headers2 ~p ~n",[Pb_Msg]),
        Opt = ejabberd_xml2pb_public:get_proto_header_opt(Pb_Msg),
	?DEBUG("Headers3 ~p ~n",[Opt]),
        R = list_to_binary(ejabberd_xml2pb_public:encode_pb_protoheader(Opt,Pb_Msg)),
	?DEBUG("Headers4 ~p ~n",[R]),
	R.

encode_presence_muc_notice_add_user(From,To,Packet) ->
%	#xmlel{name = Name, attrs = Attrs, children = E} = Packet,
	E = fxml:get_subtag(Packet,<<"x">>),
    Els = fxml:get_subtag(E,<<"item">>),
	Real_jid = proplists:get_value(<<"real_jid">>,Els#xmlel.attrs,<<"">>),
	Jid = proplists:get_value(<<"jid">>,Els#xmlel.attrs,Real_jid),
	Affiliation = proplists:get_value(<<"affiliation">>,Els#xmlel.attrs),
	Domain = proplists:get_value(<<"domain">>,Els#xmlel.attrs),
	
    Headers = [{<<"jid">>,qtalk_public:tokens_jid(Jid)},
               {<<"affiliation">>,Affiliation},
               {<<"domain">>,Domain}],    
    Body = ejabberd_xml2pb_public:encode_messagebody(Headers,<<"user_info">>),
    
    struct_pb_presence_msg(From,To,'SignalTypePresence',<<"result">>,<<"user_join_muc">>,<<"">>,'undefined',Body,[],[]).

encode_x_user_packet(From,To,Packet) ->
    Packet1 =  fxml:get_subtag(Packet,<<"x">>),
    case fxml:get_attr_s(<<"type">>,Packet#xmlel.attrs) of
    <<"unavailable">> ->
        case fxml:get_subtag(Packet1,<<"destroy">>) of
        false ->
            encode_presence_del_muc_user(From,To,Packet);
        _ ->
            encode_presence_muc_destory(From,To,Packet)
        end;
    _ ->
        encode_presence_muc_notice_add_user(From,To,Packet)
    end.
    
encode_presence_invite_muc(From,To,Packet) ->
    #xmlel{name = Name, attrs = Attrs, children = Els} = Packet,
    JID = proplists:get_value(<<"invite_jid">>,Attrs),
    Status = proplists:get_value(<<"status">>,Attrs),

    Headers = [{<<"invite_jid">>,JID},
               {<<"status">>,Status}],    
    Body = ejabberd_xml2pb_public:encode_messagebody(Headers,<<"invite_info">>),
    struct_pb_presence_msg(From,To,'SignalTypePresence',<<"result">>,<<"invite_user">>,<<"">>,'undefined',Body,[],[]).

encode_presence_muc_destory(From,To,Packet) ->
    Els = fxml:get_subtag(Packet,<<"x">>),
    Item = fxml:get_subtag(Els,<<"item">>),
    Affiliation = proplists:get_value(<<"affiliation">>,Item#xmlel.attrs),
    Role = proplists:get_value(<<"role">>,Item#xmlel.attrs),

    Headers = [{<<"affiliation">>,Affiliation},
               {<<"role">>,Role}],
    Body = ejabberd_xml2pb_public:encode_messagebody(Headers,<<"item">>),
    struct_pb_presence_msg(From,To,'SignalTypePresence',<<"result">>,<<"destory_muc">>,<<"">>,'undefined',Body,[],[]).

encode_presence_del_muc_user(From,To,Packet) ->
    Els = fxml:get_subtag(Packet,<<"x">>),
    Item = fxml:get_subtag(Els,<<"item">>),
    Affiliation = proplists:get_value(<<"affiliation">>,Item#xmlel.attrs),
    Role = proplists:get_value(<<"role">>,Item#xmlel.attrs),
    Status = fxml:get_subtag(Els,<<"status">>),
    Code = proplists:get_value(<<"code">>,Status#xmlel.attrs),

    Headers = [{<<"affiliation">>,Affiliation},
               {<<"role">>,Role},
               {<<"code">>,Code}],
    Body = ejabberd_xml2pb_public:encode_messagebody(Headers,<<"item">>),
    struct_pb_presence_msg(From,To,'SignalTypePresence',<<"result">>,<<"del_muc_user">>,<<"">>,'undefined',Body,[],[]).

encode_del_muc_register(From,To,Packet) ->
    Del_Jid = proplists:get_value(<<"del_jid">>,Packet#xmlel.attrs),
    Headers = [{<<"del_jid">>,Del_Jid}],
    Body = ejabberd_xml2pb_public:encode_messagebody(Headers,<<"del_muc_register">>),
    struct_pb_presence_msg(From,To,'SignalTypePresence',<<"result">>,<<"del_muc_register">>,<<"">>,'undefined',Body,[],[]).

encode_update_muc_vcard(From,To,Packet) ->
    Els = fxml:get_subtag(Packet,<<"vcard_updte">>),
    Nick = proplists:get_value(<<"nick">>,Els#xmlel.attrs),
    Desc = proplists:get_value(<<"desc">>,Els#xmlel.attrs),
    Title = proplists:get_value(<<"title">>,Els#xmlel.attrs),
    Pic = proplists:get_value(<<"pic">>,Els#xmlel.attrs),
    Version = proplists:get_value(<<"version">>,Els#xmlel.attrs),
    Headers = [{<<"nick">>,Nick},
               {<<"desc">>,Desc},
               {<<"title">>,Title},
               {<<"pic">>,Pic},
               {<<"version">>,Version}],
    Body = ejabberd_xml2pb_public:encode_messagebody(Headers,<<"update_muc_vcard">>),
    struct_pb_presence_msg(From,To,'SignalTypePresence',<<"result">>,<<"update_muc_vcard">>,<<"">>,'undefined',Body,[],[]).

enocde_status(From,To,Packet) ->
    Show = fxml:get_subtag_cdata(Packet,<<"show">>),
    Priority = fxml:get_subtag_cdata(Packet,<<"priority">>),
    Headers = [{<<"show">>,Show},
               {<<"priority">>,Priority}],
    Body = ejabberd_xml2pb_public:encode_messagebody(Headers,<<"user_update_status">>),
    struct_pb_presence_msg(From,To,'SignalTypePresence',<<"result">>,<<"update_user_status">>,<<"">>,'undefined',Body,[],[]).

encode_verify_friend(From,To,Packet) ->
    Type = proplists:get_value(<<"type">>,Packet#xmlel.attrs), 
    Rslt = proplists:get_value(<<"result">>,Packet#xmlel.attrs,<<"">>),
    Reason = proplists:get_value(<<"reason">>,Packet#xmlel.attrs,<<"">>),
    Method = proplists:get_value(<<"method">>,Packet#xmlel.attrs,<<"">>),
    Body1 = proplists:get_value(<<"body">>,Packet#xmlel.attrs,<<"">>),
    Headers = [{<<"type">>,Type},
               {<<"result">>,Rslt},
               {<<"reason">>,Reason},
               {<<"method">>,Method},
               {<<"body">>,Body1}],

    Body = ejabberd_xml2pb_public:encode_messagebody(Headers,<<"verify_friend">>), 
    struct_pb_presence_msg(From,To,'SignalTypePresence',<<"result">>,<<"verify_friend">>,<<"">>,'undefined',Body,[],[]).

encode_manual_authentication_confirm(From,To,Packet) ->
    Friend_num = proplists:get_value(<<"friend_num">>,Packet#xmlel.attrs,<<"">>), 
    Type = proplists:get_value(<<"type">>,Packet#xmlel.attrs,<<"">>),
    Reason = proplists:get_value(<<"reason">>,Packet#xmlel.attrs,<<"">>),
    Method = proplists:get_value(<<"method">>,Packet#xmlel.attrs,<<"">>),
    Body1 = proplists:get_value(<<"body">>,Packet#xmlel.attrs,<<"">>),
    Headers = [{<<"type">>,Type},
               {<<"reason">>,Reason},
               {<<"method">>,Method},
               {<<"body">>,Body1},
               {<<"friend_num">>,Friend_num}],

    Body = ejabberd_xml2pb_public:encode_messagebody(Headers,<<"verify_friend">>),
    struct_pb_presence_msg(From,To,'SignalTypePresence',
            <<"manual_authentication_confirm">>,<<"confirm_verify_friend">>,<<"">>,'undefined',Body,[],[]).
     
encode_delete_friend(From,To,Packet) ->
    Type = proplists:get_value(<<"type">>,Packet#xmlel.attrs,<<"">>),
    Rslt = proplists:get_value(<<"result">>,Packet#xmlel.attrs,<<"">>),
    JID = proplists:get_value(<<"jid">>,Packet#xmlel.attrs,<<"">>), 
    Domain = proplists:get_value(<<"domain">>,Packet#xmlel.attrs,<<"">>), 
    Headers = [{<<"type">>,Type},
               {<<"result">>,Rslt},
               {<<"jid">>,JID},
               {<<"domain">>,Domain}],
    Body = ejabberd_xml2pb_public:encode_messagebody(Headers,<<"delete_friend">>),
    struct_pb_presence_msg(From,To,'SignalTypePresence',<<"result">>,<<"delete_friend">>,<<"">>,'undefined',Body,[],[]).


encode_presence_mask_user(From,To,Packet) ->
    case fxml:get_subtag(Packet,<<"mask_user">>) of
    false ->
        case fxml:get_subtag(Packet,<<"cancel_masked_user">>) of
        false ->
            <<"error">>;
        Cancel_Mask ->
            encode_cancel_mask_user(From,To,Cancel_Mask)
        end;
    Mask ->
        encode_mask_user(From,To,Mask)
    end.

encode_presence_forbidden_words(From,To,Packet) ->
	case catch fxml:get_attr_s(<<"forbidden_words">>,Packet#xmlel.attrs) of
	Value when is_binary(Value) ->
		encode_forbidden_words(From,To,Packet);	
	_ ->
		<<"error">>
	end.		
    
encode_set_user_subscribe_v2(From,To,Packet) ->
    case fxml:get_subtag(Packet,<<"subscribe_updte">>) of
    false ->
        <<"error">>;
    Subscribe ->
        encode_subscribe_update(From,To, Subscribe)
    end.

encode_subscribe_update(From, To, Subscribe) ->
    Status = proplists:get_value(<<"status">>, Subscribe#xmlel.attrs,<<"0">>),
    Headers = [{<<"status">>, Status}],
    Body = ejabberd_xml2pb_public:encode_messagebody(Headers,<<"subscribe_update">>),
    struct_pb_presence_msg(From,To,'SignalTypePresence',<<"result">>,<<"subscribe_update">>,<<"">>,'undefined',Body,[],[]).

encode_mask_user(From,To,Packet) ->
    JID = proplists:get_value(<<"jid">>,Packet#xmlel.attrs,<<"">>),
    Headers = [{<<"jid">>,JID}],
    Body = ejabberd_xml2pb_public:encode_messagebody(Headers,<<"mask_user">>),
    struct_pb_presence_msg(From,To,'SignalTypePresence',<<"result">>,<<"mask_user">>,<<"">>,'undefined',Body,[],[]).

encode_forbidden_words(From,To,Packet) ->
	ForbiddenWords = fxml:get_attr_s(<<"forbidden_words">>,Packet#xmlel.attrs),
	Headers = [{<<"forbidden_words">>, ForbiddenWords}],
	Body = ejabberd_xml2pb_public:encode_messagebody(Headers,<<"forbidden_words">>),
    struct_pb_presence_msg(From,To,'SignalTypePresence',<<"result">>,<<"forbidden_words">>,<<"">>,'undefined',Body,[],[]).

encode_cancel_mask_user(From,To,Packet) ->
    JID = proplists:get_value(<<"jid">>,Packet#xmlel.attrs,<<"">>),
    Headers = [{<<"jid">>,JID}],
    Body = ejabberd_xml2pb_public:encode_messagebody(Headers,<<"cancel_mask_user">>),
    struct_pb_presence_msg(From,To,'SignalTypePresence',<<"result">>,<<"cancel_mask_user">>,<<"">>,'undefined',Body,[],[]).
   
encode_notify_presence(From,To,Packet) ->
    Data = proplists:get_value(<<"data">>,Packet#xmlel.attrs,<<"">>),
    Category = proplists:get_value(<<"category">>,Packet#xmlel.attrs,<<"1">>),
    Body = ejabberd_xml2pb_public:encode_messagebody([], Data),
    struct_pb_presence_msg(From,To,'SignalTypePresence',<<"result">>, <<"notify">>,<<"">>,'undefined',Body,[],[], 'PresenceKeyNotify', binary_to_integer(Category)).
