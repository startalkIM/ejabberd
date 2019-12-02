-module(ejabberd_xml2pb_iq).

-include("message_pb.hrl").
-include("jlib.hrl").
-include("logger.hrl").

-export([encode_pb_error_iq/3,encode_muc_user_pb/3,encode_pb_iq_bind_result/4]).
-export([encode_pb_iq_create_muc/3,encode_user_muc_pb/3,encode_muc_invite_user_v2_pb/3]). 
-export([encode_pb_muc_user_register/3,encode_pb_muc_user_del_register/3,encode_pb_muc_amdin/3]).
-export([encode_pb_set_friend_opt/3,encode_pb_get_user_friends/3,encode_pb_del_user_friend/3]).
-export([encode_pb_time_http_key/3,encode_pb_get_friend_opt/3,encode_pb_destroy_muc/3]).
-export([encode_pb_ping/3,encode_pb_get_mask_user/3,encode_pb_set_mask_user/3,encode_pb_cancel_mask_user/3]).
-export([encode_pb_handle_user_subscribe/3]).
-export([encode_pb_handle_user_subscribe_v2/3]).
-export([encode_pb_get_virtual_user/3,encode_pb_get_vuser_role/3,encode_pb_start_session/3,encode_pb_end_session/3]).
-export([struct_pb_iq_msg/10,encode_pb_mac_push_notice_jid/3,encode_pb_mac_push_notice/3,encode_pb_cancel_mac_push_notice/3]).


%%----------------------------------------------
%% @date 2016-9
%% 对iqmessage进行encode
%%----------------------------------------------
encode_pb_iq_msg(Key,Val,Msg_ID,Header,Body,Headers,Bodys) ->
	PB_IQ = 
		#iqmessage{
    			value = Val,
    			messageid = Msg_ID,
    			header = Header,
    			body = Body,
    			receivedtime = qtalk_public:get_timestamp(),
    			headers = Headers,
    			bodys = Bodys
			},
    FPB_IQ = handle_pb_iq_key(Key,PB_IQ), 
	message_pb:encode_iqmessage(FPB_IQ).


handle_pb_iq_key(Key,IQ) ->
    case ejabberd_xml2pb_public:set_iqKey_type(Key) of
    'none' ->
        IQ#iqmessage{key = Key};
    V ->
        IQ#iqmessage{definedkey = V}
    end.
       


%%----------------------------------------------
%% @date 2016-9
%% 对iqmessage进行封包
%%----------------------------------------------
struct_pb_iq_msg(From,To,Type,Key,Val,Msg_ID,Header,Body,Haeders,Bodys) ->
        IQ = list_to_binary(encode_pb_iq_msg(Key,Val,Msg_ID,Header,Body,Haeders,Bodys)),
        ?DEBUG("IQ ~p,Type ~p  ~n",[IQ,Type]),
        Pb_Msg = list_to_binary(ejabberd_xml2pb_public:encode_pb_protomessage(From,To,Type,0,IQ)),
        ?DEBUG("Pb_MSg ~p , size ~p~n",[Pb_Msg,size(Pb_Msg)]),
        Opt = ejabberd_xml2pb_public:get_proto_header_opt(Pb_Msg),
        Res = list_to_binary(ejabberd_xml2pb_public:encode_pb_protoheader(Opt,Pb_Msg)),
        ?DEBUG("Pb_MSg  , sizeen   ~p~n",[size(Res)]),
        Res.
        
	
%%----------------------------------------------
%% @date 2016-9
%% 构建PB获取群成员信息Bodys
%%----------------------------------------------
get_pb_iq_muc_user_bodys(Packet) ->
%%	#xmlel{name = Name, attrs = Attrs, children = Els} = Packet,
    case fxml:get_subtag(Packet,<<"query">>) of
    false ->
        []; 
    Query ->
        M_users = fxml:get_subtags(Query,<<"m_user">>),
        Bodys =	lists:flatmap(fun(Xml) ->
              Headers = 
                 case is_record(Xml,xmlel) of
                 true ->
        		    JID = proplists:get_value(<<"jid">>,Xml#xmlel.attrs,<<"">>),
                    case proplists:get_value(<<"affiliation">>,Xml#xmlel.attrs) of
                    undefined ->
                        [{<<"jid">>,JID}];
                    Aff ->
                        [{<<"jid">>,JID},
                         {<<"affiliation">>,Aff}]
                    end;
                _ ->
                    []
                end,
	         [ejabberd_xml2pb_public:encode_messagebody(Headers,<<"m_user">>)] end,M_users),
        Bodys
    end.
	
%%----------------------------------------------
%% @date 2016-9
%% PB获取群成员信息
%%----------------------------------------------
encode_muc_user_pb(From,To,Packet) ->
	Bodys = get_pb_iq_muc_user_bodys(Packet),
    ID = case  fxml:get_attr(<<"id">>,Packet#xmlel.attrs) of
    false ->
            integer_to_binary(qtalk_public:timestamp());
    {_, I} ->
            I
    end,    
	struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"muc_users">>,ID,'undefined','undefined',[],Bodys).
		

%%----------------------------------------------
%% @date 2016-9
%% encode PB error IQ
%%----------------------------------------------
encode_pb_error_iq(From,To,Packet) ->
    Body = 
        case fxml:get_subtag(Packet,<<"error">>) of
        false ->
            'undefined';
        Error ->
            case fxml:get_attr(<<"code">>,Error#xmlel.attrs) of
            false ->
                 'undefined';
            {value,Code} ->
               CData = fxml:get_subtag_cdata(Error,<<"text">>),
               Headers = [{<<"code">>,Code},
                          {<<"data">>,CData}],
               ejabberd_xml2pb_public:encode_messagebody(Headers,<<"error_info">>)
            end
        end,
    ID = qtalk_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"error">>,'undefined',ID,'undefined',Body,[],[]).
   
%%----------------------------------------------
%% @date 2016-9
%% encode PB IQ BIND Resut
%%----------------------------------------------
encode_pb_iq_bind_result(From,To,Packet,Key) ->
	?DEBUG("key ~p ~n",[Key]),
    Body = 
        case fxml:get_subtag(Packet,<<"bind">>) of
        false ->
            'undefined';
        Bind ->
            CData = fxml:get_subtag_cdata(Bind,<<"jid">>),
            Headers = [{<<"time_value">>,integer_to_binary(qtalk_public:get_timestamp())},
                       {<<"key_value">>,Key}],
            ejabberd_xml2pb_public:encode_messagebody(Headers,CData)
        end,
    ID = qtalk_public:get_xml_attrs_id(Packet),
	?DEBUG("id ~p ~n",[ID]),
    Res  = struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"bind">>,ID,'undefined',Body,[],[]),
?DEBUG("Res ~p~n",[Res]),
	Res.
           
%%----------------------------------------------
%% @date 2016-9
%% encode PB IQ Create MUC
%%----------------------------------------------
encode_pb_iq_create_muc(From,To,Packet) ->
    Body = case fxml:get_subtag(Packet,<<"query">>) of
           false ->
                'undefined';
           Query ->
                case fxml:get_subtag(Query,<<"create_muc">>) of
                false ->
                    'undefined';
                Muc_Res ->
                    case fxml:get_attr(<<"result">>,Muc_Res#xmlel.attrs) of
                    false ->
                            'undefined';
                    {value,Res} ->
                         ejabberd_xml2pb_public:encode_messagebody([],Res)
                    end
                end
            end,
    ID = qtalk_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"create_muc">>,ID,'undefined',Body,[],[]).
                    
    
%%----------------------------------------------
%% @date 2016-9
%% 获取PB类型IQ：用户所有注册群的bodys
%%----------------------------------------------
get_pb_iq_user_mucs_bodys(Packet) ->
%%	#xmlel{name = Name, attrs = Attrs, children = Els} = Packet,
    case fxml:get_subtag(Packet,<<"query">>) of
    false ->
        ?INFO_MSG("get_pb_iq_user_mucs_bodys1 ~p ~n",[Packet]),
        []; 
    Query ->
        Mucs = fxml:get_subtags(Query,<<"muc_rooms">>),
        Bodys =	lists:flatmap(fun(Xml) ->
              Headers = 
                 case is_record(Xml,xmlel) of
                 true ->
        		    Host = proplists:get_value(<<"host">>,Xml#xmlel.attrs, qtalk_public:get_default_domain()),
                    case proplists:get_value(<<"name">>,Xml#xmlel.attrs) of
                    undefined ->
                        ?INFO_MSG("get_pb_iq no found name ~n",[]),
                        [];
                    Name ->
                        [{<<"name">>,Name},
                         {<<"host">>,Host}]
                    end;
                _ ->
                    ?INFO_MSG("get_pb_iq_user_mucs_bodys ~p ~n",[Packet]),
                    []
                end,
	         [ejabberd_xml2pb_public:encode_messagebody(Headers,<<"muc_room">>)] end,Mucs),
        Bodys
    end.
	
%%----------------------------------------------
%% @date 2016-9
%% 对PB类型IQ进行组装
%%----------------------------------------------
encode_user_muc_pb(From,To,Packet) ->
	Bodys = get_pb_iq_user_mucs_bodys(Packet),
    ID = case  fxml:get_attr(<<"id">>,Packet#xmlel.attrs) of
    false ->
            integer_to_binary(qtalk_public:timestamp());
    {_, I} ->
            I
    end,    
	struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"user_mucs">>,ID,'undefined','undefined',[],Bodys).

%%----------------------------------------------
%% @date 2016-9
%% 获取邀请muc用户的bodys
%%----------------------------------------------
get_pb_iq_invite_muc_user_bodys(Packet) ->
    case fxml:get_subtag(Packet,<<"query">>) of
    false ->
        []; 
    Query ->
        M_invites = fxml:get_subtags(Query,<<"muc_invites">>),
        Bodys =	lists:flatmap(fun(Xml) ->
              Headers = 
                 case is_record(Xml,xmlel) of
                 true ->
        		    JID = proplists:get_value(<<"jid">>,Xml#xmlel.attrs,<<"">>),
                    case proplists:get_value(<<"status">>,Xml#xmlel.attrs) of
                    undefined ->
                        [{<<"jid">>,JID}];
                    Status ->
                        [{<<"jid">>,JID},
                         {<<"status">>,Status}]
                    end;
                _ ->
                    []
                end,
	         [ejabberd_xml2pb_public:encode_messagebody(Headers,<<"muc_invites">>)] end,M_invites),
        Bodys
    end.
	
%%----------------------------------------------
%% @date 2016-9
%% 对聊天室邀请用户进行v2处理
%%----------------------------------------------
encode_muc_invite_user_v2_pb(From,To,Packet) ->
	Bodys = get_pb_iq_invite_muc_user_bodys(Packet),
    ID = case  fxml:get_attr(<<"id">>,Packet#xmlel.attrs) of
    false ->
            integer_to_binary(qtalk_public:timestamp());
    {_, I} ->
            I
    end,    
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"muc_invite_user_v2">>,ID,'undefined','undefined',[],Bodys).

%%----------------------------------------------
%% @date 2016-9
%% 用户注册信息
%%----------------------------------------------
encode_pb_muc_user_register(From,To,Packet) ->
    Body = 
        case fxml:get_subtag(Packet,<<"query">>) of
        false ->
            'undefined';
        Query ->
            [Xml] = Query#xmlel.children,
            case Xml#xmlel.name of
            <<"set_register">> ->
                ejabberd_xml2pb_public:encode_messagebody([],<<"set_register">>);
             _ ->
                ok
            end
        end,
    ID = qtalk_public:get_xml_attrs_id(Packet),
    Res  = struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"muc_set_register">>,ID,'undefined',Body,[],[]),
    case catch From#jid.lserver of
    <<"ejabhost2">> ->
	?INFO_MSG("Res ~p ~n",[Res]);
    _ ->
	ok
    end,
    Res.


%%----------------------------------------------
%% @date 2016-9
%% 取消用户注册信息
%%----------------------------------------------
encode_pb_muc_user_del_register(From,To,Packet) ->
    Body = 
        case fxml:get_subtag(Packet,<<"query">>) of
        false ->
            'undefined';
        _ ->
            ejabberd_xml2pb_public:encode_messagebody([],<<"del_register">>)
        end,
    ID = qtalk_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"muc_del_register">>,ID,'undefined',Body,[],[]).
     
 
%%----------------------------------------------
%% @date 2016-9
%% 提升管理员/取消管理员/T人，回复是一样的
%%----------------------------------------------
encode_pb_muc_amdin(From,To,Packet) ->
    ID = qtalk_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"muc_admin">>,ID,'undefined','undefined',[],[]).


%%----------------------------------------------
%% @date 2016-9
%% 设置添加好友配置项
%%----------------------------------------------

encode_pb_set_friend_opt(From,To,Packet) ->
    Body = 
        case fxml:get_subtag(Packet,<<"set_verify_friend_mode">>) of
        false ->
            'undefined';
        Query ->
            Headers = 
                case  fxml:get_attr(<<"result">>,Query#xmlel.attrs) of
                false ->
                        [{<<"result">>,<<"false">>}];
                {value,Res} ->
                        [{<<"result">>,Res}]
                end,
            ejabberd_xml2pb_public:encode_messagebody(Headers,<<"set_verify_friend_mode">>)
        end,
    ID = qtalk_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"set_verify_friend_mode">>,ID,'undefined',Body,[],[]).

%%----------------------------------------------
%% @date 2016-9
%% 获取好友配置项
%%----------------------------------------------

encode_pb_get_friend_opt(From,To,Packet) ->
    Body = 
        case fxml:get_subtag(Packet,<<"get_verify_friend_mode">>) of
        false ->
            'undefined';
        Query ->
            Headers = make_friend_opt_headers(Query#xmlel.attrs), 
            ejabberd_xml2pb_public:encode_messagebody(Headers,<<"result">>)
        end,
    ID = qtalk_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"get_verify_friend_mode">>,ID,'undefined',Body,[],[]).

%%----------------------------------------------
%% @date 2016-9
%% 获取用户好友列表
%%----------------------------------------------

encode_pb_get_user_friends(From,To,Packet) ->
    Body = 
        case fxml:get_subtag(Packet,<<"get_user_friends">>) of
        false ->
            'undefined';
        Query ->
            Headers = 
                case  fxml:get_attr(<<"friends">>,Query#xmlel.attrs) of
                false ->
                        [];
                {value,Res} ->
                        [{<<"friends">>,Res}]
                end,
            ejabberd_xml2pb_public:encode_messagebody(Headers,<<"get_friends">>)
        end,
    ID = qtalk_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"user_get_friends">>,ID,'undefined',Body,[],[]).


%%----------------------------------------------
%% @date 2016-9
%% 删除好友
%%----------------------------------------------

encode_pb_del_user_friend(From,To,Packet) ->
    Body = 
        case fxml:get_subtag(Packet,<<"delete_friend">>) of
        false ->
            'undefined';
        Query ->
            Headers = 
                case  fxml:get_attr(<<"jid">>,Query#xmlel.attrs) of
                false ->
                        [];
                {value,Res} ->
                        case  fxml:get_attr(<<"result">>,Query#xmlel.attrs) of
                        false ->
                                [];
                        {value,Result} ->
                                [{<<"jid">>,Res},
                                 {<<"result">>,Result}]
                        end
                end,
            ejabberd_xml2pb_public:encode_messagebody(Headers,<<"del_user_friend">>)
        end,
    ID = qtalk_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"user_del_friend">>,ID,'undefined',Body,[],[]).



%%----------------------------------------------
%% @date 2016-9
%% 获取time_key和http auth key
%%----------------------------------------------

encode_pb_time_http_key(From,To,Packet) ->
    Body = 
        case fxml:get_subtag(Packet,<<"key">>) of
        false ->
            'undefined';
        Query ->
            Headers = 
                case  fxml:get_attr(<<"value">>,Query#xmlel.attrs) of
                false ->
                        [];
                {value,Res} ->
                    [{<<"time_key">>,integer_to_binary(qtalk_public:get_timestamp())},
                     {<<"key">>,Res}]
                end,
            ejabberd_xml2pb_public:encode_messagebody(Headers,<<"get_key">>)
        end,
    ID = qtalk_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"get_key">>,ID,'undefined',Body,[],[]).



%%----------------------------------------------
%% @date 2016-9
%% 销毁群结果
%%----------------------------------------------

encode_pb_destroy_muc(From,To,Packet) ->
    ID = qtalk_public:get_xml_attrs_id(Packet),
    Body =  ejabberd_xml2pb_public:encode_messagebody([{<<"result">>,<<"success">>}],<<"desctroy_muc">>),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"desctroy_muc">>,ID,'undefined',Body,[],[]).


%%----------------------------------------------
%% @date 2016-9
%% 心跳结果
%%----------------------------------------------

encode_pb_ping(From,To,Packet) ->
    ID = qtalk_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"ping">>,ID,'undefined','undefined',[],[]).


%%----------------------------------------------
%% @date 2016-9
%% 获取用户屏蔽列表
%%----------------------------------------------

encode_pb_get_mask_user(From,To,Packet) ->
    Mask_Users = fxml:get_subtags(Packet,<<"get_mask_user">>), 
    Bodys =	lists:flatmap(fun(Xml) ->
               Headers = 
                case is_record(Xml,xmlel) of
                true ->
                    case proplists:get_value(<<"masked_user">>,Xml#xmlel.attrs) of
                    undefined ->
                        [];
                    JID ->
                        [{<<"masked_user">>,JID}]
                    end;
                _ ->
                    []
                end,
	         [ejabberd_xml2pb_public:encode_messagebody(Headers,<<"get_mask_user">>)] end,Mask_Users),

    ID = case  fxml:get_attr(<<"id">>,Packet#xmlel.attrs) of
    false ->
            integer_to_binary(qtalk_public:timestamp());
    {_, I} ->
            I
    end,    
	struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"get_mask_users">>,ID,'undefined','undefined',[],Bodys).
		


%%----------------------------------------------
%% @date 2016-9
%% 屏蔽好友
%%----------------------------------------------

encode_pb_set_mask_user(From,To,Packet) ->
    Body = 
        case fxml:get_subtag(Packet,<<"mask_user">>) of
        false ->
            'undefined';
        Query ->
            Headers = 
                case  fxml:get_attr(<<"result">>,Query#xmlel.attrs) of
                false ->
                        [{<<"result">>,<<"failed">>}];
                {value,Res} ->
                        [{<<"result">>,Res}]
                end,
            ejabberd_xml2pb_public:encode_messagebody(Headers,<<"mask_user">>)
        end,
    ID = qtalk_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"set_mask_user">>,ID,'undefined',Body,[],[]).


%%----------------------------------------------
%% @date 2016-9
%% 取消屏蔽好友
%%----------------------------------------------

encode_pb_cancel_mask_user(From,To,Packet) ->
    Body = 
        case fxml:get_subtag(Packet,<<"cancel_mask_user">>) of
        false ->
            'undefined';
        Query ->
            Headers = 
                case  fxml:get_attr(<<"result">>,Query#xmlel.attrs) of
                false ->
                        [{<<"result">>,<<"failed">>}];
                {value,Res} ->
                        [{<<"result">>,Res}]
                end,
            ejabberd_xml2pb_public:encode_messagebody(Headers,<<"mask_user">>)
        end,
    ID = qtalk_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"cancel_mask_user">>,ID,'undefined',Body,[],[]).


encode_pb_handle_user_subscribe(From,To,Packet) ->
    Body = 
        case fxml:get_subtag(Packet,<<"query">>) of
        false ->
            'undefined';
        Query ->
            case handle_subscribe_xml(Query,<<"subscribe">>) of
            [] ->
                case handle_subscribe_xml(Query,<<"add_subscribe">>) of
                [] ->
                    case handle_subscribe_xml(Query,<<"delete_subscribe">>) of
                    [] ->
                        'undefined';
                    Del ->
                        ejabberd_xml2pb_public:encode_messagebody(Del,<<"delete_subscribe">>)
                    end;
                Add ->
                    ejabberd_xml2pb_public:encode_messagebody(Add,<<"add_subscribe">>)
                end;
            Sub ->
                ejabberd_xml2pb_public:encode_messagebody(Sub,<<"subscribe">>)
            end
        end,
            
    ID = qtalk_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"subscribe">>,ID,'undefined',Body,[],[]).
            
encode_pb_handle_user_subscribe_v2(From,To,Packet) ->
    Body = case fxml:get_subtag(Packet,<<"query">>) of
        false -> 'undefined';
        Query ->
            case handle_subscribe_xml(Query,<<"subscribe">>) of
            [] ->
                case handle_subscribe_xml(Query,<<"add_subscribe">>) of
                [] ->
                    'undefined';
                Add ->
                    ejabberd_xml2pb_public:encode_messagebody(Add,<<"add_subscribe">>)
                end;
            Sub ->
                ejabberd_xml2pb_public:encode_messagebody(Sub,<<"subscribe">>)
            end
        end,

    ID = qtalk_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"subscribe">>,ID,'undefined',Body,[],[]).
        
handle_subscribe_xml(Packet,Key) ->
    case fxml:get_subtag(Packet,Key) of
    false ->
        [];
    Sub ->
        case fxml:get_attr(<<"status">>,Sub#xmlel.attrs) of
        {value,V} ->
            [{<<"status">>,V}];
        _ ->
            [{<<"status">>,<<"false">>}]
        end
    end.
        


make_friend_opt_headers(Attrs) ->
    JID = proplists:get_value(<<"jid">>,Attrs,<<"">>),
    Mode = proplists:get_value(<<"mode">>,Attrs,<<"">>),
    Question = proplists:get_value(<<"question">>,Attrs,<<"">>),
    Answer = proplists:get_value(<<"answer">>,Attrs,<<"">>),
    [{<<"jid">>,JID},
     {<<"mode">>,Mode},
     {<<"question">>,Question},
     {<<"answer">>,Answer}].


encode_pb_get_virtual_user(From,To,Packet) ->
    Bodys = 
        case fxml:get_subtags(Packet,<<"virtual_user">>) of
        false ->
            ?INFO_MSG("get_virtual_user ~p ~n",[Packet]),
            []; 
        VUsers ->
            lists:flatmap(fun(Xml) ->
              Headers = 
                 case is_record(Xml,xmlel) of
                 true ->
                    case proplists:get_value(<<"virtual_user">>,Xml#xmlel.attrs) of
                    undefined ->
                        ?INFO_MSG("get_pb_iq no found name ~n",[]),
                        [];
                    [VU] ->
                        [{<<"vuser">>,VU}]
                    end;
                _ ->
                    ?INFO_MSG("get_pb_iq_user_mucs_bodys ~p ~n",[Packet]),
                    []
                end,
	         [ejabberd_xml2pb_public:encode_messagebody(Headers,<<"get_vuser">>)] end,VUsers)
    end,
    ID = case  fxml:get_attr(<<"id">>,Packet#xmlel.attrs) of
    false ->
            integer_to_binary(qtalk_public:timestamp());
    {_, I} ->
            I
    end,    
	struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"get_vuser">>,ID,'undefined','undefined',[],Bodys).
    
        
encode_pb_get_vuser_role(From,To,Packet) ->
    Body = 
        case fxml:get_subtag(Packet,<<"on_duty_virtual_user">>) of
        false ->
            'undefined';
        Xml ->
            Headers = 
                case  proplists:get_value(<<"virtual_user">>,Xml#xmlel.attrs) of
                undefined ->
                        [{<<"vitual_user">>,<<"">>}];
                [VU] ->
                        [{<<"vitual_user">>,VU}]
                end,
            ejabberd_xml2pb_public:encode_messagebody(Headers,<<"on_duty_virtual_user">>)
        end,
    ID = qtalk_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"on_duty_virtual_user">>,ID,'undefined',Body,[],[]).
    
       
encode_pb_start_session(From,To,Packet) ->
    Body = 
        case fxml:get_subtag(Packet,<<"start_session">>) of
        false ->
            'undefined';
        Xml ->
            Headers = 
                case proplists:get_value(<<"result">>,Xml#xmlel.attrs) of
                undefined ->
                        [{<<"start_session">>,<<"failed">>}];
                <<"start session sucess">> ->
                        RUser = proplists:get_value(<<"real_user">>,Xml#xmlel.attrs,<<"">>),
                        [{<<"start_session">>,<<"success">>},{<<"real_user">>,RUser}];
                <<"start session failed">> ->
                         [{<<"start_session">>,<<"failed">>}]
                end,
            ejabberd_xml2pb_public:encode_messagebody(Headers,<<"start_session">>)
        end,
    ID = qtalk_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"start_session">>,ID,'undefined',Body,[],[]).

encode_pb_end_session(From,To,Packet) ->
    Body = 
        case fxml:get_subtag(Packet,<<"end_session">>) of
        false ->
            'undefined';
        Xml ->
            Headers = 
                case  proplists:get_value(<<"result">>,Xml#xmlel.attrs) of
                undefined ->
                        [{<<"result">>,<<"failed">>}];
                Res ->
                        [{<<"result">>,Res}]
                end,
            ejabberd_xml2pb_public:encode_messagebody(Headers,<<"end_session">>)
        end,
    ID = qtalk_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"end_session">>,ID,'undefined',Body,[],[]).


encode_pb_mac_push_notice_jid(From,To,Packet) ->
    Body =
        case fxml:get_subtag(Packet,<<"get_mac_push_notice">>) of
        false ->
            'undefined';
        Xml ->
            Headers = 
                case  proplists:get_value(<<"result">>,Xml#xmlel.attrs) of
                undefined ->
                     [{<<"result">>,<<"no_shield">>}];
                Res  ->
                        [{<<"result">>,Res}]
                end,
            ejabberd_xml2pb_public:encode_messagebody(Headers,<<"get_mac_push_notice">>)
        end,
    ID = qtalk_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"get_mac_push_notice">>,ID,'undefined',Body,[],[]).
            

encode_pb_mac_push_notice(From,To,Packet) ->
    ID = qtalk_public:get_xml_attrs_id(Packet),
       % case fxml:get_subtag(Packet,<<"mac_push_notice">>) of
    case fxml:get_subtags(Packet,<<"mac_push_notice">>) of
    [] ->
        'undefined';
    Xmls when is_list(Xmls) ->
        X = lists:nth(1,Xmls),
        case  proplists:get_value(<<"result">>,X#xmlel.attrs) of
        undefined ->
            Bodys = handle_mac_push_notice(Xmls),
            ?DEBUG("Bodys ~p ~n",[Bodys]),
            struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"get_mac_push_notices">>,ID,'undefined','undefined',[],Bodys);
        Res  ->
            Body = ejabberd_xml2pb_public:encode_messagebody([{<<"result">>,Res}],<<"set_mac_push_notice">>),
            struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"set_mac_push_notice">>,ID,'undefined',Body,[],[])
        end
    end.
            
handle_mac_push_notice(XMLS) ->
      lists:flatmap(fun(Xml) ->
              Headers = 
                case is_record(Xml,xmlel) of
                true ->
                    case proplists:get_value(<<"shield_user">>,Xml#xmlel.attrs) of
                    'undefined' ->
                            [];
                    User ->
                        [{<<"shield_user">>,User}]
                    end;
                _ ->
                    []  
                end,
               [ejabberd_xml2pb_public:encode_messagebody(Headers,<<"shield_users">>)] end,XMLS).
     
encode_pb_cancel_mac_push_notice(From,To,Packet) ->
    Body =
        case fxml:get_subtag(Packet,<<"calcel_mac_push_notice">>) of
        false ->
            'undefined';
        Xml ->
            Headers = 
                case  proplists:get_value(<<"result">>,Xml#xmlel.attrs) of
                undefined ->
                     [{<<"result">>,<<"failed">>}];
                Res  ->
                        [{<<"result">>,Res}]
                end,
            ejabberd_xml2pb_public:encode_messagebody(Headers,<<"cancel_mac_push_notice">>)
        end,
    ID = qtalk_public:get_xml_attrs_id(Packet),
    struct_pb_iq_msg(From,To,'SignalTypeIQ',<<"result">>,<<"cancel_mac_push_notice">>,ID,'undefined',Body,[],[]).
            
    
