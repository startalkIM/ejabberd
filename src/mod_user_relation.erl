-module(mod_user_relation).

-behavior(gen_mod).

-export([start/2, stop/1, depends/2, mod_opt_type/1]).
-export([verify_friend_mode/3,do_verify_friend/3,get_users_friend_num/3,get_user_friends/3,del_friend/3,del_invite/3]).

-include("logger.hrl").
-include("jlib.hrl").

-record(friend_opts,{user,rec_msg_flag,vld_friend_flag,validate_quetion,validate_answer,version}).

start(Host, _Opts) ->
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,?NS_VER_FRI_MODE, ?MODULE, verify_friend_mode, no_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,?NS_VER_FRI_MODE, ?MODULE, verify_friend_mode, no_queue),

    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,?NS_GET_FRI, ?MODULE, get_user_friends, no_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,?NS_GET_FRI, ?MODULE, get_user_friends, no_queue),

    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,?NS_DEL_FRI, ?MODULE, del_friend, no_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,?NS_DEL_FRI, ?MODULE, del_friend, no_queue),

    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,?NS_DEL_INVITE, ?MODULE, del_invite, no_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,?NS_DEL_INVITE, ?MODULE, del_invite, no_queue).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,?NS_VER_FRI_MODE),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,?NS_VER_FRI_MODE),

    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,?NS_GET_FRI),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,?NS_GET_FRI),

    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,?NS_DEL_FRI),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,?NS_DEL_FRI),

    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,?NS_DEL_INVITE),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,?NS_DEL_INVITE).

verify_friend_mode(From, To,#iq{type = Type, sub_el = SubEl} = IQ) ->
    case {Type, SubEl} of
        {get, #xmlel{name  = <<"get_verify_friend_mode">>}} -> 
            IQ#iq{type = result, sub_el = [iq_get_verify_friend_mode(From,To,SubEl)]};
        {set, #xmlel{name  = <<"set_verify_friend_mode">>}} -> 
            IQ#iq{type = result, sub_el = [iq_set_verify_friend_mode(From,SubEl)]}
    end.

get_user_friends(From, _To, #iq{type = Type, sub_el = SubEl} = IQ) ->
    case {Type, SubEl} of
        {get,#xmlel{name = <<"get_user_friends">>}} ->
            IQ#iq{type = result, sub_el = [iq_get_user_friend(From,SubEl)]};
        _ ->
            IQ#iq{type = result, sub_el = []}
    end.

do_verify_friend(Server,User, Userhost) ->
    case catch user_relation:get_user_friend_opts(Server,User, Userhost, get_user_relation_opts) of
        Fri_opts when is_record(Fri_opts,friend_opts) ->
            [{<<"mode">>,Fri_opts#friend_opts.vld_friend_flag},
             {<<"answer">>,Fri_opts#friend_opts.validate_answer},
             {<<"num">>,case catch get_users_friend_num(Server,User,Userhost) of N when is_integer(N) -> N; _ -> 0 end}];
        _ -> []
    end.

iq_get_user_friend(From, _SubEl) ->
    case user_relation:get_user_friends(From#jid.lserver,From#jid.luser,From#jid.lserver, list) of
        FL when is_list(FL) ->
            #xmlel{name = <<"get_user_friends">>,
                   attrs = [{<<"xmlns">>,?NS_GET_FRI},{<<"friends">>,make_user_friends(FL)}],
	           children = []};
        _ ->
            #xmlel{name = <<"get_user_friends">>,
                   attrs = [{<<"xmlns">>,?NS_GET_FRI},{<<"friends">>,<<"">>}],
                   children = []}
    end.

make_user_friends(Friend_list) ->
    J_FL = lists:map(fun({{F,H},V}) ->
        {obj,[{"F",F},{"H",H},{"V",V}]}
    end,Friend_list),

    list_to_binary(rfc4627:encode(J_FL)).

get_users_friend_num(Server,User,Userhost) ->
    user_relation:get_user_friends(Server,User,Userhost, size).

iq_get_verify_friend_mode(From,To,SubEl) ->
    ?DEBUG("SubEl ~p ~n",[SubEl]),
    case fxml:get_tag_attr_s(<<"jid">>,SubEl) of
    <<"">> ->
        #xmlel{name = <<"get_verify_friend_mode">>,
                attrs = [{<<"xmlns">>,?NS_VER_FRI_MODE},{<<"jid">>,<<"">>},{<<"question">>,<<"">>}, {<<"mode">>,<<"3">>}],
                children = []};
    User ->
        case catch user_relation:get_user_friend_opts(To#jid.lserver,User,To#jid.lserver,get_user_relation_opts) of
            Fri_opts when is_record(Fri_opts,friend_opts) ->
                case Fri_opts#friend_opts.vld_friend_flag of
                    <<"2">> ->
                     case User =/= From#jid.luser of
                        true ->
                            #xmlel{name = <<"get_verify_friend_mode">>,
                                   attrs = [{<<"xmlns">>,?NS_VER_FRI_MODE},{<<"jid">>,User}, {<<"mode">>,<<"2">>},{<<"question">>,Fri_opts#friend_opts.validate_quetion}],
                                   children = []};
                        _ ->
                            #xmlel{name = <<"get_verify_friend_mode">>,
                                   attrs = [{<<"xmlns">>,?NS_VER_FRI_MODE},{<<"jid">>,User},{<<"mode">>,<<"2">>}, {<<"question">>,Fri_opts#friend_opts.validate_quetion},{<<"answer">>,Fri_opts#friend_opts.validate_answer}],
                                   children = []}
                    end;
                   V when is_binary(V)->
                       #xmlel{name = <<"get_verify_friend_mode">>,
                              attrs = [{<<"xmlns">>,?NS_VER_FRI_MODE},{<<"jid">>,User}, {<<"mode">>,V}],
                              children = []};
                   _ ->
                       #xmlel{name = <<"get_verify_friend_mode">>,
                             attrs = [{<<"xmlns">>,?NS_VER_FRI_MODE},{<<"jid">>,User}, {<<"mode">>,<<"3">>}],
                             children = []}
               end;
           _ ->
               #xmlel{name = <<"get_verify_friend_mode">>,
                      attrs = [{<<"xmlns">>,?NS_VER_FRI_MODE},{<<"jid">>,User},{<<"question">>,<<"">>}, {<<"mode">>,<<"3">>}],
                      children = []}
        end
    end.

iq_set_verify_friend_mode(From,SubEl) ->
    ?DEBUG("From,SubEl ~p, EL ~p ~n",[From,SubEl]),
    User = From#jid.luser,
    Mode = fxml:get_tag_attr_s(<<"mode">>,SubEl),
    Question = fxml:get_tag_attr_s(<<"question">>,SubEl),
    Answer = fxml:get_tag_attr_s(<<"answer">>,SubEl),
    Rslt = case (Mode == <<"2">> andalso Question =/= <<"">> andalso Answer =/= <<"">>) orelse Mode =/= <<"2">> of
        true ->
            case user_relation:add_user_friend_opts(From#jid.lserver,User,From#jid.lserver,Mode,Question,Answer,<<"1">>) of
                true -> <<"success">>;
                _ -> <<"failed">>
            end;
        false -> <<"failed">>
    end,

    #xmlel{name = <<"set_verify_friend_mode">>,
           attrs = [{<<"xmlns">>,?NS_VER_FRI_MODE},{<<"result">>,Rslt}],
           children = []}.

del_friend(From, _To, #iq{type = Type, sub_el = SubEl} = IQ) ->
    case {Type, SubEl} of
        {set, #xmlel{name  = <<"delete_friend">>}} -> 
            IQ#iq{type = result, sub_el = [iq_del_friend(From,SubEl)]};
        _ ->
            IQ#iq{type = error, sub_el = [SubEl, ?ERR_FEATURE_NOT_IMPLEMENTED]}
    end.

iq_del_friend(From,SubEl) ->
    case fxml:get_tag_attr_s(<<"jid">>,SubEl) of
        <<"">> ->
            #xmlel{name = <<"delete_friend">>,
                   attrs = [{<<"xmlns">>,?NS_DEL_FRI},{<<"jid">>,<<"">>},{<<"result">>,<<"failed">>}],
                   children = []};
        User ->
            Mode = fxml:get_tag_attr_s(<<"mode">>,SubEl),
            Domain = case fxml:get_tag_attr_s(<<"domain">>,SubEl) of
                <<>> -> From#jid.lserver;
                H -> H
            end,
        Rslt = case user_relation:del_friend(From#jid.lserver,From#jid.luser,From#jid.lserver,User,Domain,Mode) of
            true ->
                send_del_presence_packet(From,User,Domain,<<"success">>,Mode),
                <<"success">>;
            _ ->
                catch send_del_presence_packet(From,User,Domain,<<"failed">>,Mode),
                <<"failed">>
        end,

        #xmlel{name = <<"delete_friend">>,
               attrs = [{<<"xmlns">>,?NS_DEL_FRI},{<<"jid">>,User},{<<"result">>,Rslt}],
               children = []}
    end.

del_invite(From, _To, #iq{type = Type, sub_el = SubEl} = IQ) ->
    case {Type, SubEl} of
        {set, #xmlel{name  = <<"delete_invite">>}} -> 
            IQ#iq{type = result, sub_el = [iq_del_invite(From,SubEl)]};
        _ ->
            IQ#iq{type = error,
                  sub_el = [SubEl, ?ERR_FEATURE_NOT_IMPLEMENTED]}
    end.

iq_del_invite(From,SubEl) ->
    case fxml:get_tag_attr_s(<<"time">>,SubEl) of
        <<"">> ->
            #xmlel{name = <<"delete_invite">>,
                   attrs = [{<<"xmlns">>,?NS_DEL_INVITE},{<<"result">>,<<"failed">>}],
                   children = []};
        _ ->
            Rslt = case catch ejabberd_sql:sql_query(From#jid.lserver,
			 [<<"delete from invite_spool where username = '">>,From#jid.luser,<<"' and host = '">>,From#jid.lserver,<<"';">>]) of
                {updated,_} -> <<"success">>;
                _ -> <<"failed">>
            end,

            #xmlel{name = <<"delete_friend">>,
                   attrs = [{<<"xmlns">>,?NS_DEL_FRI},{<<"result">>,Rslt}],
                   children = []}
    end.

send_del_presence_packet(From_jid,To,Domain,Rslt,Mode) ->
    From = jlib:jid_replace_resource(From_jid,<<"">>),
    Packet = #xmlel{name = <<"presence">>,
                    attrs = [{<<"xmlns">>,?NS_DEL_FRI},{<<"jid">>,To},{<<"domain">>,Domain},{<<"result">>,Rslt}],
                    children = []},

    ejabberd_router:route(From,From,Packet),

    case Rslt of 
        <<"failed">> -> ok;
        _ ->
            case Mode of 
                <<"2">> ->
                    New_Packet = #xmlel{name = <<"presence">>,
                                        attrs = [{<<"xmlns">>,?NS_DEL_FRI},{<<"type">>,<<"two_way_del">>},{<<"domain">>,From#jid.lserver}, {<<"jid">>,From#jid.luser},{<<"result">>,Rslt}],
                                        children = []},

                    case jlib:make_jid(To,Domain,<<"">>) of
                        error -> ok;
                        To_Jid -> catch ejabberd_router:route(From,To_Jid,New_Packet)
                    end;
               _ -> ok
            end
    end.


depends(_Host, _Opts) ->
    [].

mod_opt_type(_) -> fun gen_iq_handler:check_type/1.
