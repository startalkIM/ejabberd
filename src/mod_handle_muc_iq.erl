-module(mod_handle_muc_iq).
-include("logger.hrl").
-include("jlib.hrl").

-export([handle_iq/7]).

handle_iq(Server,Room,Host,<<"">>,From,To,Packet) ->
    case jlib:iq_query_info(Packet) of
    #iq{type = Type, xmlns = XMLNS, lang = _Lang,
        sub_el = #xmlel{name = SubElName} = SubEl} = IQ
    when    (XMLNS == (?NS_MUC_ADMIN)) or
            (XMLNS == (?NS_MUC_REGISTER)) or
            (XMLNS == (?NS_MUC_INVITE_V2)) or
            (XMLNS == (?NS_MUC_OWNER)) or
            (XMLNS == (?NS_CREATE_MUC)) or
            (XMLNS == (?NS_MUC_USER_SUBSCRIBE)) or
            (XMLNS == (?NS_MUC_USER_SUBSCRIBE_V2)) or
            (XMLNS == (?NS_MUC_USER_SUBSCRIBE_V2)) or
            (XMLNS == (?NS_MUC_USER_FORBIDDEN_WORDS)) or
            (XMLNS == (?NS_MUC_DEL_REGISTER)) ->
     case XMLNS of
        ?NS_MUC_ADMIN ->
            case Type of
            set -> mod_muc:recreate_muc_room(Server,Host,Room,From,<<"">>,Packet,true);
            _ -> send_error_packet(From,To,Packet)
            end;
        ?NS_MUC_OWNER ->
            case Type of
            set -> mod_muc:recreate_muc_room(Server,Host,Room,From,<<"">>,Packet,true);
            _ -> send_error_packet(From,To,Packet)
            end;
        ?NS_CREATE_MUC ->
            case Type of
            set -> mod_muc:handle_recreate_muc(Server,Room,Host,From,<<"">>,Packet, true);
            _ -> send_error_packet(From,To,Packet)
            end;
        ?NS_MUC_REGISTER ->
            case Type of
            get ->
                Res = process_iq_muc_register(From,Server,Room, Host),
                hanlde_IQ_Res(Res,IQ,SubElName,SubEl,XMLNS,To,From);
            set -> send_error_packet(From,To,Packet)
            end;
        ?NS_MUC_INVITE_V2 ->
            case Type of
            set -> mod_muc:recreate_muc_room(Server,Host,Room,From,<<"">>,Packet,true);
            _ -> send_error_packet(From,To,Packet)
            end;
        ?NS_MUC_USER_FORBIDDEN_WORDS ->
            case Type of
            set -> mod_muc:recreate_muc_room(Server,Host,Room,From,<<"">>,Packet,true);
            _ -> send_error_packet(From,To,Packet)
            end;
        ?NS_MUC_USER_SUBSCRIBE ->
            mod_muc:recreate_muc_room(Server,Host,Room,From,<<"">>,Packet,true);
        ?NS_MUC_USER_SUBSCRIBE_V2 ->
            mod_muc:recreate_muc_room(Server,Host,Room,From,<<"">>,Packet,true);
        ?NS_MUC_DEL_REGISTER ->
            case Type of
            set -> mod_muc:recreate_muc_room(Server,Host,Room,From,<<"">>,Packet,true);
            _ -> ok
            end
        end;
    _ ->
        send_error_packet(From,To,Packet)
    end;
handle_iq(_Server,_Room,_Host,_Nick,From,To,Packet) ->
    send_error_packet(From,To,Packet).

process_iq_muc_register(From,Server,Room, Host) ->
    {result,get_muc_room_users(Server,From,Room, Host)}.

hanlde_IQ_Res(Dispose_IQ,IQ,SubElName,SubEl,XMLNS,Muc_JID,From) ->
    IQRes = case Dispose_IQ of
        {result, Res} -> IQ#iq{type = result, sub_el = [#xmlel{name = SubElName, attrs = [{<<"xmlns">>,XMLNS}],children = Res}]};
        {error, Error} -> IQ#iq{type = error,sub_el = [SubEl, Error]}
    end,
    ejabberd_router:route(Muc_JID, From,jlib:iq_to_xml(IQRes)).

send_error_packet(From,To,Packet) ->
    Lang = <<"en">>,
    ErrText = <<"Conference room does not exist">>,
    Err = jlib:make_error_reply(Packet,?ERRT_ITEM_NOT_FOUND(Lang,ErrText)),
    ejabberd_router:route(To, From, Err).

make_muc_user_attrs(User,Host,UL) ->
    case jlib:jid_to_string({User,Host,<<"">>}) of
        error -> [{<<"jid">>,<<"">>}];
        JID ->
            Aff = proplists:get_value(JID,UL,none),
            if  Aff =:= admin -> [{<<"jid">>,JID},{<<"affiliation">>,<<"admin">>}];
                Aff =:= owner -> [{<<"jid">>,JID},{<<"affiliation">>,<<"owner">>}];
                true -> [{<<"jid">>,JID}]
            end
    end.

get_muc_room_users(Server,From,Room, Host) ->
    case catch qtalk_sql:get_muc_users(Server,Room, Host) of
        {selected, _, Res} ->
            UL = lists:foldl(fun([U, H], Acc) ->
                case str:str(H,<<"conference">>) of
                    0 -> [{U, H}|Acc];
                    _ -> Acc
                end
            end, [], Res),
            handle_muc_room_users_attrs(Server, Room,From,UL);
        _ -> []
    end.

handle_muc_room_users_attrs(Server, Room,From,UL) ->
    UserL = get_room_affiliation(Server, Room),
    case lists:member({From#jid.luser,From#jid.lserver},UL) of
    true ->
        lists:map(fun ({User,Host}) ->
            Attrs = make_muc_user_attrs(User,Host,UserL),
                #xmlel{name = <<"m_user">>,
                    attrs =     Attrs,
                     children = []} end,UL);
    _ ->
        catch monitor_util:monitor_count(<<"muc_users_empty">>, 1),
        ?ERROR_MSG("the muc_users is [] for ~p~n", [{Room,From,UL, UserL}]),
        []
    end.

get_room_affiliation(Server, Room) ->
    case catch ejabberd_sql:sql_query(Server, [<<"select opts from muc_room ">>,<<"where name='">>, Room, <<"';">>]) of
        {selected, [<<"opts">>], [[Opts]]} ->
            get_affction_opts(ejabberd_sql:decode_term(Opts));
        _ -> []
    end.

get_affction_opts(Opts) ->
    lists:flatmap(
      fun({affiliations, Affs}) ->
             lists:flatmap(
                 fun({{U, S, _R}, {NewAff,<<"">>}}) ->
                     [{jlib:jid_to_string({iolist_to_binary(U), iolist_to_binary(S),<<"">>}),
                           NewAff}];
                    (_) ->
                        []
                    end, Affs);
         (_) ->
              []
      end, Opts).
