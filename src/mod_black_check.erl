-module(mod_black_check).

-behaviour(gen_mod).

-export([start/2, stop/1,
	 check_packet/6,
	 mod_opt_type/1, depends/2]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

start(Host, _Opts) ->
    ejabberd_hooks:add(privacy_check_packet, Host, ?MODULE,
		       check_packet, 50).

stop(Host) ->
    ejabberd_hooks:delete(privacy_check_packet, Host,
			  ?MODULE, check_packet, 50).

%% From is the sender, To is the destination.
%% If Dir = out, User@Server is the sender account (From).
%% If Dir = in, User@Server is the destination account (To).
check_packet(_, _User, _Server, _UserList,
	     {From, To, #xmlel{name = <<"message">>, attrs = Attrs}},
	     out) ->
    FromStr = jid:to_string(jid:replace_resource(To, <<"">>)),
    ToStr = jid:to_string(jid:replace_resource(From, <<"">>)),
    Type = fxml:get_attr_s(<<"type">>, Attrs),
    case do_check_packet(FromStr, ToStr, Type) of
        deny -> deny;
        R ->
            case ejabberd_config:get_option(message_permissions, fun(Url)-> Url end, undefined) of
                true -> do_check_priv(From, To, Type);
                _ -> R
            end
    end;
check_packet(_, _User, _Server, _UserList, _, _) ->
    allow.

do_check_packet(_FromStr, _ToStr, <<"readmark">>) ->
    allow;
do_check_packet(_FromStr, _ToStr, <<"error">>) ->
    allow;
do_check_packet(FromStr, ToStr, _) ->
    case mod_redis:hash_get(11, <<"blacklist:", FromStr/binary>>, ToStr) of
        {ok, undefined} -> allow;
        _ -> deny 
    end.

do_check_priv(From, To, <<"chat">>) ->
    case catch  ejabberd_sql:sql_query([<<"select relationship from user_friends where username='">>, From#jid.luser, <<"' and userhost='">>, From#jid.lserver, <<"' and friend ='">>, To#jid.luser, <<"' and host='">>, To#jid.lserver, <<"';">>]) of
        {selected, _, [[<<"1">>]]} -> allow;
        _ -> deny
    end;
do_check_priv(_, _, _) -> allow.

depends(_Host, _Opts) ->
    [].

mod_opt_type(_) -> [].
