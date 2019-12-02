%% Feel free to use, reuse and abuse the code in this file.

-module(http_muc_vcard_presence).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-export([send_update_vcard_presence/1]).
-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-record(muc_online_room,
          {name_host = {<<"">>, <<"">>} :: {binary(), binary()} | '$1' |{'_', binary()} | '_', pid = self() :: pid() | '$2' | '_' | '$1'}).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req1} = cowboy_req:method(Req),
    case Method of 
        <<"GET">> ->
            {ok, Req2} = get_echo(Method,Req1),
            {ok, Req2, State};
        _ ->
            {ok,Req2} = echo(undefined, Req1),
            {ok, Req2, State}
    end.
    	
get_echo(<<"GET">>,Req) ->
    {Muc_name, Req1} = cowboy_req:qs_val(<<"muc_name">>, Req),
    send_update_vcard_presence(Muc_name),
    cowboy_req:reply(200, [ {<<"content-type">>, <<"text/plain; charset=utf-8">>}], <<"ok">>, Req1);
get_echo(<<"Get">>,Req) -> cowboy_req:reply(405, Req).

echo(undefined, Req) -> cowboy_req:reply(400, [], <<"Missing parameter.">>, Req);
echo(Echo, Req) -> cowboy_req:reply(200, [ {<<"content-type">>, <<"text/plain; charset=utf-8">>}], Echo, Req).

terminate(_Reason, _Req, _State) -> ok.

send_update_vcard_presence(Name) ->
    Servers = ejabberd_config:get_myhosts(),
    Server = lists:nth(1,Servers),
    send_update_vcard_presence(Server,Name).

send_update_vcard_presence(Server,Name) ->
    Room_server = case catch str:str(Name,<<"@conference.">>) of
        0 -> 
            case catch ejabberd_sql:sql_query([<<"select host from muc_room where name = '">>,Name,<<"';">>]) of
                {selected,[<<"host">>],[[H]]} -> H;
                _ -> str:concat(<<"conference.">>,Server)
            end;
        N1 when is_integer(N1) -> str:substr(Name,N1+1,size(Name)-N1);
        _ -> Server
    end,

    Muc_Name = case str:str(Name,<<"@conference.">>) of
        0 -> Name;
        N -> str:substr(Name,1,N-1)
    end,

    case mod_muc_redis:get_muc_room_pid(Muc_Name,Room_server) of
        [] -> muc_vcard_update(Server,Room_server,Muc_Name,Name);
        [R] ->
            Pid = R#muc_online_room.pid,
            Pid ! muc_vcard_update
    end.

muc_vcard_update(Server,Room_server,Muc_name,Name) ->
    case catch ejabberd_sql:sql_query(Server,	
            [<<"select show_name,muc_desc,muc_title,muc_pic,version from muc_vcard_info where  muc_name = '">>,
             qtalk_public:concat(Muc_name,<<"@">>,Room_server),<<"' or muc_name = '">>,Muc_name,<<"' or muc_name = '">>,Name,<<"';">>]) of
        {selected, _ , [[S,D,T,P,V]]} ->
            MUC_JID = jlib:make_jid(Muc_name,Room_server,<<"">>),
            Packet = #xmlel{name = <<"presence">>,
                            attrs = [{<<"xmlns">>,<<"http://jabber.org/protocol/muc#vcard_update">>}],
                            children = [#xmlel{name = <<"vcard_updte">>,
                                               attrs = [{<<"nick">>,qtalk_public:get_pg_default_val(S,<<"">>)},
                                                        {<<"desc">>,qtalk_public:get_pg_default_val(D,<<"">>)},
                                                        {<<"title">>,qtalk_public:get_pg_default_val(T,<<"">>)},
                                                        {<<"pic">>,qtalk_public:get_pg_default_val(P,<<"">>)},
                                                        {<<"version">>,qtalk_public:get_pg_default_val(V,<<"0">>)}],
                                               children =  [] }]},
           case catch ejabberd_sql:sql_query(Server, [<<"select username,host from muc_room_users where muc_name = '">>,Muc_name,<<"';">>]) of
               {selected, _ , UL} when is_list(UL) ->
                   lists:foreach(fun([U, H]) -> 
                       case jlib:make_jid(U,H,<<"">>) of
                           error -> ok;
                           JID -> ejabberd_router:route(MUC_JID,JID, Packet)
                       end
                   end, UL);
               _ -> ok
           end;
        _ -> ok
    end.
