-module(utils).
-include("logger.hrl").
-include("ejabberd.hrl").

-compile(export_all).

-spec http_get(string()) -> {ok | error, string(), string()}.
http_get(Url) ->
    case http_client:http_get(?LSERVER, Url, [{"connection", "close"}], [], []) of
		{ok, {_Status, Body}} ->
			{ok, "", Body};
		{ok, {_StatusLine, Headers, Body}} ->
			{ok, Headers, Body};
		{error,Reason} ->
			{error, "", Reason}
	end.

-spec http_post(string(), string()) -> {ok | error, string(), string()}.
http_post(Url, Data) ->
    case http_client:http_post(?LSERVER, Url, [{"connection", "close"}], "application/x-www-form-urlencoded", Data, [], []) of
		{ok, {_Status, Body}} ->
			{ok, "", Body};
		{ok, {_StatusLine, Headers, Body}} ->
			{ok, Headers, Body};
		{error,Reason} ->
			{error, "", Reason}
	end.

-spec http_post_json(string(), string()) -> {ok | error, string(), string()}.
http_post_json(Url, Data) ->
    case http_client:http_post(?LSERVER, Url, [{"connection", "close"}], "application/json", Data, [], []) of
		{ok, {_Status, Body}} ->
			{ok, "", Body};
		{ok, {_StatusLine, Headers, Body}} ->
			{ok, Headers, Body};
		{error,Reason} ->
			{error, "", Reason}
	end.

% 获取当前时间的秒数
now_seconds() ->
	{MegaSecs, Secs, _} = os:timestamp(),
	MegaSecs*1000000+Secs.

getHttpParameter(Key, Req) ->
    {Value, _Req1} = cowboy_req:qs_val(Key, Req, <<"">>),
	binary_to_list(Value).

%% userstatus 0:离线，6:在线
-spec getUserStatus(string()) -> integer().
getUserStatus(StrUser) ->
	Host = ejabberd_config:get_option(hosts, fun iolist_to_binary/1),
	http_getuserstatus:get_user_status(binary_to_list(Host), StrUser).

safe_binary_to_integer(Binary) ->
    try
		binary_to_integer(Binary)
	catch
		_:_ -> 0
	end.

safe_list_to_integer(Binary) ->
    try
		list_to_integer(Binary)
	catch
		_:_ -> 0
	end.

getJid(Uin) ->
	Hosts = ejabberd_config:get_option(hosts, fun iolist_to_binary/1),
	Jid = jlib:make_jid(list_to_binary(Uin), Hosts, <<"">>),
	Jid.

getRoutePacket(_From, To, Msg) ->
	Pack = fxml:to_xmlel({xmlel
			,"message"
			,[{"type","chat"},{"id","web9999"},{"to",jlib:jid_to_string(To)}]
			,[{xmlel,"active",[{"xmlns","http://jabber.org/protocol/chatstates"}],[]},{xmlel,"body",[],[{xmlcdata, Msg}]}]}),
	Pack.

sendUserMsg(FromUin, ToUin, Msg) ->
	From = utils:getJid(FromUin),
	To = utils:getJid(ToUin),
	Packet = utils:getRoutePacket(From, To, Msg),
	ejabberd_router:route(From, To, Packet),
	?DEBUG("send msg From=~p To=~p msg=~p", [FromUin, ToUin, Msg]).
