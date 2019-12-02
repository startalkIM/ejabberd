-module(http_dispatch).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-include("ejabberd.hrl").
-include("logger.hrl").

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Path, _} = cowboy_req:path_info(Req),
    {ok, Req1} = handle_process(Path, Req),
    {ok, Req1, State}.

terminate(_Reason, _Req, _State) ->
    ok.

handle_process([<<"send_thirdmessage">>], Req) ->
    http_send_thirdmessage:handle(Req);
handle_process([<<"send_xml">>], Req) ->
    http_send_xml:handle(Req);
handle_process([<<"send_notify">>], Req) ->
    http_send_notify:handle(Req);
handle_process([<<"create_muc">>], Req) ->
    http_muc_create:handle(Req);
handle_process([<<"add_muc_user">>], Req) ->
    http_muc_add_user:handle(Req);
handle_process([<<"del_muc_user">>], Req) ->
    http_muc_del_user:handle(Req);
handle_process([<<"destroy_muc">>], Req) ->
    http_muc_destroy:handle(Req);
handle_process([<<"muc_users">>], Req) ->
    http_muc_users:handle(Req);
handle_process([<<"send_notice_vcard">>], Req) ->
    http_muc_notice_vcard:handle(Req);
handle_process([<<"management">>, <<"change_muc_opts">>], Req) ->
    http_management_change_muc_opts:handle(Req);
handle_process([<<"management">>, <<"get_muc_opts">>], Req) ->
    http_management_get_muc_opts:handle(Req);
handle_process([<<"management">>, <<"del_muc_user">>], Req) ->
    http_management_del_muc_user:handle(Req);
handle_process([<<"add_host">>], Req) ->
    http_host_add:handle(Req);
handle_process([<<"auth_uk">>], Req) ->
    http_auth_uk:handle(Req);
handle_process([<<"get_user_nick">>], Req) ->
    http_get_user_nick:handle(Req);
handle_process([<<"send_message">>], Req) ->
    http_send_message:handle(Req);
handle_process([<<"clear_staff">>], Req) ->
    http_clear_staff:handle(Req);
handle_process(_, Req) ->
http_utils:cowboy_req_reply_json(http_utils:gen_fail_result(1, <<"request not defined">>), Req).
