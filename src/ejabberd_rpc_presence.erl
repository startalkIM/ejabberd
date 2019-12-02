-module(ejabberd_rpc_presence).

-export([send_notify_presence/4, get_notify_presence/4]).
-export([send_login_presence/1]).

-include("jlib.hrl").

send_notify_presence(From, To, Catagory, Data) ->
    JFrom = jlib:string_to_jid(From),
    JTo = jlib:string_to_jid(To),
    Xml = get_notify_presence(From, To, Catagory, Data),
    ejabberd_router:route(JFrom, JTo, Xml).

get_notify_presence(From, To, Catagory, Data) ->
    #xmlel{name = <<"presence">>, 
           attrs = [{<<"from">>, From},
                    {<<"to">>, To},
                    {<<"category">>, Catagory},
                    {<<"data">>, Data},
                    {<<"type">>, <<"notify">>}], 
           children = [#xmlel{name = <<"notify">>, attrs = [{<<"xmlns">>, <<"jabber:x:presence_notify">>}], children = []}]}.

send_login_presence(User) ->
    Resources = ejabberd_sm:get_user_present_resources(User#jid.luser, User#jid.lserver),
    Rs = lists:foldl(fun({_, R}, Acc) ->
        [_, P|_] = binary:split(R, <<"_">>, [global]),
        [P|Acc]
    end, [], Resources),

    Data = jiffy:encode(Rs),
    Catagory = <<"9">>,
    UserString = jid:to_string(jid:remove_resource(User)),
    send_notify_presence(UserString, UserString, Catagory, Data).
