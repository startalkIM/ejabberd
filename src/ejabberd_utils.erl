-module(ejabberd_utils).

-export([get_host/0]).

get_host() ->
    case ejabberd_config:get_option(hosts, fun(X) -> X end) of
        [Host|_] ->
            Host;
        _ ->
            <<"">>
    end.
