-module(http_client).

-export([http_post/7,http_get/5]).
-export([http_post/6,http_get/4]).

-include("ejabberd.hrl").
-include("logger.hrl").

http_post(Url, Header, Type, Body, HTTPOptions, Options) ->
    http_post(1, Url, Header, Type, Body, HTTPOptions, Options).

http_post(_Host,Url, Header, Type, Body, HTTPOptions, Options) ->
    httpc:request(post, {Url, Header, Type, Body}, HTTPOptions, Options).

http_get(Url, Header, HTTPOptions, Options) ->
    http_get(1,Url, Header, HTTPOptions, Options).

http_get(_Host,Url, Header, HTTPOptions, Options) ->
    httpc:request(get, {Url, Header}, HTTPOptions, Options).
