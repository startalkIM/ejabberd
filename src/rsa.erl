-module(rsa).
-export([enc/1,dec/1,test/1]).

read_rsa_key(PemBin) ->
    [Entry] = public_key:pem_decode(PemBin),
    public_key:pem_entry_decode(Entry).

rsa_public_key() ->
    read_rsa_key(ejabberd_config:get_option(rsa_public_key, fun(Key)-> Key end, <<"">>)).

rsa_private_key() ->
    read_rsa_key(ejabberd_config:get_option(rsa_private_key, fun(Key)-> Key end, <<"">>)).

enc(PlainText) ->
    public_key:encrypt_public(PlainText, rsa_public_key()).

dec(CipherText)->
    public_key:decrypt_private(CipherText, rsa_private_key()).

test(Msg) ->
    CipherText = enc(Msg),
    io:format("plain text:~p, cipher text:~p~n", [Msg, CipherText]),
    PlainText = dec(CipherText),
    io:format("plain text after decode:~p~n", [PlainText]).
