-module(ejabberd_reload_hosts).

-include("ejabberd_config.hrl").
-include("logger.hrl").
-include("ejabberd.hrl").

-export([init_db_host/1,add_hosts/1,get_hosts/0, remove_host/1]).


add_hosts(Host) ->
    Hosts = ejabberd_config:get_myhosts(),
    case  lists:member(Host,Hosts) of
    true ->
        false;
    _ ->
         reload_host(Host)
    end.


init_db_host(Server) ->
	case catch ejabberd_sql:sql_query(Server,[<<"select distinct(host) from host_info">>]) of
	{selected,_,L} when is_list(L) ->
		lists:foreach(fun([H]) ->
			reload_host(H) end,L);
	_ ->
		ets:insert(local_config ,#local_config{key = db_hosts,value  = []})
	end.
	
%%1.add host to dn
%%2.add route 
%%3.add mod_muc route
reload_host(Host) ->
	?INFO_MSG("reload_host ~p ~n",[Host]),
	case catch ets:lookup(local_config,{global,db_hosts}) of
	[DHost] when is_record(DHost,local_config) ->
		V = DHost#local_config.value,	
		case  lists:member(Host,V) of
		true ->
			false;
		_ ->
			if V == [] ->
				ets:insert(local_config ,DHost#local_config{key = {global,db_hosts},value  = [Host] }),
				true;
			true ->
				ets:insert(local_config ,DHost#local_config{key = {global,db_hosts},value  = [Host] ++ V }),
				add_db_host(Host)
			end
		end;
	[] ->
		ets:insert(local_config ,#local_config{key = {global,db_hosts},value  = [Host] }),
		add_db_host(Host);
	_ ->
		false
	end.

add_db_host(Host) ->
	case ?SERVERTYPE of
		c2s -> whereis(ejabberd_local) ! {register_route,Host};
		muc ->
			case catch gen_mod:start_module(Host,mod_muc) of
				ok -> true;
				_ -> false
			end;
		all ->
			whereis(ejabberd_local) ! {register_route,Host},
			case catch gen_mod:start_module(Host,mod_muc) of
				ok -> true;
				_ -> false
			end
	end.

get_hosts() ->
	case catch ets:lookup(local_config, {global,db_hosts}) of
	[DHost] when is_record(DHost,local_config) ->
		DHost#local_config.value;
	_ ->
		[]
	end.
		
remove_host(Host) ->
    Hosts = ejabberd_config:get_myhosts(),
    case  lists:member(Host,Hosts) of
    false ->
        false;
    _ ->
         does_remove_host(Host)
    end.

does_remove_host(Host) ->
	?INFO_MSG("reload_host ~p ~n",[Host]),
	case catch ets:lookup(local_config,{global,db_hosts}) of
	[DHost] when is_record(DHost,local_config) ->
		V = DHost#local_config.value,	
		case  lists:member(Host,V) of
		false ->
			false;
		_ ->
			NewHost = lists:delete(Host, V),
			ets:insert(local_config ,DHost#local_config{key = {global,db_hosts},value  = NewHost}),
			remove_db_host(Host)
		end;
	_ ->
		false
	end.

remove_db_host(Host) ->
	case ?SERVERTYPE of
		c2s -> whereis(ejabberd_local) ! {unregister_route,Host};
		muc ->
			case catch gen_mod:stop_module(Host,mod_muc) of
				ok -> true;
				_ -> false
			end;
		all ->
			whereis(ejabberd_local) ! {unregister_route,Host},
			case catch gen_mod:stop_module(Host,mod_muc) of
				ok -> true;
				_ -> false
			end
	end.
