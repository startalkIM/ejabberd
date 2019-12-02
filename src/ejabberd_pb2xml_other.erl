-module(ejabberd_pb2xml_other).

-include("message_pb.hrl").
-include("jlib.hrl").

-export([parse_iq_message/1]).

parse_iq_message(Pb_message) ->
	case catch message_pb:decode_iqmessage(Pb_message#protomessage.message) of
 	IQ when is_record(IQ,iqmessage)  ->
		make_iq_message(IQ#iqmessage.key,IQ#iqmessage.value,
					Pb_message#protomessage.from,Pb_message#protomessage.to,Pb_message#protomessage.signaltype,
					IQ#iqmessage.messageid,IQ#iqmessage.body);
	_ -> false
	end.

make_iq_message("BIND",Value,_From,_To,_Type,ID,_Body) ->
	Xml = 
		#xmlel{name = <<"iq">>, 
			attrs = [{<<"id">>,list_to_binary(ID)},{<<"type">>,<<"set">>}], 
			children = [#xmlel{name = <<"bind">>,
						   attrs = [{<<"xmlns">>,<<"urn:ietf:params:xml:ns:xmpp-bind">>}],
								   children = [#xmlel{name = <<"resource">>, 
						   				  attrs = [],children = [{'xmlcdata',list_to_binary(Value)}]}]}]},
	{xmlstreamelement,Xml};
make_iq_message(_,_,_,_,_,_,_) ->
	false.


