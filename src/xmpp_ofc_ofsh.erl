-module(xmpp_ofc_ofsh).

-export([init/7, handle_message/2, terminate/1]).

-record(state, {switch_dpid :: binary()}).

-include_lib("ofs_handler/include/ofs_handler.hrl").

init(_Mode, _Ip, DatapathId, _Features, _Version, _Connection, _Options) ->
    ok = xmpp_ofc_gen_switch:open_connection(DatapathId),
    {ok, #state{switch_dpid = DatapathId}}.

handle_message(Msg, #state{switch_dpid = Dpid}) ->
    xmpp_ofc_gen_switch:handle_message(Dpid, Msg).

terminate(#state{switch_dpid = Dpid}) ->
    xmpp_ofc_gen_switch_logic:terminate_connection(Dpid).
