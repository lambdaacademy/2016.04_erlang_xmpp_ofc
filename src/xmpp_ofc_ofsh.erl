-module(xmpp_ofc_ofsh).

-export([init/7, handle_message/2, terminate/1]).

-record(state, {gen_switch_pid :: pid()}).

-include_lib("ofs_handler/include/ofs_handler.hrl").

init(_Mode, _Ip, DatapathId, _Features, _Version, _Connection, _Options) ->
    {ok, Pid} = xmpp_ofc_gen_switch:open_connection(DatapathId),
    {ok, #state{gen_switch_pid = Pid}}.

handle_message(Msg, #state{gen_switch_pid = Pid}) ->
    xmpp_ofc_gen_switch:handle_message(Pid, Msg).

terminate(#state{gen_switch_pid = Pid}) ->
    xmpp_ofc_gen_switch_logic:terminate_connection(Pid).
