-module(xmpp_ofc_l2_switch_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("of_protocol/include/of_protocol.hrl").
-include_lib("of_protocol/include/ofp_v4.hrl").

-define(DATAPATH_ID, "00:00:00:00:00:00:00:0B").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

init_per_testcase(_TestCase, Config) ->
    {ok, Pid, _, _} = xmpp_ofc_l2_switch:start_link(?DATAPATH_ID),
    [{pid, Pid} | Config].

end_per_testcase(_TestCase, Config) ->
    ok = xmpp_ofc_l2_switch:stop(?config(pid, Config)).

all() ->
    [l2_mod_sends_packet_out].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

l2_mod_sends_packet_out(Config) ->
    {ok, [PacketIn]} =
        file:consult(filename:join([?config(data_dir, Config),
                                    "packet_in.data"])),
    Response = xmpp_ofc_l2_switch:handle_message(?config(pid, Config),
                                                 PacketIn,
                                                 []),
    ?assert(lists:any(
              fun(#ofp_message{body = #ofp_packet_out{}}) -> true;
                 (_) -> false
              end, Response)).
