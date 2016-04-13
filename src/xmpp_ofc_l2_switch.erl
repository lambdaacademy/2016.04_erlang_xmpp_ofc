-module(xmpp_ofc_l2_switch).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,
         stop/1,
         handle_message/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Includes & Type Definitions & Macros
%% ------------------------------------------------------------------

-include_lib("of_protocol/include/of_protocol.hrl").
-include_lib("of_protocol/include/ofp_v4.hrl").
-include("xmpp_ofc_v4.hrl").

-type fwd_table() :: #{MacAddr :: string() => SwitchPort :: integer()}.
-record(state, {datapath_id :: binary(),
                fwd_table :: fwd_table()}).

-define(SERVER, ?MODULE).
-define(OF_VER, 4).
-define(ENTRY_TIMEOUT, 30*1000).
-define(FM_TIMEOUT_S(Type), case Type of
                                idle ->
                                    10;
                                hard ->
                                    30
                            end).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link(binary()) -> {ok, pid()} | ignore | {error, term()}.
start_link(DatapathId) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [DatapathId], []),
    {ok, Pid, subscriptions(), [init_flow_mod()]}.

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

-spec handle_message(pid(),
                     {MsgType :: term(),
                      Xid :: term(),
                      MsgBody :: [tuple()]},
                     [ofp_message()]) -> [ofp_message()].
handle_message(Pid, Msg, OFMessages) ->
    gen_server:call(Pid, {handle_message, Msg, OFMessages}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([DatapathId]) ->
    {ok, #state{datapath_id = DatapathId, fwd_table = #{}}}.


handle_call({handle_message, {packet_in, _, MsgBody} = Msg, CurrOFMesssages},
            _From, #state{datapath_id = Dpid,
                          fwd_table = FwdTable0} = State) ->
    case packet_in_extract(reason, MsgBody) of
        action ->
            {OFMessages, FwdTable1} = handle_packet_in(Msg, Dpid, FwdTable0),
            {reply, OFMessages ++ CurrOFMesssages,
             State#state{fwd_table = FwdTable1}};
        _ ->
            {reply, CurrOFMesssages, State}
    end.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info({remove_entry, Dpid, SrcMac},
            #state{fwd_table = FwdTable} = State) ->
    lager:debug("Removed forwarding entry in ~p: ~p => ~p",
                [Dpid, format_mac(SrcMac), maps:get(SrcMac,
                                                    FwdTable)]),
    {noreply, State#state{fwd_table = maps:remove(SrcMac, FwdTable)}}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

subscriptions() ->
    [packet_in].

init_flow_mod() ->
    Matches = [],
    Instructions = [{apply_actions, [{output, controller, no_buffer}]}],
    FlowOpts = [{table_id, 0}, {priority, 10},
                {idle_timeout, 0},
                {idle_timeout, 0},
                {cookie, <<0,0,0,0,0,0,0,1>>},
                {cookie_mask, <<0,0,0,0,0,0,0,0>>}],
    of_msg_lib:flow_add(?OF_VER, Matches, Instructions, FlowOpts).


handle_packet_in({_, Xid, PacketIn}, DatapathId, FwdTable0) ->
    FwdTable1  = learn_src_mac_to_port(PacketIn, DatapathId, FwdTable0),
    case get_port_for_dst_mac(PacketIn, FwdTable0) of
        undefined ->
            {[packet_out(Xid, PacketIn, flood)], FwdTable1};
        PortNo ->
            {[flow_to_dst_mac(PacketIn, PortNo),
              packet_out(Xid, PacketIn, PortNo)],
             FwdTable1}
    end.

learn_src_mac_to_port(PacketIn, Dpid, FwdTable0) ->
    [InPort, SrcMac] = packet_in_extract([in_port, src_mac], PacketIn),
    case maps:get(SrcMac, FwdTable0, undefined) of
        InPort ->
            FwdTable0;
        _ ->
            FwdTable1 = maps:put(SrcMac, InPort, FwdTable0),
            schedule_remove_entry(Dpid, SrcMac),
            lager:debug("Added forwarding entry in ~p: ~p => ~p",
                        [Dpid, format_mac(SrcMac), InPort]),
            FwdTable1
    end.


get_port_for_dst_mac(PacketIn, FwdTable) ->
    DstMac = packet_in_extract(dst_mac, PacketIn),
    case maps:find(DstMac, FwdTable) of
        error ->
            undefined;
        {ok, Port} ->
            Port
    end.

flow_to_dst_mac(PacketIn, OutPort) ->
    [InPort, DstMac] = packet_in_extract([in_port, dst_mac], PacketIn),
    Matches = [{in_port, InPort}, {eth_dst, DstMac}],
    Instructions = [{apply_actions, [{output, OutPort, no_buffer}]}],
    FlowOpts = [{table_id, 0}, {priority, 100},
                {idle_timeout, ?FM_TIMEOUT_S(idle)},
                {idle_timeout, ?FM_TIMEOUT_S(hard)},
                {cookie, <<0,0,0,0,0,0,0,10>>},
                {cookie_mask, <<0,0,0,0,0,0,0,0>>}],
    of_msg_lib:flow_add(?OF_VER, Matches, Instructions, FlowOpts).


packet_out(Xid, PacketIn, OutPort) ->
    Actions = [{output, OutPort, no_buffer}],
    {InPort, BufferIdOrPacketPortion} =
        case packet_in_extract(buffer_id, PacketIn) of
            no_buffer ->
                list_to_tuple(packet_in_extract([in_port, data],
                                                PacketIn));
            BufferId when is_integer(BufferId) ->
                {packet_in_extract(in_port, PacketIn), BufferId}
        end,
    PacketOut =  of_msg_lib:send_packet(?OF_VER,
                                        BufferIdOrPacketPortion,
                                        InPort,
                                        Actions),
    PacketOut#ofp_message{xid = Xid}.

packet_in_extract(Elements, PacketIn) when is_list(Elements) ->
    [packet_in_extract(H, PacketIn) || H <- Elements];
packet_in_extract(src_mac, PacketIn) ->
    <<_:6/bytes, SrcMac:6/bytes, _/binary>> = proplists:get_value(data, PacketIn),
    SrcMac;
packet_in_extract(dst_mac, PacketIn) ->
    <<DstMac:6/bytes, _/binary>> = proplists:get_value(data, PacketIn),
    DstMac;
packet_in_extract(in_port, PacketIn) ->
    <<InPort:32>> = proplists:get_value(in_port, proplists:get_value(match, PacketIn)),
    InPort;
packet_in_extract(buffer_id, PacketIn) ->
    proplists:get_value(buffer_id, PacketIn);
packet_in_extract(data, PacketIn) ->
    proplists:get_value(data, PacketIn);
packet_in_extract(reason, PacketIn) ->
    proplists:get_value(reason, PacketIn).

format_mac(MacBin) ->
    Mac0 = [":" ++ integer_to_list(X, 16) || <<X>> <= MacBin],
    tl(lists:flatten(Mac0)).

schedule_remove_entry(SrcMac, Dpid) ->
    {ok, _Tref} = timer:send_after(?ENTRY_TIMEOUT,
                                   {remove_entry, Dpid, SrcMac}).

