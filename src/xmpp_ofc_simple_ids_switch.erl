-module(xmpp_ofc_simple_ids_switch).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2,
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
                parent_pid :: pid(),
                fwd_table :: fwd_table()}).

-define(SERVER, ?MODULE).
-define(OF_VER, 4).
-define(FLOW_STATS_REQUEST_INTERVAL, 1000 * 60).
-define(THRESHOLD, 40/60).
-define(TCP_DST, <<5222:16>>).
-define(INIT_COOKIE, <<0,0,0,0,0,0,0,150>>).
-define(ETH_TYPE, 16#0800).
-define(IP_PROTO, <<6>>).
-define(INIT_FLOW_PRIORITY, 150).
-define(FLOW_PRIORITY, 200).
-define(FLOW_DROP_PRIORITY, 300).
-define(COOKIE, <<0,0,0,0,0,0,0,200>>).
-define(COOKIE_MASK, <<0,0,0,0,0,0,0,0>>).
-define(FM_TIMEOUT_S(Type), case Type of
                                idle ->
                                    30;
                                hard ->
                                    120
                            end).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link(binary(), pid()) -> {ok, pid()} | ignore | {error, term()}.
start_link(DatapathId, ParentPid) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [DatapathId, ParentPid], []),
    {ok, Pid, subscriptions(), [init_flow_mod()]}.

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

-spec handle_message(pid(),
                     {MsgType :: term(), Xid :: term(), MsgBody :: [tuple()]},
                     OFMessages) -> OFMessages when
                        OFMessages :: [ofp_message() |
                                      {Timeout :: non_neg_integer(), ofp_message()}].
handle_message(Pid, Msg, OFMessages) ->
    gen_server:call(Pid, {handle_message, Msg, OFMessages}).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([DatapathId, ParentPid]) ->
    schedule_request_flow_stats(DatapathId),
    {ok, #state{datapath_id = DatapathId, parent_pid = ParentPid, fwd_table = #{}}}.

handle_call({handle_message, {packet_in, _, MsgBody} = Msg, CurrOFMesssages},
            _From, #state{datapath_id = Dpid, fwd_table = FwdTable0} = State) ->
    case xmpp_ofc_util:packet_in_extract(reason, MsgBody) of
        action ->
            {OFMessages, FwdTable1} = handle_packet_in(Msg, Dpid, FwdTable0),
            {reply, OFMessages ++ CurrOFMesssages,
             State#state{fwd_table = FwdTable1}};
        _ ->
            {reply, CurrOFMesssages, State}
    end;
handle_call({handle_message, {flow_stats_reply, _, MsgBody} = _Msg,
            CurrOFMessages}, _From,
            #state{datapath_id = Dpid, fwd_table = FwdTable0} = State) ->
    FlowEntries = proplists:get_value(flows, MsgBody),
    case FlowEntries of
        [_H|_T] ->
            {OFMessages, FwdTable1, _} = lists:foldl(fun handle_flow_entry_stats/2,
                                      {[], FwdTable0, Dpid}, FlowEntries),
            {reply, OFMessages ++ CurrOFMessages,
             State#state{fwd_table = FwdTable1}};
        [] ->
            {reply, CurrOFMessages, State}
    end.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({request_flow_stats, DatapathId}, State) ->
    lager:info("Called: handle_info()"),
    request_flow_stats(DatapathId, State#state.parent_pid),
    schedule_request_flow_stats(DatapathId),
    {noreply, State};
handle_info({remove_entry, Dpid, SrcMac},
            #state{fwd_table = FwdTable} = State) ->
    lager:debug("Removed forwadring endty in ~p: ~p => ~p",
                [Dpid, xmpp_ofc_util:format_mac(SrcMac), maps:get(SrcMac, FwdTable)]),
    {noreply, State#state{fwd_table = maps:remove(SrcMac, FwdTable)}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

subscriptions() ->
    [packet_in, flow_stats_reply].

init_flow_mod() ->
    Matches = [{eth_type, ?ETH_TYPE}, {ip_proto, ?IP_PROTO}, {tcp_dst, ?TCP_DST}],
    Instructions = [{apply_actions, [{output, controller, no_buffer}]}],
    FlowOpts = [{table_id, 0}, {priority, ?INIT_FLOW_PRIORITY},
                {idle_timeout, 0},
                {cookie, ?INIT_COOKIE},
                {cookie_mask, ?COOKIE_MASK}],
    of_msg_lib:flow_add(?OF_VER, Matches, Instructions, FlowOpts).

handle_packet_in({_, Xid, PacketIn}, _, FwdTable0) ->
    [IpSrc, TCPSrc] = xmpp_ofc_util:packet_in_extract([ipv4_src, tcp_src], PacketIn),
    Matches = [{eth_type, ?ETH_TYPE},
               {ipv4_src, IpSrc},
               {ip_proto, ?IP_PROTO},
               {tcp_src, TCPSrc},
               {tcp_dst, ?TCP_DST}],
    Instructions = [{apply_actions, [{output, 1, no_buffer}]}],
    FlowOpts = [{table_id, 0}, {priority, ?FLOW_PRIORITY},
                {idle_timeout, 30},
                {cookie, ?COOKIE},
                {cookie_mask, ?COOKIE_MASK}],

    FM = of_msg_lib:flow_add(?OF_VER, Matches, Instructions, FlowOpts),
    PO = xmpp_ofc_util:packet_out(Xid, PacketIn, 1),
    {[FM, PO], FwdTable0}.

handle_flow_entry_stats(FlowEntry, Acc) ->
    [DurationSec, PacketCount, IPSrc, TCPSrc]
            = xmpp_ofc_util:flow_stats_reply_extract([duration_sec, packet_count, 
                                                      ipv4_src, tcp_src], FlowEntry),
    case packets_threshold_exceeded(DurationSec, PacketCount) of
        true ->
            {FlowMods, FwdTable0, Dpid} = Acc,
            FM1 = drop_flow_mod(IPSrc, TCPSrc),
            FM2 = remove_flow_mod(IPSrc, TCPSrc), 
            {[FM2, FM1] ++ FlowMods, FwdTable0, Dpid};
        false ->
            Acc
    end.

packets_threshold_exceeded(DurationSec, PacketCount) ->
    if
        PacketCount/DurationSec > ?THRESHOLD ->
             true;
        true ->
            false
    end.    

drop_flow_mod(IPSrc, TCPSrc) ->
    Matches = [{eth_type, ?ETH_TYPE},
               {ipv4_src, IPSrc},
               {ip_proto, ?IP_PROTO},
               {tcp_src, TCPSrc},
               {tcp_dst, ?TCP_DST}],
    Instructions = [{apply_actions, []}],
    FlowOpts = [{table_id, 0}, {priority, ?FLOW_DROP_PRIORITY},
                {idle_timeout, ?FM_TIMEOUT_S(idle)},
                {hard_timeout, ?FM_TIMEOUT_S(hard)},
                {cookie, ?COOKIE},
                {cookie_mask, ?COOKIE_MASK}],
    lager:info("Adding flow mod. DROP_ALL. IPSrc: ~p TCPSrc: ~p", [IPSrc, TCPSrc]),
    of_msg_lib:flow_add(?OF_VER, Matches, Instructions, FlowOpts).

remove_flow_mod(IPSrc, TCPSrc) ->
    Matches = [{eth_type, ?ETH_TYPE},
               {ipv4_src, IPSrc},
               {ip_proto, ?IP_PROTO},
               {tcp_src, TCPSrc},
               {tcp_dst, ?TCP_DST}],
    FlowOpts = [{table_id, 0}, {priority, ?FLOW_PRIORITY},
                {idle_timeout, ?FM_TIMEOUT_S(idle)},
                {hard_timeout, ?FM_TIMEOUT_S(hard)},
                {cookie, ?COOKIE},
                {cookie_mask, ?COOKIE_MASK}],
    lager:info("Removing flow mod: IPSrc: ~p TCPSrc: ~p", [IPSrc, TCPSrc]),
    of_msg_lib:flow_delete(?OF_VER, Matches, FlowOpts).


schedule_request_flow_stats(DatapathId) ->
    lager:info("Called: schedule_flow_stats_request()"),
    timer:send_after(?FLOW_STATS_REQUEST_INTERVAL, {request_flow_stats, DatapathId}).

request_flow_stats(DatapathId, ParentPid) ->
    OFMessage = request_flow_stats_message(),
    ParentPid ! {request_flow_stats, DatapathId, OFMessage}.

request_flow_stats_message() ->
    Matches = [{eth_type, ?ETH_TYPE},
               {ip_proto, ?IP_PROTO},
               {tcp_dst, ?TCP_DST}],
    TableId = 0,
    of_msg_lib:get_flow_statistics(?OF_VER, TableId, Matches,
                                               [{cookie, ?COOKIE}, 
                                                {cookie_mask, ?COOKIE_MASK}]).
 
