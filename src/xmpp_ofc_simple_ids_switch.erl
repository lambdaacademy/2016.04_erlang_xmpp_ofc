-module(xmpp_ofc_simple_ids_switch).
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
            _From, #state{datapath_id = Dpid, fwd_table = FwdTable0} = State) ->
    case xmpp_ofc_util:packet_in_extract(reason, MsgBody) of
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
    [packet_in].

init_flow_mod() ->
    Matches = [{eth_type, 16#0800}, {ip_proto, <<6>>}, {tcp_dst, <<5222:16>>}],
    Instructions = [{apply_actions, [{output, controller, no_buffer}]}],
    FlowOpts = [{table_id, 0}, {priority, 150},
                {idle_timeout, 0},
                {cookie, <<0,0,0,0,0,0,0,150>>},
                {cookie_mask, <<0,0,0,0,0,0,0,0>>}],
    of_msg_lib:flow_add(?OF_VER, Matches, Instructions, FlowOpts).

handle_packet_in({_, Xid, PacketIn}, _, FwdTable0) ->
    [IpSrc, TCPSrc] = xmpp_ofc_util:packet_in_extract([ipv4_src, tcp_src], PacketIn),
    Matches = [{eth_type, 16#0800},
               {ipv4_src, IpSrc},
               {ip_proto, <<6>>},
               {tcp_src, TCPSrc},
               {tcp_dst, <<5222:16>>}],
    Instructions = [{apply_actions, [{output, 1, no_buffer}]}],
    FlowOpts = [{table_id, 0}, {priority, 200},
                {idle_timeout, 30},
                {cookie, <<0,0,0,0,0,0,0,200>>},
                {cookie_mask, <<0,0,0,0,0,0,0,0>>}],

    FM = of_msg_lib:flow_add(?OF_VER, Matches, Instructions, FlowOpts),
    PO = xmpp_ofc_util:packet_out(Xid, PacketIn, 1),
    {[FM, PO], FwdTable0}.

 
