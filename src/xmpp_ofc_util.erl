-module(xmpp_ofc_util).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([packet_in_extract/2, packet_out/3, format_mac/1]).

%% ------------------------------------------------------------------
%% Includes & Type Definitions & Macros
%% ------------------------------------------------------------------

-include_lib("of_protocol/include/of_protocol.hrl").

-define(OF_VER, 4).

%% ------------------------------------------------------------------
%% Function Definitions
%% ------------------------------------------------------------------


packet_in_extract(Elements, PacketIn) when is_list(Elements) ->
    [packet_in_extract(H, PacketIn) || H <- Elements];
packet_in_extract(src_mac, PacketIn) ->
    <<_:6/bytes, SrcMac:6/bytes, _/binary>> = proplists:get_value(data, PacketIn),
    SrcMac;
packet_in_extract(dst_mac, PacketIn) ->
    <<DstMac:6/bytes, _/binary>> = proplists:get_value(data, PacketIn),
    DstMac;
packet_in_extract(ipv4_src, Packetin) ->
    <<_:26/bytes, IpSrc:4/bytes, _/binary>> = proplists:get_value(data, Packetin),
    IpSrc;
packet_in_extract(ipv4_dst, Packetin) ->
    <<_:28/bytes, IpDst:4/bytes, _/binary>> = proplists:get_value(data, Packetin),
    IpDst;
packet_in_extract(tcp_src, Packetin) ->
    <<_:34/bytes, TCPSrc:2/bytes, _/binary>> = proplists:get_value(data, Packetin),
    TCPSrc;
packet_in_extract(in_port, PacketIn) ->
    <<InPort:32>> = proplists:get_value(in_port, proplists:get_value(match, PacketIn)),
    InPort;
packet_in_extract(buffer_id, PacketIn) ->
    proplists:get_value(buffer_id, PacketIn);
packet_in_extract(data, PacketIn) ->
    proplists:get_value(data, PacketIn);
packet_in_extract(reason, PacketIn) ->
    proplists:get_value(reason, PacketIn).


packet_out(Xid, PacketIn, OutPort) ->
    Actions = [{output, OutPort, no_buffer}],
    {InPort, BufferIdOrPacketPortion} = 
        case packet_in_extract(buffer_id, PacketIn) of
            no_buffer ->
                list_to_tuple(packet_in_extract([in_port, data], PacketIn));
            BufferId when is_integer(BufferId) ->
                {packet_in_extract(in_port, PacketIn), BufferId}
        end,
    PacketOut = of_msg_lib:send_packet(?OF_VER, BufferIdOrPacketPortion, InPort, Actions),
    PacketOut#ofp_message{xid = Xid}.


format_mac(MacBin) ->
    Mac0 = [":" ++ integer_to_list(X, 16) || <<X>> <= MacBin],
    tl(lists:flatten(Mac0)).

