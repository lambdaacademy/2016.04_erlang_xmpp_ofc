xmpp_ofc - XMPP Controller
=====

An OTP application

Build
-----

$ rebar3 compile

## References ##

#### General Erlang ####

* [Offical Erlang Documentation](http://www.erlang.org/doc/)
* [Erlang Reference Manual User's Guide](http://www.erlang.org/doc/reference_manual/users_guide.html)
* [Getting Started with Erlang User's Guide](http://www.erlang.org/doc/getting_started/users_guide.html)
* [Learn You Some Erlang](http://learnyousomeerlang.com/content)

#### OTP ####

* [gen_server behaviour](http://erlang.org/doc/design_principles/gen_server_concepts.html)
* [supervisor behaviour](http://erlang.org/doc/design_principles/sup_princ.html)
* [application behaviour](http://erlang.org/doc/design_principles/applications.html)

### OpenFlow ###

* http://flowgrammable.org/
* [OpenFlow Specifications](https://www.opennetworking.org/sdn-resources/technical-library)
  * we're interested in 1.3.x

## Architecture ##

![alt](img/arch.png)

### xmpp_ofc_gen_switch

#### API

```erlang

%% starts the process
start_link() -> {ok, pid()} 

%% called when a new switch with given DatapathId connects
open_connection(DatapathId) -> ok

%% called when a switch with given DatapathId disconnects
terminate_connection(DatapathId) -> ok.

%% called when there's a message from a switch that that some module subscribed to
handle_message(DatapathId, Msg) -> ok.
```

#### Functionalities

* Starts the modules that will implement the logic
* Keeps the runtime configuration for the switches (enabled modules, pid's under witch they run, subscriptions)
* References the LOOM libraries for handling OpenFlow messages
```erlang
subscribe(DatapathId, MsgTypes) ->
    lists:foreach(
      fun(MsgType) ->
              ofs_handler:subscribe(DatapathId,xmpp_ofc_ofsh, MsgType) 
      end, MsgTypes).

....

unsubscribe(DatapathId, MsgTypes) ->
    lists:foreach(
      fun(MsgType) ->
              case ofs_handler:unsubscribe(DatapathId, xmpp_ofc_ofsh,
                                           MsgType) of
                  ok ->
                      ok;
                  no_handler ->
                      ok
              end
              end, MsgTypes).

....
of_send(DatapathId, Messages) ->
    exometer:update(?EXO_OF_MESSAGES_SENT, 1),
    [ofs_handler:send(DatapathId, M) || M <- Messages].
```

* When a message comes from a switch it passes it to the enabled moduels based on the subcriptions:

```erlang
handle_message(MsgType, Msg, ModsConfig) ->
    exometer:update(?EXO_OF_MESSAGES_RECEIVED, 1),
    lists:foldl(fun({Mod, Pid, MsgTypes}, Acc) ->
                        case lists:member(MsgType, MsgTypes) of
                            true ->
                                Mod:handle_message(Pid, Msg, Acc);
                            false ->
                                Acc
                        end
                end, [], ModsConfig).

```

* updates metrics that are sent to graphite


### Controller Modules

#### API

```erlang
%% Starts the module and returns its pid, subscriptions, and initial OpenFlow messages that are to be sent to the contoller
start_link(DatapathId) ->  {ok, Pid, subscriptions(), init_of_messages()}

%% Stops the module
stop(Pid) -> ok

%% Called when a meesage that this module have subscribed to is received by the controller
%% Pid is the module Pid, the Msg is the message that was delivered and the OFMessages is a list of all the OpenFlow messages that are going to be sent to the controller and that were returned from the other modules.
%% This functions is epxected to return the updated list of the OpenFlow messages.
-spec handle_message(pid(),
                     {MsgType :: term(),
                      Xid :: term(),
                      MsgBody :: [tuple()]},
                     [ofp_message()]) -> [ofp_message()].
handle_message(Pid, Msg, OFMessages) -> [UpdatedOFMessage]

```

#### Description

* A controller module provides a set of functionality to be applied on the network
* It subscribtes to the messages from the switch (like packet-in)
* When a messages that it has subscribed to is recived by the controller it is passed to the Module's handle_message/4 callback which in turn adds OpenFlow messages to be sent down to the switch
* `xmpp_ofc_l2_switch` is a module implementing basic MAC-learning switch functionality
   * it subscribes to packet-in messages
   * on receiving such message it builds up a forwarding table and send appropriate Flow-Mods down to the switch
```erlang
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

```
