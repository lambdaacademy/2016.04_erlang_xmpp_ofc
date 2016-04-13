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
