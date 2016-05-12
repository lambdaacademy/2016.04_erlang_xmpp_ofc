-module(xmpp_ofc_gen_switch).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
         open_connection/1,
         terminate_connection/1,
         handle_message/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Includes & Type Definitions & Macros
%% ------------------------------------------------------------------

-type mod_config() :: {Mod :: atom(),
                       Pid :: pid(),
                       [MsgType :: atom()]}.
-type switches_config() ::
        #{DatapathId :: binary() => ModsCfg :: [mod_config()]}.
-record(state, {enabled_modules :: [Mod :: atom()],
                switches_config :: switches_config()}).
-type state() :: #state{}.

-define(SERVER, ?MODULE).
-define(DEFAULT_MODULES, [xmpp_ofc_l2_switch, xmpp_ofc_ids_switch]).
-define(EXO_SWITCHES_CNT, [counters, switches]).
-define(EXO_OF_MESSAGES_RECEIVED, [counters, of_messages_received]).
-define(EXO_OF_MESSAGES_SENT, [counters, of_messages_sent]).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec open_connection(binary()) -> ok.
open_connection(DatapathId) ->
    gen_server:cast(?MODULE, {open_connection, DatapathId}).

-spec terminate_connection(binary()) -> ok | {error, term()}.
terminate_connection(DatapathId) -> 
    gen_server:call(?MODULE, {terminate_connection, DatapathId}).

-spec handle_message(binary(), {MsgType :: term(),
                                Xid :: term(),
                                MsgBody :: [tuple()]}) -> ok.
handle_message(DatapathId, Msg) ->
    gen_server:cast(?MODULE, {handle_message, DatapathId, Msg}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    init_exometer(),
    {ok, #state{enabled_modules = ?DEFAULT_MODULES,
                switches_config = #{}}}.



handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({open_connection, DatapathId}, State) ->
    ModsCfg = open_connection(DatapathId, State#state.enabled_modules),
    SwitchesCfg0 = State#state.switches_config,
    SwitchesCfg1 = SwitchesCfg0#{DatapathId => ModsCfg},
    lager:info("Opened connection to ~p", [DatapathId]),
    lager:debug("Config for switch ~p: ~p", [DatapathId, ModsCfg]),
    {noreply, State#state{switches_config = SwitchesCfg1}};

handle_cast({terminate_connection, DatapathId},
            State = #state{switches_config = SwitchesCfg}) ->
    terminate_connection(DatapathId,
                         maps:get(DatapathId, SwitchesCfg)),
    lager:info("Terminated connection with ~p", [DatapathId]),
    {noreply, State#state{ switches_config =
                               maps:remove(DatapathId, SwitchesCfg)}};

handle_cast({handle_message, DatapathId, {MsgType, _,  _} = Msg},
            State) ->
    ModsCfg = maps:get(DatapathId, State#state.switches_config),
    OFMessages = handle_message(MsgType, Msg, ModsCfg),
    lager:debug("Switch ~p handled message: ~p", [DatapathId, Msg]),
    of_send(DatapathId, OFMessages),
    lager:debug("Switch ~p sent OF messages: ~p", [DatapathId,
                                                   OFMessages]),
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(_Request, State) ->
    {noreply, State}.


terminate(_Reason, #state{switches_config = SwitchesCfg}) ->
    [begin
         terminate_connection(Dpid, maps:get(Dpid, SwitchesCfg))
     end || Dpid <- maps:keys(SwitchesCfg)].


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

init_exometer() ->
    Metrics =
        [{?EXO_SWITCHES_CNT, counter, [value], 10000},
         {?EXO_OF_MESSAGES_RECEIVED, spiral, [one, count], 10000},
         {?EXO_OF_MESSAGES_SENT, spiral, [one, count], 10000}],
    %% spiral: sum of values reported withing given time span
    [ok = init_exometer_graphite_metrics(Name,
                                         Type,
                                         DataPoints,
                                         ReportInterval)
     || {Name, Type, DataPoints, ReportInterval} <- Metrics].

init_exometer_graphite_metrics(
  Name, Type, DataPoints, ReportInterval) ->
    exometer:new(Name, Type),
    exometer_report:subscribe(exometer_report_graphite,
                              Name, DataPoints, ReportInterval).

open_connection(DatapathId, EnabledMods) ->
    exometer:update(?EXO_SWITCHES_CNT, 1),
    {ModsCfg, MsgTypesToSubscribe, InitOFMessages} =
        lists:foldl(
          fun(Mod, {ModsCfgAcc, MsgTypesAcc, OFMessagesAcc}) ->
                  {ok, Pid, MsgTypes, InitOFMessages} =
                      Mod:start_link(DatapathId),
                  {
                    [{Mod, Pid, MsgTypes} | ModsCfgAcc],
                    MsgTypes ++ MsgTypesAcc,
                    InitOFMessages ++ OFMessagesAcc
                  }
          end, {[], [], []}, EnabledMods),
    subscribe(DatapathId, lists:usort(MsgTypesToSubscribe)),
    of_send(DatapathId, InitOFMessages),
    lists:reverse(ModsCfg).

terminate_connection(DatapathId, ModsConfig) ->
    exometer:update(?EXO_SWITCHES_CNT, -1),
    MsgTypesToUnsubscribe =
        lists:foldl(fun({Mod, Pid, MsgTypes}, Acc) ->
                            ok = Mod:stop(Pid),
                            MsgTypes ++ Acc
                    end, [], ModsConfig),
    unsubscribe(DatapathId, lists:usort(MsgTypesToUnsubscribe)).

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

subscribe(DatapathId, MsgTypes) ->
    lists:foreach(
      fun(MsgType) ->
              ofs_handler:subscribe(DatapathId,xmpp_ofc_ofsh, MsgType) 
      end, MsgTypes).

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

of_send(DatapathId, Messages) ->
    exometer:update(?EXO_OF_MESSAGES_SENT, 1),
    [ofs_handler:send(DatapathId, M) || M <- Messages].
