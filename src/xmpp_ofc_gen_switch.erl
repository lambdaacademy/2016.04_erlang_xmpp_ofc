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
-define(DEFAULT_MODULES, [xmpp_ofc_l2_switch]).


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
    {noeply, State};

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

open_connection(DatapathId, EnabledMods) ->
    {ModsCfg, MsgTypesToSubscribe} =
        lists:foldl(
          fun(Mod, {ModsCfgAcc, MsgTypesAcc}) ->
                  {ok, Pid, MsgTypes} = Mod:start_link(DatapathId),
                  {
                    [{Mod, Pid, MsgTypes} | ModsCfgAcc],
                    [MsgTypes | MsgTypesAcc]
                  }
          end, {[], []}, EnabledMods),
    subscribe(DatapathId, lists:usort(MsgTypesToSubscribe)),
    lists:reverse(ModsCfg).

terminate_connection(DatapathId, ModsConfig) ->
    MsgTypesToUnsubscribe =
        lists:foldl(fun({Mod, Pid, MsgTypes}, Acc) ->
                            ok = Mod:stop(Pid),
                            [MsgTypes | Acc]
                    end, ModsConfig),
    unsubscribe(DatapathId, lists:usort(MsgTypesToUnsubscribe)).

handle_message(MsgType, Msg, ModsConfig) ->
    lists:foldl(fun({Mod, Pid, MsgTypes}, Acc) ->
                        case lists:member(MsgType, MsgTypes) of
                            true ->
                                Mod:handle_messsag(Pid, Msg,  Acc);
                            false ->
                                Acc
                        end
                end, [], ModsConfig).

subscribe(DatapathId, MsgTypes) ->
    lists:foreach(fun(MsgType) ->
                          ok = ofs_handler:subscribe(DatapathId,
                                                     xmpp_ofs_ofsh,
                                                     MsgType)
                  end, MsgTypes).

unsubscribe(DatapathId, MsgTypes) ->
    lists:foreach(fun(MsgType) ->
                          ok = ofs_handler:unsubscribe(DatapathId,
                                                       xmpp_ofs_ofsh,
                                                       MsgType)
                  end, MsgTypes).

of_send(DatapathId, Messages) ->
    [ofs_handler:send(DatapathId, M) || M <- Messages].
