%%%-------------------------------------------------------------------
%% @doc xc top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(xmpp_ofc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 2,
                 period => 10},
    Child = #{id => xmpp_ofc_gen_switch,
              start => {xmpp_ofc_gen_switch, start_link, []},
              shutdown => 5000,
              restart => permanent,
              type => worker,
              modules => [xmpp_ofc_gen_switch]},
    {ok, {SupFlags, [Child]}}.

%%====================================================================
%% Internal functions
%%====================================================================
