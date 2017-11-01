%%%-------------------------------------------------------------------
%% @doc counter top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(counter_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(InitialValue) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, InitialValue).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(InitialValue) ->
    io:format("~p (~p) starting... ~n", [{local, ?SERVER}, self()]),
    RestartStrategy = one_for_one,
    MaxRestarts = 3,
    MaxSecondsBetween = 5,
    Flags = #{strategy => RestartStrategy, intensity => MaxRestarts,
              period => MaxSecondsBetween},
    ChildId = counterId,
    StartFunc = {counter_server, start_link, [InitialValue]},
    Restart =  permanent,
    Shutdown = infinity,
    Type = worker,
    Module = [counter_server],
    ChildSpecification = [#{id => ChildId, start => StartFunc,
                            restart => Restart, shutdown => Shutdown,
                            type => Type, modules => Module}],
    {ok, {Flags, ChildSpecification}}.

%%====================================================================
%% Internal functions
%%====================================================================

