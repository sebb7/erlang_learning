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

%%====================================================================
%% API functions
%%====================================================================

start_link(InitialValue) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, InitialValue).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(InitialValue) ->
    io:format("~p (~p) starting... ~n", [{local, ?MODULE}, self()]),
    RestartStrategy = one_for_one,
    MaxRestarts = 3,
    MaxSecondsBetween = 5,
    Flags = #{strategy => RestartStrategy, intensity => MaxRestarts,
              period => MaxSecondsBetween},
    ChildIdS = counterId,
    ChildIdR = reserveId,
    StartFuncServer = {counter_server, start_link, [InitialValue]},
    StartFuncReserve = {counter_reserve, start_link, []},
    Restart =  permanent,
    Shutdown = infinity,
    Type = worker,
    ModuleServer = [counter_server],
    ModuleReserve = [counter_reserve],
    ChildSpecification = [#{id => ChildIdR, start => StartFuncReserve,
                            restart => Restart, shutdown => Shutdown,
                            type => Type, modules => ModuleReserve},
                          #{id => ChildIdS, start => StartFuncServer,
                            restart => Restart, shutdown => Shutdown,
                            type => Type, modules => ModuleServer}],
    {ok, {Flags, ChildSpecification}}.

%%====================================================================
%% Internal functions
%%====================================================================

