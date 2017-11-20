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
    Flags = #{strategy => one_for_one, intensity => 3, period => 5},
    ChildSpecification = [get_reserve_spec(), get_counter_spec(InitialValue)],
    {ok, {Flags, ChildSpecification}}.

%%====================================================================
%% Internal functions
%%====================================================================

get_counter_spec(InitialValue) ->
    get_child_spec(counterId, {counter_server, start_link, [InitialValue]},
                   permanent, infinity, worker, [counter_server]).

get_reserve_spec() ->
    get_child_spec(reserveId, {counter_reserve, start_link, []},
                   permanent, infinity, worker, [counter_reserve]).

get_child_spec(Id, StartFunc, Restart, Shutdown, Type, Module) ->
    #{id => Id, start => StartFunc,
      restart => Restart, shutdown => Shutdown,
      type => Type, modules => Module}.

