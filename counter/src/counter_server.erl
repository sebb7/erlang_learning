%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(counter_server).

-behaviour(gen_server).

%% API
-export([start_link/1, increment/1, decrement/1, get_value/0, reset/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {value, reset}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(N) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, N, []).

increment(N) ->
    gen_server:cast(?MODULE, {increment, N}).

decrement(N) ->
    gen_server:cast(?MODULE, {decrement, N}).

get_value() ->
    gen_server:call(?MODULE, get_value).

reset() ->
    gen_server:call(?MODULE, reset).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(N) ->
    process_flag(trap_exit, true),
    {OldState, ResetValue} = counter_reserve:get_backup(),
    if
        OldState =:= undefined ->
            State = #state{value = N, reset = N};
        true ->
            State = #state{value = OldState, reset = ResetValue}
    end,
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_value, _From, State = #state{value = Val}) ->
    {reply, Val, State};
handle_call(reset, _From, State = #state{reset = Res}) ->
    NewState = State#state{value = Res},
    {reply, NewState#state.value, NewState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State
%% @end
%%--------------------------------------------------------------------
handle_cast({increment, N}, State = #state{value = Val}) ->
    NewVal = Val + N,
    NewState = State#state{value = NewVal},
    {noreply, NewState};
handle_cast({decrement, N}, State = #state{value = Val}) ->
    NewVal = Val -N,
    NewState = State#state{value = NewVal},
    {noreply, NewState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    counter_reserve:save({State#state.value, State#state.reset}),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

