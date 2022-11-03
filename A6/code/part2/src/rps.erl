-module(rps).

%-export([start/0, queue_up/3, move/2, statistics/1, drain/3, host_game/2]).

-export([init/1, handle_cast/2, handle_call/3]).
-behaviour(gen_server).

-compile(nowarn_export_all).
-compile(export_all).

% -type choice() :: rock | paper | scissor.

% call = synchrounous, cast = asynchronous
-record(state, {longest, queue, ongoing}).
-record(coord, {name, oponent, rounds, duration, bref, draining}).

start() -> gen_server:start(?MODULE, [], []).

queue_up(BRef, Name, Rounds) -> gen_server:call(BRef, {queue_up, Name , Rounds}).

move(Cid, Choice) -> gen_server:call(Cid, {move, Choice}) .


statistics(BRef) -> gen_server:call(BRef, statistics).

drain(BRef, Pid, Msg) ->  gen_server:cast(BRef, {drain, Pid, Msg}).

init([]) -> 
    {ok, #state{longest = 0, queue = [], ongoing = []}};

init({Name, Oponent, Rounds, BREF}) -> 
    {ok, #coord{name = {Name, 0}, oponent = {Oponent, 0}, rounds = Rounds, duration = 0, bref = BREF, draining = false}}.

handle_call({queue_up, Name, Rounds}, _From, State) -> 
    P = {Name, Rounds},
    case find_match(Rounds, State#state.queue) of
        {Oponent, Rounds} -> {_, Cid} = coordinator(Name, Oponent, Rounds, self()),
                             
                             NewState = State#state{queue = lists:keydelete(Rounds, 2, State#state.queue), ongoing = State#state.ongoing ++ [Cid]},
                             {reply, {ok, Oponent, Cid}, NewState};
                             %MISSING: Remove players from queue
        false -> NewState = State#state{queue = State#state.queue ++ [P]},
                            {reply, ok, NewState}
    end;

handle_call(statistics, _, State) ->
    LongestGame = State#state.longest,
    InQueue = length(State#state.queue),
    %On going is a list of our Coord ID
    OnGoing = length(State#state.ongoing),
    {reply, {ok, LongestGame, InQueue, OnGoing}, State};

handle_call({move, Choice}, _From, State) ->
    Is_Draining = State#coord.draining,
    if 
    Is_Draining ->
        {reply, server_stopping, State};
    true ->
        OpChoice = rock, % random
        Rounds = State#coord.rounds,
        if 
        OpChoice =:= Choice -> 
            NewState = State#coord{duration = State#coord.duration + 1},
            {reply, tie, NewState}; 
        true ->
            Res = result(Choice, OpChoice),
            case Res of
                win -> 
                    OpScore = element(2, State#coord.oponent),
                    MyName = element(1, State#coord.name),
                    MyScore = element(2, State#coord.name) + 1,
                    NewState = State#coord{name = {MyName, MyScore}, duration = State#coord.duration + 1},
                    Bool = game_over(MyScore, OpScore, Rounds),
                    if 
                    Bool ->
                        gen_server:cast(NewState#coord.bref, {game_over, NewState#coord.duration}),
                        % Destroy cid
                        {reply, {game_over, MyScore, OpScore}, NewState};
                    true ->
                        {reply, {Res}, NewState}
                    end;

                {loss, _} -> 
                    OpScore = element(2, State#coord.oponent) + 1,
                    OpName = element(1, State#coord.oponent),
                    MyScore = element(2, State#coord.name),
                    NewState = State#coord{oponent = {OpName, OpScore}, duration = State#coord.duration + 1},
                    Bool = game_over(MyScore, OpScore, Rounds),
                    if
                    Bool ->
                        gen_server:cast(NewState#coord.bref, {game_over, NewState#coord.duration}),
                        % Destroy cid
                        {reply, {game_over, MyScore, OpScore}, NewState};
                    true ->
                        {reply, Res, NewState}
                    end
            end
        end
    end.

handle_cast(draining, State) ->
    io:format("This is our state: ~p", [State]),
    NewState = State#coord{draining = true},
    {noreply, NewState};

handle_cast({drain, Pid, Msg}, State) ->
    [spawn(fun()-> gen_server:cast(Cid, draining) end) || Cid <- State#state.ongoing],
    % Pid ! Msg,
    {noreply, ok};
    % We can create a list of all our coordinators
    % And do gen_server:cast to all of them 

handle_cast({game_over, Duration}, State) ->
    % io:format("State = ~p", [State]),
    if 
    State#state.longest < Duration ->
        NewState = State#state{longest = Duration, ongoing = State#state.ongoing - 1},
        % io:format("NewState = ~p \n", [NewState]),
        {noreply, NewState};
    true ->
        NewState = State#state{ongoing = State#state.ongoing - 1},
        % io:format("NewState = ~p \n", [NewState]),
        {noreply, NewState}
    end.


game_over(MyScore, OpScore, Rounds) ->
    Half = Rounds div 2,
    if
    (MyScore > Half) or (OpScore > Half) ->
        true;
    true -> 
        false
    end.


% join game -> lists.keyfind(Rounds = Rounds) -> spawn coordinator.
% stand in queue -> queue.append({Name, Rounds})
% error(rounds < 1) -> if rounds < 1: error

-spec find_match(Rounds, [{Name, Rounds}]) -> {Name, Rounds} | false.
find_match(Rounds, Queue) ->
    case lists:keyfind(Rounds, 2, Queue) of
        {Player, Rounds} -> {Player, Rounds};
        false -> false
    end.

-spec result(_, OpChoice) -> win | {loss, OpChoice} | tie.
result(MyChoice, OpChoice) ->
    case MyChoice of
        rock -> case OpChoice of
            paper -> {loss, paper};
            scissor -> win;
            _ -> win
        end;

        paper -> case OpChoice of
            scissor -> {loss, scissor};
            rock -> win;
            _ -> win
        end;

        scissor -> case OpChoice of
            rock -> {loss, rock};
            paper -> win;
            _ -> win
        end;

        _ -> case OpChoice of
            rock -> {loss, OpChoice};
            paper -> {loss, OpChoice};
            scissor -> {loss, OpChoice};
            _ -> tie
        end
    end.



coordinator(Name, Oponent, Rounds, BREF) -> 
    gen_server:start_link(?MODULE, {Name, Oponent, Rounds, BREF}, []).




