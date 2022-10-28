-module(rps).

%-export([start/0, queue_up/3, move/2, statistics/1, drain/3, host_game/2]).

-export([init/1, handle_cast/2, handle_call/3]).
-behaviour(gen_server).

-compile(nowarn_export_all).
-compile(export_all).



-record(state, {longest, queue, ongoing}).
-record(coord, {player1, player2, rounds}).

start() -> gen_server:start(?MODULE, [], []).

queue_up(BRef, Name, Rounds) -> gen_server:call(BRef, {queue_up, Name , Rounds}).

move(_, _) -> nope.


statistics(_) -> nope.

drain(_, _, _) ->  nope.

init([]) -> 
    {ok, #state{longest = 0, queue = [], ongoing = []}};

init({Player1, Player2, Rounds}) -> 
    {ok, #coord{player1 = {Player1, 0}, player2 = {Player2, 0},  rounds = Rounds}}.

handle_call({queue_up, Name, Rounds}, _From, State) -> 
    P = {Name, Rounds},
    case find_match(Rounds, State#state.queue) of
        {Oponent, Rounds} -> {_, Cid} = coordinator(Name, Oponent, Rounds),
                             {reply, {ok, Oponent, Cid}, State};
        false -> NewState = State#state{queue = State#state.queue ++ [P]},
                            {reply, ok, NewState}
    end.
    

handle_cast(_,_) -> nope.

% join game -> lists.keyfind(Rounds = Rounds) -> spawn coordinator.
% stand in queue -> queue.append({Name, Rounds})
% error(rounds < 1) -> if rounds < 1: error

-spec find_match(Rounds, [{Name, Rounds}]) -> {Name, Rounds} | false.
find_match(Rounds, Queue) ->
    case lists:keyfind(Rounds, 2, Queue) of
        Player -> Player;
        false -> false
    end.

coordinator(Player1, Player2, Rounds) -> 
    gen_server:start_link(?MODULE, {Player1, Player2, Rounds}, []).




