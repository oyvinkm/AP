-module(async).

-export([new/2, wait/1, poll/1, wait_catch/1, wait_any/1]).

% gen_server behaviour
-export([init/1, handle_cast/2, handle_call/3]).

-behaviour(gen_server).

new(Fun, Arg) -> gen_server:start(?MODULE, {Fun, Arg}, []).
wait(Aid) -> gen_server:call(Aid, wait).
poll(Aid) -> gen_server:cast(Aid, poll).

init({Fun, Arg}) ->
	{ok, Fun(Arg)}.

wait_catch(Aid) -> nope.
wait_any(Aids) -> nope.

handle_cast(_, _) -> nope.

handle_call(wait, _From, State) ->
	{reply, {ok, State}, State};
	% _From ! {reply, State}.

handle_call(poll, _From, State) ->
	case State of
		nothing -> {reply, nothing};
		_ : Ex -> {exception, Ex};
		Res -> {ok, Res}
	end.

