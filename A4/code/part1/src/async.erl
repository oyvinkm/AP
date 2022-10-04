-module(async).

-export([new/2, wait/1, poll/1]).

% Worker processs
new(Fun, Arg) -> Self = self(),
				 spawn(fun() -> Self ! Fun(Arg) end).

poll(Aid) -> nope.
% 	receive
% 		Aid -> {exception, Ex}

% 	end.

wait(Aid) -> 
	receive
		X -> X
	end.

