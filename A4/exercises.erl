-module(exercises).

-export([move/2, ignore_invalid/2]).


move(north, {X, Y}) -> {X, Y + 1};
move(south, {_, Y}) when Y =< 0 ->
    throw(invalid_move);
move(south, {X, Y}) -> {X, Y - 1};


move(east, {X, Y}) -> {X + 1, Y};
move(west, {X, _}) when X =< 0 ->
    throw(invalid_move);
move(west, {X, Y}) -> {X - 1, Y}.

ignore_invalid(Dir, Pos) ->
    try move(Dir, Pos)
    catch
        invalid_move -> Pos
    end.