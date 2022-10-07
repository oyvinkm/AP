-module(counter).

-export([start/0, incr/1, decr_with/2, get_value/1, get_slow/1, modify/2]).

start() -> spawn(fun () -> loop(0) end).

incr(Cid) -> request_reply(Cid, incr).

decr_with(Cid, N) -> request_reply(Cid, {decr_with, N}).

get_value(Cid) -> request_reply(Cid, get_value).

get_slow(Cid) -> request_reply(Cid, get_slow).

modify(Cid, Fun) -> request_reply(Cid, {modify, Fun}).

request_reply(Pid, Request) ->
    Ref = make_ref(),
    Pid ! {self(), Ref, Request},
    receive
        {Ref, Response} -> Response
    end.

loop(Count) ->
    receive
      {From, Ref, incr} ->
        {NewState, Res} = {Count + 1, ok},
        From ! {Ref, Res},
        loop(NewState);
      {From, Ref, {decr_with, N}} ->
        {NewState, Res} = {Count - N, ok},
        From ! {Ref, Res},
        loop(NewState);
      {From, Ref, get_value} ->
        {NewState, Res} = {Count, {ok, Count}},
        From ! {Ref, Res},
        loop(NewState);
      {From, Ref, get_slow} ->
        spawn(fun() ->
                timer:sleep(2000),
                From ! {Ref, Count}
              end),
        NewState = Count,
        loop(NewState);
      {From, Ref, {modify, Fun}} ->
        Me = self(),
        process_flag(trap_exit, true),
        Worker = spawn_link(fun() ->
                              FunResult = Fun(Count),
                              Me ! {self(), FunResult}
                            end),
        receive
            {'EXIT', Worker, Reason} ->
                {NewState, Res} = {Count, {error, Reason}},
                From ! {Ref, Res},
                loop(NewState);
            {Worker, Result} ->
                {NewState, Res} = {Result, ok},
                From ! {Ref, Res},
                loop(NewState)
        end
    end.
