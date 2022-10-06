-module(emoji).

-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
         analytics/5, get_analytics/2, remove_analytics/3,
         stop/1, loop/1, hit/2, accessed/2]).

-type shortcode() :: string().
-type emoji() :: binary().
-type analytic_fun(State) :: fun((shortcode(), State) -> State).

% Lists of pairs (string, binary) e.g. [("smiley", <<240,159,152,131>>)]

hit(_, N) -> N+1.

accessed(SC, TS) ->
    Now = calendar:local_time(),
    [{SC,Now} | TS].

start(Initial) -> 
    case length(Initial) == length(lists:ukeysort(1, Initial)) of
        true -> E = spawn(fun() -> loop(Initial) end),
                  {ok, E};
        false -> {error, reasons}
    end.


request_reply(Request, Pid) ->
    Pid ! {Request, self()},
    receive
        {Res} -> Res

    end.

% append the emoji to the list in E.
new_shortcode(E, Short, Emo) -> request_reply({new_shortcode, Short, Emo}, E).

% yeet(E) -> request_reply(yeet, E).

alias(_, _, _) -> not_implemented.

delete(E, Short) -> request_reply({delete, Short}, E).

lookup(E, Short) -> request_reply({lookup, Short}, E).

analytics(E, Short, Fun, Label, Init) -> 
                    request_reply({analytics, Short, Fun, Label, Init}, E)

get_analytics(_, _) -> not_implemented.

remove_analytics(_, _, _) -> not_implemented.

stop(E) -> request_reply(stop, E).

loop(Lst_of_pairs) -> 
    receive
        {{new_shortcode, Short, Emo}, From} ->
            case lists:member(Short, [Ele || {Ele, _} <- Lst_of_pairs]) of
                true -> From ! {{error, "Short already exists"}},
                        loop(Lst_of_pairs);
                false -> New_lst = Lst_of_pairs ++ [{Short, Emo}],
                         From ! {ok},
                         loop(New_lst)
            end;
        {{delete, Short}, From} ->
            New_lst = lists:keydelete(Short, 1, Lst_of_pairs),
            From ! {ok},
            loop(New_lst);
        {{lookup, Short}, From} ->
            case lists:keyfind(Short, 1, Lst_of_pairs) of
                false -> 
                    From ! {no_emoji},
                    loop(Lst_of_pairs);
                {Short, Emo} -> 
                    From ! {{ok, Emo}},
                    loop(Lst_of_pairs)
            end;
        % What kind of error could occur, 
        % is it correct
        {stop, From} ->
            E = self(),
            spawn(fun() -> 
                try exit(E, normal) of
                    _ -> From ! {ok}
                catch
                    _ : Ex -> From ! {{error, "did not work"}}
                end
            end);
        {{analytics, Short, Fun, Label, Init}, E} ->
            E = self(),
            Linked = spawn_link(fun() -> )

    end.

% c(emoji).
% {ok, E} = emoji:start([]).
% emoji:new_shortcode(E, "smiley", <<240,159,152,131>>).
% emoji:lookup(E, "smiley").
% emoji:delete(E, "smiley").
% emoji:lookup(E, "smiley").
