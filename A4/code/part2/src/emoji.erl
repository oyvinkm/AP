-module(emoji).

-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
         analytics/5, get_analytics/2, remove_analytics/3,
         stop/1, loop/1, yeet/1]).

-type shortcode() :: string().
-type emoji() :: binary().
-type analytic_fun(State) :: fun((shortcode(), State) -> State).

% Lists of pairs (string, binary) e.g. [("smiley", <<240,159,152,131>>)]
start(Initial) -> 
    case length(Initial) == length(lists:ukeysort(1, Initial)) of
        true -> spawn(fun() -> loop(Initial) end),
                  {ok, self()};
        false -> {error, reasons}
    end.


request_reply(Request, Pid) ->
    Pid ! {self(), Request},
    receive
        {Res} -> Res
        % {error, reason} -> {error};
        % {ok} -> {ok}
    end.

% append the emoji to the list in E.
new_shortcode(E, Short, Emo) -> request_reply({new_shortcode, Short, Emo}, E).

yeet(E) -> request_reply(yeet, E).

alias(_, _, _) -> not_implemented.

delete(_, _) -> not_implemented.

lookup(_, _) -> not_implemented.

analytics(_, _, _, _, _) -> not_implemented.

get_analytics(_, _) -> not_implemented.

remove_analytics(_, _, _) -> not_implemented.

stop(_) -> not_implemented.

loop(Lst_of_pairs) -> 
    receive
        {From, {new_shortcode, Short, Emo}} ->
            case lists:member(Short, [Ele || {Ele, _} <- Lst_of_pairs]) of
                true -> From ! {error},
                        loop(Lst_of_pairs);
                false -> New_lst = Lst_of_pairs ++ [{Short, Emo}],
                         From ! {ok},
                         loop(New_lst)
            end;
        {From, yeet} ->
            From ! {"notok"},
            loop(Lst_of_pairs)
    end.

% emoji:start([("smiley", <<240,159,152,131>>)]).