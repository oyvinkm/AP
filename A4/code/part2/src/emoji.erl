-module(emoji).

-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
         analytics/5, get_analytics/2, remove_analytics/3,
         stop/1, loop/1, hit/2, accessed/2, eval_fun/2, map/2]).


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
        true -> Init = {Initial, []}, 
            E = spawn(fun() -> loop(Init) end),
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

alias(E, Short1, Short2) -> request_reply({alias, Short1, Short2}, E).

delete(E, Short) -> request_reply({delete, Short}, E).

lookup(E, Short) -> request_reply({lookup, Short}, E).

analytics(E, Short, Fun, Label, Init) -> 
                    request_reply({analytics, Short, Fun, Label, Init}, E).


get_analytics(E, Short) -> request_reply({get_analytics, Short}, E).

remove_analytics(E, Short, Label) -> request_reply({remove_analytics, Short, Label}, E).

stop(E) -> request_reply(stop, E).

map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].

eval_fun(Emo, {Label, F, State}) -> 
    {Label, F, F(Emo,State)}.

%[{Short , Emo, [{Label, Fun, State}]}]


loop(Lst_of_pairs) -> 
    receive
        {{new_shortcode, Short, Emo}, From} ->
            case lists:member(Short, [Ele || {Ele, _, _} <- Lst_of_pairs]) of
                true -> From ! {{error, "Short already exists"}},
                        loop(Lst_of_pairs);
                false -> New_lst = Lst_of_pairs ++ [{Short, Emo, []}],
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
                {Short, Emo, Analytics} -> Anal = [eval_fun(Emo, X) || X <- Analytics],
                                           Temp_Lst = lists:keydelete(Short, 1, Lst_of_pairs),
                                           New_Lst = Temp_Lst ++ [{Short, Emo, Anal}],
                                           %Should not return Anal.
                                           From ! {{ok, Emo}},
                                           loop(New_Lst)
            end;
        % What kind of error could occur, 
        % is it correct
        {stop, From} ->
            E = self(),
            spawn(fun() -> 
                try exit(E, normal) of
                    _ -> From ! {ok}
                catch
                    _ : _ -> From ! {{error, "did not work"}}
                end
            end);
        {{analytics, Short, Fun, Label, Init}, From} ->
            case lists:keyfind(Short, 1, Lst_of_pairs) of
                false -> From ! {{error, "Short does not exist"}};
                {Short, Emo, Analytics} -> case lists:member(Label, [Ele || {Ele, _, _} <- Analytics]) of
                                                true -> From ! {{error, "Label already exists."}};
                                                false -> Anal = Analytics ++ [{Label, Fun, Init}],
                                                         From ! {ok},
                                                         Temp_Lst = lists:keydelete(Short, 1, Lst_of_pairs),
                                                         New_Lst = Temp_Lst ++ [{Short, Emo, Anal}],
                                                         loop(New_Lst)                            
                                           end
            end;
        {{get_analytics, Short}, From} -> 
            case lists:keyfind(Short, 1, Lst_of_pairs) of
                false -> From ! ({{error, "Short does not exist."}});
                {_, _, Analytics} ->  Anal = [{Label, Stats} || {Label,_,Stats} <- Analytics],
                                            From ! {{ok, Anal}},
                                            loop(Lst_of_pairs)
            end;
        {{remove_analytics, Short, Label}, From} ->
            Me = self(),
            spawn(fun() -> 
                case lists:keyfind(Short, 1, Lst_of_pairs) of
                    false -> From ! ({{error, "Short does not exist."}});
                    {Short, Emo, Analytics} -> Anal = lists:keydelete(Label, 1, Analytics),
                    Temp_Lst = lists:keydelete(Short, 1, Lst_of_pairs),
                    New_Lst = Temp_Lst ++ [{Short, Emo, Anal}], 
                    Me ! {New_Lst}
                end
            end),
            receive
                {Lst} ->                                    
                    From ! {{ok}},
                    loop(Lst)
                end;
        {{alias, Short1, Short2}, From} -> 
                case lists:keymember(Short1, 1, element(1, Lst_of_pairs)) of
                    false -> From ! {{error, "Short1 does not exists."}};
                    true -> case lists:keymember(Short2, 1, element(2, Lst_of_pairs)) of
                                true -> From ! {{error, "Alias already exists."}};
                                false -> NewLst =  {element(1, Lst_of_pairs), element(2, Lst_of_pairs) ++ [{Short2, Short1}]},
                                        From ! {{ok, NewLst}},
                                        loop(NewLst)
                            end
                end 
    end.

% c(emoji).
% {ok, E} = emoji:start([]).
% emoji:new_shortcode(E, "smiley", <<240,159,152,131>>).
% emoji:lookup(E, "smiley").
% emoji:delete(E, "smiley").
% emoji:lookup(E, "smiley").
% F = fun(X,Y) -> element(2, X) =/= Y end.
% [L || L <- Ls, F(L, A)]
