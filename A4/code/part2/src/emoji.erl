-module(emoji).

% -export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
%          analytics/5, get_analytics/2, remove_analytics/3,
%          stop/1, loop/1, hit/2, accessed/2, eval_fun/2, map/2]).

-compile(export_all).

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
        false -> {error, reasons};
        true -> case length(Initial) > 0 of
                    true -> Temp = [{Short, Emo, []} || {Short, Emo} <- Initial],
                            Init = {Temp, []}, 
                            E = spawn(fun() -> loop(Init) end),
                            {ok, E};
                    false -> Init = {Initial, []}, 
                             E = spawn(fun() -> loop(Init) end),
                             {ok, E}    
                end
    end.


request_reply(Request, Pid) ->
    Pid ! {Request, self()},
    receive
        {Res} -> Res

    end.

% append the emoji to the list in E.
new_shortcode(E, Short, Emo) -> request_reply({new_shortcode, Short, Emo}, E).

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

eval_fun(_, {}) -> [];
eval_fun(Emo, {Label, F, State}) -> 
    {Label, F, F(Emo,State)}.

%[{Short , Emo, [{Label, Fun, State}]}]
findEmo({Short, List}) ->
    case lists:keymember(Short, 1, List) of
        {Short, Emo, Analytics} -> Anal = [eval_fun(Emo, X) || X <- Analytics],
                                   Temp_Lst = lists:keydelete(Short, 1, List),
                                   NewLst = (Temp_Lst ++ [{Short, Emo, Anal}]),
                                   NewLst
    end.

filter(Pred, L) -> lists:reverse(filter(Pred, L,[])).
filter(_, [], Acc) -> Acc;
filter(Pred, [H|T], Acc) ->
    case Pred(H) of
        true -> filter(Pred, T, [H|Acc]);
        false -> filter(Pred, T, Acc)
    end.

% Third argu,emt 
remove_if_first(Pred, {First, _, _}) ->
    Pred =/= First.

remove_if_second(Pred, {_, Second}) ->
    Pred =/= Second.


loop(Pair_of_lsts) -> 
    receive
        {{new_shortcode, Short, Emo}, From} ->
            case lists:keymember(Short, 1, element(1, Pair_of_lsts)) of
            %case lists:member(Short, [Ele || {Ele, _, _} <- Pair_of_lsts]) of
                true -> From ! {{error, "Short already exists"}},
                        loop(Pair_of_lsts);
                false -> NewLst =  {element(1, Pair_of_lsts) ++ [{Short, Emo, []}], 
                        element(2, Pair_of_lsts)},
                         From ! {ok},
                         loop(NewLst)
            end;
        %Do this later
        {{delete, Short}, From} ->
            Primary = element(1, Pair_of_lsts),
            Prim_filtered = filter(fun(X) -> 
                remove_if_first(Short, X) end, Primary),

            Alias = element(2, Pair_of_lsts),
            Alias_filtered = filter(fun(X) ->
                remove_if_second(Short, X) end, Alias),

            From ! {ok},
            loop({Prim_filtered, Alias_filtered});

        {{lookup, Short}, From} ->
            % From ! {{check_input, Pair_of_lsts}},
            case lists:keyfind(Short, 1, element(1,Pair_of_lsts)) of
                {NewShort, Emo, Analytics} -> 
                    Anal = [eval_fun(Emo, X) || X <- Analytics],
                    Temp_Lst = lists:keydelete(NewShort, 1, element(1, Pair_of_lsts)),
                    NewLst = {Temp_Lst ++ [{NewShort, Emo, Anal}], 
                        element(2, Pair_of_lsts)},
                    From ! {{ok, Emo}},
                       loop(NewLst);
                false -> 
                    % looks for Short, in first element of tuple in second element of Pair_of_lsts
                    case lists:keyfind(Short, 1, element(2, Pair_of_lsts)) of
                        {_, RefShort} ->
                            E = self(), 
                            spawn(fun() -> E ! {{lookup, RefShort}, From} end),
                            loop(Pair_of_lsts);
                        false -> 
                            From ! {{error, no_emoji}},
                            loop(Pair_of_lsts)
                    end
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
        % Attaches a analytics to the label
        {{analytics, Short, Fun, Label, Init}, From} ->
            case lists:keyfind(Short, 1, element(1, Pair_of_lsts)) of
                false -> case lists:keyfind(Short, 1, element(2, Pair_of_lsts)) of
                    {_, RefShort} -> 
                        E = self(), 
                            spawn(fun() -> E ! {{analytics, RefShort, Fun, Label, Init}, From} end),
                            loop(Pair_of_lsts);
                        false -> From ! {{error, "Short does not exist, nor as an alias."}},
                                 loop(Pair_of_lsts)

                         end;
                {Short, Emo, Analytics} -> case lists:member(Label, [Ele || {Ele, _, _} <- Analytics]) of
                                                true -> From ! {{error, "Label already exists."}},
                                                        loop(Pair_of_lsts);
                                                false -> Anal = Analytics ++ [{Label, Fun, Init}],
                                                         Temp_Lst = lists:keydelete(Short, 1, element(1, Pair_of_lsts)),
                                                         New_Lst = Temp_Lst ++ [{Short, Emo, Anal}],
                                                         From ! {ok},
                                                         loop({New_Lst, element(2, Pair_of_lsts)})                            
                                           end
            end;

        {{get_analytics, Short}, From} -> 
            case lists:keyfind(Short, 1, element(1, Pair_of_lsts)) of
                {_, _, Analytics} ->  Anal = [{Label, Stats} || {Label,_,Stats} <- Analytics],
                                            From ! {{ok, Anal}},
                                            loop(Pair_of_lsts);
                false -> case lists:keyfind(Short, 1, element(2, Pair_of_lsts)) of
                                {_, RefShort} -> %{Alias, RefShort}
                                    E = self(), 
                                        spawn(fun() -> E ! {{get_analytics, RefShort}, From} end),
                                        loop(Pair_of_lsts);
                                false -> 
                                    From ! ({{error, "Short does not exist, nor as an alias."}}),
                                    loop(Pair_of_lsts)
                         end
            end;

        {{remove_analytics, Short, Label}, From} ->
            case lists:keyfind(Short, 1, element(1, Pair_of_lsts)) of
                {Short, Emo, Analytics} -> 
                    Anal = lists:keydelete(Label, 1, Analytics),
                    Temp_Lst = lists:keydelete(Short, 1, element(1, Pair_of_lsts)),
                    New_Lst = Temp_Lst ++ [{Short, Emo, Anal}], 
                    From ! {ok},
                    loop({New_Lst, element(2, Pair_of_lsts)});

                false -> 
                    case lists:keyfind(Short, 1, element(2, Pair_of_lsts)) of
                        {_, RefShort} -> 
                            E = self(), 
                            spawn(fun() -> E ! {{remove_analytics, RefShort, Label}, From} end),
                            loop(Pair_of_lsts);

                        false -> From ! ({{error, "Short does not exist, nor as an alias."}})
                end
            end;

        {{alias, Short1, Short2}, From} -> 
                case lists:keymember(Short1, 1, element(1, Pair_of_lsts)) of
                    false ->  case lists:keyfind(Short1, 1, element(2, Pair_of_lsts)) of
                                {_, RefShort} ->
                                    E = self(), 
                                    spawn(fun() -> E ! {{alias, RefShort, Short2}, From} end),
                                    loop(Pair_of_lsts);
                                false -> 
                                    From ! {{error, "Short1 does not exists."}},
                                    loop(Pair_of_lsts)
                             end;
                    true -> case lists:keymember(Short2, 1, element(2, Pair_of_lsts)) of
                                true -> From ! {{error, "Alias already exists."}};
                                false -> NewLst =  {element(1, Pair_of_lsts), element(2, Pair_of_lsts) ++ [{Short2, Short1}]},
                                        From ! {ok},
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
