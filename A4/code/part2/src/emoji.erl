-module(emoji).

-compile(export_all).

-type shortcode() :: string().
-type emoji() :: binary().
-type analytic_fun(State) :: fun((shortcode(), State) -> State).

hit(_, N) -> N+1.

accessed(SC, TS) ->
    Now = calendar:local_time(),
    [{SC,Now} | TS].

start(Initial) -> 
    case length(Initial) == length(lists:ukeysort(1, Initial)) of
        false -> {error, reasons};
        true -> case length(Initial) > 0 of
                    true -> Temp = 
                [{Short, Emo, []} || {Short, Emo} <- Initial],
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
new_shortcode(E, Short, Emo) -> 
    request_reply({new_shortcode, Short, Emo}, E).

alias(E, Short1, Short2) -> 
    request_reply({alias, Short1, Short2}, E).

delete(E, Short) -> 
    request_reply({delete, Short}, E).

lookup(E, Short) -> 
    request_reply({lookup, Short}, E).

analytics(E, Short, Fun, Label, Init) -> 
                    request_reply({analytics, Short, Fun, Label, Init}, E).

get_analytics(E, Short) -> 
    request_reply({get_analytics, Short}, E).

remove_analytics(E, Short, Label) -> 
    request_reply({remove_analytics, Short, Label}, E).

stop(E) -> request_reply(stop, E).

map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].

eval_fun(_, {}) -> [];
eval_fun(Emo, {Label, F, State}) -> 
    {Label, F, F(Emo,State)}.


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
                true -> 
                    From ! {{error, "Short already exists"}},
                    loop(Pair_of_lsts);
                false -> 
                    NewLst =  {element(1, Pair_of_lsts) ++ [{Short, Emo, []}], 
                    element(2, Pair_of_lsts)},
                    From ! {ok},
                    loop(NewLst)
            end;
        %Deletes entry if Short is the Short of the primary list or
        % If Short is Short2 in the secondary list
        {{delete, Short}, From} ->
            E = self(),
            Primary = element(1, Pair_of_lsts),
            Prim_filtered = filter(fun(X) -> 
                remove_if_first(Short, X) end, Primary),

            Alias = element(2, Pair_of_lsts),
            Alias_filtered = filter(fun(X) ->
                remove_if_second(Short, X) end, Alias),

            %Finds the one element in the original list, and deletes that
            %entry
            Filtered = {Prim_filtered, Alias_filtered},
            case lists:keyfind(Short, 1, element(2, Filtered)) of
                        {_, RefShort} ->
                            spawn(fun() -> 
                                E ! {{delete, RefShort}, From} end),
                            loop(Filtered);
                        false -> 
                            From ! {ok},
                            loop({Prim_filtered, Alias_filtered})
                end;

        {{lookup, Short}, From} ->
            case lists:keyfind(Short, 1, element(1,Pair_of_lsts)) of
                {NewShort, Emo, Analytics} -> 
                    %This try only works if there is one function as we evaluate
                    %the whole list, so if one fails, all fail. 
                    %can be fixed by handling bad functions in eval_fun.
                        try  [eval_fun(Short, X) || X <- Analytics] of 
                            Anal -> Temp_Lst = 
                        lists:keydelete(NewShort, 1, element(1, Pair_of_lsts)),
                                    NewLst = 
                                {Temp_Lst ++ [{NewShort, Emo, Anal}], 
                                            element(2, Pair_of_lsts)},
                                    From ! {{ok, Emo}},
                                    loop(NewLst)
                        catch
                            _ : _ -> From ! {{ok, Emo}},
                                        loop(Pair_of_lsts)
                        end;
                       
                false -> 
                    % looks for Short, in first element of tuple in second element of Pair_of_lsts
                    case lists:keyfind(Short, 1, element(2, Pair_of_lsts)) of
                        {_, RefShort} ->
                            E = self(), 
                            spawn(fun() -> E ! 
                        {{lookup, RefShort}, From} end),
                            loop(Pair_of_lsts);
                        false -> 
                            From ! { no_emoji},
                            loop(Pair_of_lsts)
                    end
                end;

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
            case lists:keyfind(Short, 1, element(1, Pair_of_lsts)) of
                false -> case lists:keyfind(Short, 1, element(2, Pair_of_lsts)) of
                    {_, RefShort} -> 
                        E = self(), 
                            spawn(fun() -> E ! 
                        {{analytics, RefShort, Fun, Label, Init}, From} end),
                            loop(Pair_of_lsts);
                        false -> From ! 
                    {{error, "Short does not exist, nor as an alias."}},
                                 loop(Pair_of_lsts)

                         end;
                {Short, Emo, Analytics} -> 
                    case lists:member(Label, [Ele || {Ele, _, _} <- Analytics]) of
                    true -> From ! {{error, "Label already exists."}},
                            loop(Pair_of_lsts);
                    false -> Anal = Analytics ++ [{Label, Fun, Init}],
                                Temp_Lst = 
                            lists:keydelete(Short, 1, element(1, Pair_of_lsts)),
                                New_Lst = Temp_Lst ++ [{Short, Emo, Anal}],
                                From ! {ok},
                                loop({New_Lst, element(2, Pair_of_lsts)})                            
                    end
            end;

        {{get_analytics, Short}, From} -> 
            case lists:keyfind(Short, 1, element(1, Pair_of_lsts)) of
                {_, _, Analytics} ->  
                    Anal = [{Label, Stats} || {Label,_,Stats} <- Analytics],
                    From ! {{ok, Anal}},
                    loop(Pair_of_lsts);
                false -> 
                    case lists:keyfind(Short, 1, element(2, Pair_of_lsts)) of
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
                    Temp_Lst = 
                    lists:keydelete(Short, 1, element(1, Pair_of_lsts)),
                    New_Lst = Temp_Lst ++ [{Short, Emo, Anal}], 
                    From ! {ok},
                    loop({New_Lst, element(2, Pair_of_lsts)});

                false -> 
                    case lists:keyfind(Short, 1, element(2, Pair_of_lsts)) of
                        {_, RefShort} -> 
                            E = self(), 
                            spawn(fun() -> E ! 
                            {{remove_analytics, RefShort, Label}, From} end),
                            loop(Pair_of_lsts);

                        false -> From ! 
                            ({{error, 
                            "Short does not exist, nor as an alias."}})
                end
            end;

        {{alias, Short1, Short2}, From} -> 
                case lists:keymember(Short1, 1, element(1, Pair_of_lsts)) of
                    false ->  
                        case lists:keyfind(Short1, 1, element(2, Pair_of_lsts)) of
                        {_, RefShort} ->
                            E = self(), 
                            spawn(fun() -> E ! {{alias, RefShort, Short2}, From} end),
                            loop(Pair_of_lsts);
                        false -> 
                            From ! {{error, "Short1 does not exists."}},
                            loop(Pair_of_lsts)
                        end;
                    true -> 
                        case lists:keymember(Short2, 1, element(2, Pair_of_lsts)) of
                        true -> From ! {{error, "Alias already exists."}};
                        false -> NewLst =  
                                    {element(1, Pair_of_lsts), 
                                    element(2, Pair_of_lsts) ++ 
                                    [{Short2, Short1}]},
                                From ! {ok},
                                loop(NewLst)
                        end
                end
    end.
