-module(test_bst).

-import(bst, [empty/0, insert/3, delete/2, find/2, union/2]).
-import(bst, [valid/1, to_sorted_list/1, keys/1]).

-include_lib("eqc/include/eqc.hrl").

%% The following two lines are super bad style, except during development
-compile(nowarn_export_all).
-compile(export_all).


%%% A symbolic generator for bst, parameterised by key and value generators
bst(Key, Value) ->
    ?LAZY(
        frequency([{1, {call, bst, empty, []}}
            , {4, ?LETSHRINK([KVS], [bst(Key, Value)],
                {call, bst, insert, [Key, Value, KVS]})}
            ])).

% example key and value generators
int_key() -> eqc_gen:int().
atom_key() -> eqc_gen:elements([a,b,c,d,e,f,g,h]).

int_value() -> eqc_gen:int().


%%% invariant properties

% all generated bst are valid
prop_arbitrary_valid() ->
    ?FORALL(T, bst(atom_key(), int_value()),
            valid(eval(T))).

% if we insert into a valid tree it stays valid
prop_insert_valid() ->
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
            valid (eval({call, bst, insert, [K, V, T]}))).

% If we check if a valid tree is empty is it still valid? 
% Do we test empty like this?
prop_empty_valid() -> 
    ?FORALL({_, _, _}, {atom_key(), int_value(), bst(atom_key(), int_value())},
            valid(eval({call, bst, empty, []}))).

%If we delete a node in a tree is it still valid? 
prop_delete_valid() ->
    ?FORALL({K, T}, {atom_key(), bst(atom_key(), int_value())},
            valid (eval({call, bst, delete, [K,T]}))).

%If we take union of a branch and 
prop_union_valid() ->
    ?FORALL({T1, T2}, {bst(atom_key(), int_value()), bst(atom_key(), int_value())},
            valid (eval({call, bst, union, [T1, T2]}))).


%%% -- postcondition properties
prop_insert_post() ->
    ?FORALL({K1, K2, V, T},
            {atom_key(), atom_key(), int_value(), bst(atom_key(), int_value())},
            eqc:equals(find(K2, eval({call, bst, insert, [K1, V, T]})),
                       case K1 =:= K2 of
                           true ->  {found, V};
                           false -> find(K2, eval(T))
                       end)).

prop_delete_post() -> 
    ?FORALL({K1, K2, T},
            {atom_key(), atom_key(), bst(atom_key(), int_value())},
            eqc:equals(find(K2, eval({call, bst, delete, [K1, T]})),
                        case K1 =:= K2 of
                            true -> nothing;
                            false -> find(K2, eval(T))
                        end)).
prop_union_post() ->
    ?FORALL ({K, V,T1, T2},
            {atom_key(), int_value(), 
            bst(atom_key(), int_value()), 
            bst(atom_key(), int_value())},
            eqc:equals(find(K, eval({call, bst, union, 
            [{call, bst, insert, [K, V, T1]}, T2]})), 
            {found, V})).
% Maybe this is metamorphism idk. 
%eval({call, bst, union, [{call, bst, insert, [K, V, T1]}, T2]})

%[{set, {var,1}, {call, bst, insert, [K, V, T1]},},
%{set, {var,2}, {call,erlang,union,[{var,1}, T2]}}]

prop_find_post_present() ->
  % ∀ k v t. find k (insert k v t) === {found, v}
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
            eqc:equals(find(K, eval({call, bst, insert, [K, V, T]})),
                       {found, V})).


prop_find_post_absent() -> 
     % ∀ k t. find k (delete k t) === nothing
    ?FORALL({K, T}, {atom_key(), bst(atom_key(), int_value())},
            eqc:equals(find(K, eval({call, bst, delete, [K, T]})),
                       nothing)).


%%% -- metamorphic properties

%% the size is larger after an insert
prop_size_insert() ->
    % ∀ k v t. size (insert k v t) >= size t
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
            bst:size(eval({call, bst, insert, [K, V, T]})) >= bst:size(eval(T))).

prop_size_delete() -> 
    ?FORALL({K, T}, {atom_key(), bst(atom_key(), int_value())},
            bst:size(eval({call, bst, delete, [K, T]})) =< bst:size(eval(T))).

prop_size_union() -> 
    ?FORALL({T1,T2}, {bst(atom_key(), int_value()), bst(atom_key(), int_value())},
            (bst:size(eval({call, bst, union, [T1, T2]})) >= bst:size(eval(T1))) and 
            (bst:size(eval({call, bst, union, [T1, T2]})) >= bst:size(eval(T2)))).

prop_insert_insert() ->
    ?FORALL({K1, K2, V1, V2, T},
            {atom_key(), atom_key(), int_value(), int_value(),
             bst(atom_key(), int_value())},
            obs_equals(eval({call, bst, insert, 
            [K1, V1, {call, bst, insert, [K2, V2, T]}]}),
                       case K1 =:= K2 of
                           true ->  eval({call, bst, insert, [K1, V1, T]});
                           false -> eval({call, bst, insert, 
                                    [K2, V2, 
                                    {call, bst, insert, [K1, V1, T]}]})
                       end)).

prop_insert_union() -> 
    ?FORALL({K, V, T1, T2}, 
        {atom_key(), int_value(), bst(atom_key(), int_value()),
        bst(atom_key(), int_value())},
        eqc:equals(find(K, 
        eval({call, bst, union, 
            [{call, bst, insert, 
            [K,V,T1]},T2]})), 
        find(K, eval({call, bst, insert, 
        [K,V,T1]})))).

        
% Is this proper
prop_insert_delete() ->
    ?FORALL({K, V, T},
            {atom_key(), int_value(), bst(atom_key(), int_value())},
            obs_equals(eval({call, bst, delete, [K, T]}), 
            eval({call, bst, delete, 
            [K, {call, bst, insert, [K, V, T]}]}))).

obs_equals(T1, T2) ->
     eqc:equals(to_sorted_list(eval(T1)), to_sorted_list(eval(T2))).
        
prop_delete_insert() ->
    ?FORALL({K, V, T},
            {atom_key(), int_value(), bst(atom_key(), int_value())},
            obs_equals(eval({call, bst, insert, [K, V, T]}), 
            eval({call, bst, insert, 
                [K, V, {call, bst, delete, [K, T]}]}))).

prop_delete_delete() -> 
    ?FORALL({K1, K2, T}, {atom_key(), atom_key(), 
            bst(atom_key(), int_value())},
            obs_equals(eval({call, bst, delete, 
                    [K1, {call, bst, delete, [K2, T]}]}), 
                        case K1 =:= K2 of
                            true -> 
                                eval({call, bst, delete, [K1, T]});
                            false -> 
                                eval({call, bst, delete, [K2, {call, bst, delete, [K1, T]}]})
                        end)).

prop_delete_union() ->
    ?FORALL({K, T1, T2},
        {atom_key(), bst(atom_key(), int_value()), bst(atom_key(), int_value())},
        obs_equals(eval(
            {call, bst, union, 
            [{call, bst, delete, [K, T1]}, 
            {call, bst, delete, [K, T2]}]}), 
            eval({call, bst, delete, [K, {call, bst, union, [T1, T2]}]}))).

prop_union_insert() ->
    ?FORALL({K, V, T1, T2},
        {atom_key(), int_value(), bst(atom_key(), int_value()), bst(atom_key(), int_value())},
        obs_equals(
            eval({call, bst, union, 
                [{call, bst, insert, [K, V, T1]}, 
                {call, bst, insert, [K, V, T2]}]}), 
            eval({call, bst, insert, 
                [K, V, 
                {call, bst, union, [T1, T2]}]}))).

prop_union_union() ->
    ?FORALL({T1, T2},
        {bst(atom_key(), int_value()), bst(atom_key(), int_value())},
        obs_equals(
            eval({call, bst, union, 
                [T1, {call, bst, union, 
                [{call, bst, empty, []}, T2]}]})
            , eval({call, bst, union, [T1, T2]}))).

prop_union_union2() -> 
    ?FORALL({T1, T2, T3},
        {bst(atom_key(), int_value()), bst(atom_key(), int_value()),
        bst(atom_key(), int_value())},
        obs_equals(
            eval({call, bst, union, 
                [T1, {call, bst, union, 
                [T2, T3]}]})
            , eval({call, bst, union, 
                [{call, bst, union, [T1, T2]}, T3]}))).

            % union(T1, union(T2, T3)), union(union(T1, T2), T3))).

%%% -- Model based properties
model(T) -> to_sorted_list(T).


prop_insert_model() ->
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
        equals(
            model(eval({call, bst, insert, [K, V, T]})), 
            sorted_insert(K, V, delete_key(K, model(eval(T)))))).
                

prop_find_model()->
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
        equals(
            sorted_find(K, model(eval({call, bst, insert, [K, V, T]}))),
            {found, V})).
            % equals(sorted_find(K, model(insert(K, V, T))), {found, V})).

prop_empty_model() -> 
        equals(sorted_find(atom_key(), 
            model(eval({call, bst, empty, []}))), nothing).
    

prop_delete_model() -> 
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
        equals(model(eval({call, bst, delete, [K, T]})),
            delete_key(K, sorted_insert(K, V, model(eval(T))))
            )).

prop_union_model() -> 
    ?FORALL({T1,T2}, {bst(atom_key(), int_value()), bst(atom_key(), int_value())},
        equals(model(eval({call, bst, union, [T1, T2]})),
            sorted_union(model(eval(T1)), 
            model(eval(T2)))
            )).
    % equals(model(union(T1, T2)), sorted_union(model(T1), model(T2)))).


-spec delete_key(Key, [{Key, Value}]) -> [{Key, Value}].
delete_key(Key, KVS) -> [ {K, V} || {K, V} <- KVS, K =/= Key ].

-spec sorted_insert(Key, Value, [{Key, Value}]) -> nonempty_list({Key, Value}).
sorted_insert(Key, Value, [{K, V} | Rest]) when K < Key ->
    [{K, V} | sorted_insert(Key, Value, Rest)];
sorted_insert(Key, Value, KVS) -> [{Key, Value} | KVS].

-spec sorted_find(Key, [{Key, Value}]) -> {found, Value}.
sorted_find(Key, KVS) -> case lists:keyfind(Key, 1, KVS) of
                            {_, V} -> {found, V};
                            false -> nothing
                         end.

-spec sorted_union([{Key, Value}], [{Key, Value}]) -> [{Key, Value}].
sorted_union(KVS1, KVS2) -> lists:ukeymerge(1, KVS1, KVS2).
%% -- Test all properties in the module: eqc:module(test_bst)