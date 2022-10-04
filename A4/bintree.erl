-module(bintree).

-export([init/0, insert/2, contains/2]).

% Example tree
% T = {node, 6, {node, 3, leaf, leaf} {node, 9 leaf, leaf}}

init() -> {node, 'nil', leaf, leaf}.

contains(_, leaf) -> false;
contains(Key, {node, K, Left, Right}) ->
    if Key =:= K -> true;
        Key < K -> contains(Key, Left);
        Key > K -> contains(Key, Right)
    end.

insert(Key, {node, 'nil', leaf, leaf}) -> {node, Key, leaf, leaf};
insert(Key, leaf) -> case leaf of
                            {node, K, Left, Right} -> if Key =< K -> insert(Key, Left);
                                                     Key > K -> insert(Key, Right)
                     end.
    


