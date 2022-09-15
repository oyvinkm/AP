module Warmup where

{-# OPTIONS_GHC -W #-}  -- Just in case you forgot...

type Pos = (Int, Int)
data Direction = North | South | East | West
  deriving (Eq, Show, Read, Ord)

move :: Direction -> Pos -> Pos
move North (x,y) = (x, y+1)
move West (x,y) = (x-1, y)
move East (x,y) = (x+1, y)
move South (x,y) = (x,y-1)

moves :: [Direction] -> Pos -> Pos
moves [] pos = pos
moves (dir:dirs) pos = moves dirs (move dir pos)

data Nat = Zero | Succ Nat
  deriving (Eq, Show, Read, Ord)

add :: Nat -> Nat -> Nat
add a Zero = a
add Zero b = b
add a (Succ b) = add (Succ a) b
-- add _ _= undefined

mult :: Nat -> Nat -> Nat
mult _ Zero = Zero
mult Zero _ = Zero
mult a (Succ Zero) = a
mult a (Succ b) = add a (mult a b)
-- mult _ _ = undefined

-- Do not use these to define add/mult!
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ a) = 1 + nat2int a

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat a = Succ (int2nat (a-1))

data Tree = Leaf | Node Int Tree Tree
  deriving (Eq, Show, Read, Ord)

insert :: Int -> Tree -> Tree
insert n Leaf = Node n Leaf Leaf
insert n (Node a left right)
    | n == a = Node a left right
    | n < a = Node a (insert n left) right
    | otherwise = Node a left (insert n right)


-- The polymorphic variant, to avoid name clashes with the above
data PTree a = PLeaf | PNode a (PTree a) (PTree a)
  deriving (Eq, Show, Read, Ord)

pinsert :: (Ord a) => a -> PTree a -> PTree a -- uncomment and replace with the proper type of pinsert
pinsert n PLeaf = PNode n PLeaf PLeaf
pinsert n (PNode a left right)
    | n == a = PNode a left right
    | n < a = PNode a (pinsert n left) right
    | otherwise = PNode a left (pinsert n right)
