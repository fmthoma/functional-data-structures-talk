{-# LANGUAGE LambdaCase #-}
module LeftistHeap where

import           Prelude hiding (head, (++))

data Heap a
    = Empty
    | Heap
         { _rank :: !Int
         , head  :: a
         , left  :: Heap a
         , right :: Heap a }

rank :: Heap a -> Int
rank = \case
    Empty        -> 0
    Heap r _ _ _ -> r

mkLeftist :: a -> Heap a -> Heap a -> Heap a
mkLeftist a xs ys
    | rank xs < rank ys = Heap (rank xs + 1) a ys xs
    | otherwise         = Heap (rank ys + 1) a xs ys

(++) :: Ord a => Heap a -> Heap a -> Heap a
Empty ++ xs = Empty
xs ++ Empty = Empty
xs ++ ys
    | head xs <= head ys = mkLeftist (head xs) (left xs) (right xs ++ ys)
    | otherwise          = mkLeftist (head ys) (left ys) (xs ++ right ys)

insert :: Ord a => a -> Heap a -> Heap a
insert a Empty = mkLeftist a Empty Empty
insert a xs
    | a <= head xs = mkLeftist a xs Empty
    | otherwise    = mkLeftist (head xs) (insert a (left xs)) (right xs)
