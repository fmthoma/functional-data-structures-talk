{-# LANGUAGE LambdaCase #-}
module BinomialHeap where

import           Prelude hiding (head, tail, (++))

-- | Invariants:
--
--    * A tree of 'rank' n contains exactly 2^n elements
--    * A tree of 'rank' n contains exactly n subtrees of 'rank's 1..n
--    * The subtrees in 'tail' are stored by increasing rank
--    * Elements are in heap order (head is the smallest element)
data Tree a = Node
    { rank :: !Int
    , head :: a
    , tail :: [Tree a] }

-- | Invariants:
-- * The trees are stored in increasing rank
newtype Heap a = Heap [Tree a]

-- | Linking works only for trees of same rank!
link :: Ord a => Tree a -> Tree a -> Tree a
link s t
    | rank s == rank t = if head s <= head t
        then Node (rank s + 1) (head s) (t : tail s)
        else Node (rank s + 1) (head t) (s : tail t)
    | otherwise        = undefined


-- | Rank of the inserted tree must be <= rank of the Heap!
insertTree :: Ord a => Tree a -> Heap a -> Heap a
insertTree s = \case
    Heap [] -> Heap [s]
    Heap (t : ts)
        | rank s <  rank t -> Heap (s : t : ts)
        | rank s == rank t -> insertTree (link s t) (Heap ts)
        | otherwise        -> undefined

insert :: Ord a => a -> Heap a -> Heap a
insert a = insertTree (Node 0 a [])

