{-# LANGUAGE LambdaCase #-}
module BinomialHeap where

import           Prelude hiding (head, tail, (++))

-- | Invariants:
--
--    * The 'forest' of a 'Tree' of 'rank' r contains exactly r trees of 'rank' r-1, …, 0
--    * A 'Tree' of 'rank' n contains exactly 2^n elements
--    * Elements are in heap order (head is the smallest element)
data Tree a = Node
    { rank   :: !Int
    , root   :: a
    , forest :: ![Tree a] }

-- | Always link trees of equal rank!
link :: Ord a => Tree a -> Tree a -> Tree a
link left right
    | rank left /= rank right = error "Only link trees of equal rank"
    | root left <= root right = Node rank' (root left)  (right : forest left)
    | otherwise               = Node rank' (root right) (left  : forest right)
  where rank' = rank left + 1



-- | Invariants:
-- * The trees are stored by increasing rank
newtype Heap a = Heap [Tree a]


insert :: Ord a => a -> Heap a -> Heap a
insert a (Heap trees) = Heap (insertTree (Node 0 a []) trees)

insertTree :: Ord a => Tree a -> [Tree a] -> [Tree a]
insertTree s [] = [s]
insertTree s (t : ts)
    | rank s < rank t  -- found a free spot
        = s : t : ts
    | otherwise -- must be equal rank (because of sorting) => carry
        = insertTree (link s t) ts


merge :: Ord a => Heap a -> Heap a -> Heap a
merge (Heap left) (Heap right) = Heap (mergeTrees left right)

mergeTrees :: Ord a => [Tree a] -> [Tree a] -> [Tree a]
mergeTrees left   []    = left
mergeTrees []     right = right
mergeTrees (l:ls) (r:rs)
    | rank l > rank r  = l : mergeTrees ls (r:rs)
    | rank l < rank r  = r : mergeTrees (l:ls) rs
    | otherwise        = link l r : mergeTrees ls rs


viewMin :: Ord a => Heap a -> Maybe (a, Heap a)
viewMin (Heap []) = Nothing
viewMin (Heap trees) = Just (root minTree, Heap rest)

  where (minTree, trees') = findMinTree trees
        rest = mergeTrees trees' (reverse (forest minTree))

        findMinTree :: Ord a => [Tree a] -> (Tree a, [Tree a])
        findMinTree [t] = (t, [])
        findMinTree (t : ts)
            | root t < root t' = (t, ts)
            | otherwise        = (t', t:ts')
          where (t', ts') = findMinTree ts
