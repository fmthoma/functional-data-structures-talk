module SkewBinomialHeap where

import GHC.Exts

data Tree a = Node
    { rank   :: !Int
    , root   :: a
    , forest :: !(Forest a) }

type Forest a = StrictList (Tree a)

-- | Always link trees of equal rank!
link :: Ord a => Tree a -> Tree a -> Tree a
link left right
    | rank left /= rank right = error "Only link trees of equal rank"
    | root left <= root right = Node rank' (root left)  (right :! forest left)
    | otherwise               = Node rank' (root right) (left  :! forest right)
  where rank' = rank left + 1

skewLink :: Ord a => Tree a -> Tree a -> Tree a -> Tree a
skewLink top left right
    | rank left /= rank right = error "Only link trees of equal rank"
    | root left  <= root top && root left  <= root right = Node rank' (root left)  (top  :! right :! forest left)
    | root right <= root top && root right <= root left  = Node rank' (root right) (top  :! left  :! forest right)
    | otherwise                                          = Node rank' (root top)   (left :! right :! forest top)
  where rank' = rank left + 1

skewInsert :: Ord a => Tree a -> Forest a -> Forest a
skewInsert tree (left :! right :! rest)
    | rank left == rank right = skewLink tree left right :! rest
skewInsert tree trees         = tree :! trees

data Heap a = Empty | Heap (Tree a)

-- | O(1) worst-case
insert :: Ord a => a -> Heap a -> Heap a
insert a Empty = Heap (Node 0 a Nil)
insert a (Heap tree)
    | a <= root tree = Heap (Node 0 a (tree :! Nil))
    | otherwise      = Heap (Node 0 (root tree) (skewInsert (Node 0 a Nil) (forest tree)))

-- | O(1) worst-case
merge :: Ord a => Heap a -> Heap a -> Heap a
merge heap Empty = heap
merge Empty heap = heap
merge (Heap left) (Heap right)
    | root left <= root right = Heap (Node 0 (root left)  (skewInsert right (forest left)))
    | otherwise               = Heap (Node 0 (root right) (skewInsert left (forest right)))

-- | O(log n) worst-case; O(1) if the updated heap is not forced
viewMin :: Ord a => Heap a -> Maybe (a, Heap a)
viewMin Empty            = Nothing
viewMin (Heap tree) = Just (root tree, deleteMin tree)
  where
    deleteMin :: Ord a => Tree a -> Heap a
    deleteMin tree | Nil <- forest tree = Empty
                   | otherwise          = Heap (restructure (forest tree))

    restructure :: Ord a => Forest a -> Tree a
    restructure f = Node 0 (root minTree) (foldr skewInsert rest' left)
      where (minTree, rest) = findMinTree f
            (top, left, right) = splitForest (rank minTree) (forest minTree, Nil, Nil)
            rest' = skewMerge (skewMerge right rest) top

    findMinTree :: Ord a => Forest a -> (Tree a, Forest a)
    findMinTree (t :! Nil) = (t, Nil)
    findMinTree (t :! ts)
        | root t < root t' = (t, ts)
        | otherwise        = (t', t :! ts')
      where (t', ts') = findMinTree ts

    splitForest :: Int -> (Forest a, Forest a, Forest a) -> (Forest a, Forest a, Forest a)
    splitForest 0 (top, left, right) = (top, left, right)
    splitForest 1 (t :! Nil, left, right) = (Nil, left, t  :! right)
    splitForest 1 (t1 :! t2 :! ts, left, right)
        | rank t2 == 0                    = (ts, t1 :! left, t2 :! right)
        | otherwise                       = (t2 :! ts, left, t1 :! right)
    splitForest r (t1 :! t2 :! ts, left, right)
        | rank t1 == rank t2              = (ts, left, t1 :! t2 :! right)
        | rank t1 == 0                    = splitForest (r - 1) (ts, t1 :! left, t2 :! right)
        | otherwise                       = splitForest (r - 1) (t2 :! ts, left, t1 :! right)

    skewMerge :: Ord a => Forest a -> Forest a -> Forest a
    skewMerge left right = unionUniq (uniquify left) (uniquify right)

    unionUniq :: Ord a => Forest a -> Forest a -> Forest a
    unionUniq Nil trees = trees
    unionUniq trees Nil = trees
    unionUniq lefts@(left :! lefts') rights@(right :! rights')
        | rank left < rank right = left  :! unionUniq lefts' rights
        | rank left > rank right = right :! unionUniq lefts rights'
        | otherwise              = insertTree (link left right) (unionUniq lefts' rights')

    uniquify :: Ord a => Forest a -> Forest a
    uniquify Nil = Nil
    uniquify (tree :! trees) = insertTree tree trees

    insertTree :: Ord a => Tree a -> Forest a -> Forest a
    insertTree tree Nil = tree :! Nil
    insertTree tree (tree' :! trees)
        | rank tree > rank tree' = error "Inserted tree must have smaller or equal rank"
        | rank tree < rank tree' = tree :! tree' :! trees
        | otherwise              = insertTree (link tree tree') trees


-- | Helper: A list where spine and leaves are strict
infixr 5 :!
data StrictList a = Nil | !a :! !(StrictList a)

instance Foldable StrictList where
    foldr f z = go
      where go Nil = z
            go (x :! xs) = f x (go xs)
