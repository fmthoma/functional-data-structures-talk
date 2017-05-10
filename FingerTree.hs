module FingerTree where


data FingerTree a
    = Empty
    | Single a
    | Deep !Int !(Digit a) (FingerTree (Node a)) !(Digit a)

data Digit a = One a | Two a a | Three a a a | Four a a a a

data Node a = Node2 a a | Node3 a a a

class Sized a where
    size :: a -> Int

instance Sized a => Sized (FingerTree a) where
    size Empty          = 0
    size (Single a)     = size a
    size (Deep s _ _ _) = s

instance Sized a => Sized (Digit a) where
    size (One a)        = size a
    size (Two a b)      = size a + size b
    size (Three a b c)  = size a + size b + size c
    size (Four a b c d) = size a + size b + size c + size d

instance Sized a => Sized (Node a) where
    size (Node2 a b)   = size a + size b
    size (Node3 a b c) = size a + size b + size c

instance Sized a => Sized (Maybe a) where
    size Nothing  = 0
    size (Just a) = size a

deep :: Sized a => Digit a -> FingerTree (Node a) -> Digit a -> FingerTree a
deep l t r = Deep (size l + size t + size r) l t r

{-|
>>> toList (cons (E 1) (cons (E 2) (cons (E 3) Empty))) :: [Int]
[1,2,3]

>>> toList (cons (E 5) (fromList [4,3,2,1])) :: [Int]
[5,4,3,2,1]
-}
cons :: Sized a => a -> FingerTree a -> FingerTree a
cons a Empty = Single a
cons a (Single b) = deep (One a) Empty (One b)
cons a (Deep s l t r) = case l of
    One   b       -> Deep s' (Two   a b)     t                      r
    Two   b c     -> Deep s' (Three a b c)   t                      r
    Three b c d   -> Deep s' (Four  a b c d) t                      r
    Four  b c d e -> t `seq`
                     Deep s' (Two   a b)     (Node3 c d e `cons` t) r
  where s' = s + size a

{-|
>>> toList (snoc (fromList [1..19]) (E 20)) :: [Int]
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
-}
snoc :: Sized a => FingerTree a -> a -> FingerTree a
snoc Empty z = Single z
snoc (Single y) z = deep (One y) Empty (One z)
snoc (Deep s l t r) z = case r of
    One        y -> Deep s' l t                      (Two y z)
    Two      x y -> Deep s' l t                      (Three x y z)
    Three  w x y -> Deep s' l t                      (Four w x y z)
    Four v w x y -> t `seq`
                    Deep s' l (t `snoc` Node3 v w x) (Two x z)
  where s' = s + size z

{-|
>>> fmap (const ()) (viewL (fromList []))
Nothing

>>> let Just (E a, t) = viewL (fromList [1]) in (a, toList t)
(1,[])

>>> let Just (E a, t) = viewL (fromList [1,2,3]) in (a, toList t)
(1,[2,3])

>>> let Just (E a, t) = viewL (fromList [1..100]) in (a, toList t)
(1,[2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100])
-}
viewL :: Sized a => FingerTree a -> Maybe (a, FingerTree a)
viewL Empty = Nothing
viewL (Single a) = Just (a, Empty)
viewL (Deep s l t r) = Just $ case l of
    Four  a b c d              -> (a, Deep (s - size a) (Three b c d) t     r)
    Three a b c                -> (a, Deep (s - size a) (Two   b c)   t     r)
    Two   a b                  -> (a, Deep (s - size a) (One   b)     t     r)
    One   a -> case viewL t of
        Just (Node3 b c d, t') -> (a, Deep (s - size a) (Three b c d) t'    r)
        Just (Node2 b c,   t') -> (a, Deep (s - size a) (Two   b c)   t'    r)
        Nothing -> case r of
            Four  b c d e      -> (a, Deep (s - size a) (Two b c)     Empty (Two d e))
            Three b c d        -> (a, Deep (s - size a) (Two b c)     Empty (One d))
            Two   b c          -> (a, Deep (s - size a) (One b)       Empty (One c))
            One   b            -> (a, Single b)

{-|
>>> fmap (const ()) (viewR (fromList []))
Nothing

>>> let Just (t, E a) = viewR (fromList [1]) in (toList t, a)
([],1)

>>> let Just (t, E a) = viewR (fromList [1,2,3]) in (toList t, a)
([1,2],3)

>>> let Just (t, E a) = viewR (fromList [1..100]) in (toList t, a)
([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99],100)
-}
viewR :: Sized a => FingerTree a -> Maybe (FingerTree a, a)
viewR Empty = Nothing
viewR (Single a) = Just (Empty, a)
viewR (Deep s l t r) = Just $ case r of
    Four  w x y z              -> (Deep (s - size z) l             t     (Three w x y), z)
    Three   x y z              -> (Deep (s - size z) l             t     (Two     x y), z)
    Two       y z              -> (Deep (s - size z) l             t     (One       y), z)
    One         z -> case viewR t of
        Just (t', Node3 w x y) -> (Deep (s - size z) l             t'    (Three w x y), z)
        Just (t', Node2   x y) -> (Deep (s - size z) l             t'    (Two     x y), z)
        Nothing -> case l of
            Four  v w x y      -> (Deep (s - size z) (Two v w)     Empty (Two     x y), z)
            Three   w x y      -> (Deep (s - size z) (Two   w x)   Empty (One       y), z)
            Two       x y      -> (Deep (s - size z) (One     x)   Empty (One       y), z)
            One         y      -> (Single y,                                            z)


{-|
Left tree contains items < pos, right tree items >= pos

>>> let (l, E a, r) = split 0 (fromList [1,2,3,4,5]) in (toList l, a, toList r)
([],1,[2,3,4,5])

>>> let (l, E a, r) = split 1 (fromList [1,2,3,4,5]) in (toList l, a, toList r)
([],1,[2,3,4,5])

>>> let (l, E a, r) = split 2 (fromList [1,2,3,4,5]) in (toList l, a, toList r)
([1],2,[3,4,5])

>>> let (l, E a, r) = split 3 (fromList [1,2,3,4,5]) in (toList l, a, toList r)
([1,2],3,[4,5])

>>> let (l, E a, r) = split 4 (fromList [1,2,3,4,5]) in (toList l, a, toList r)
([1,2,3],4,[5])

>>> let (l, E a, r) = split 5 (fromList [1,2,3,4,5]) in (toList l, a, toList r)
([1,2,3,4],5,[])

>>> let (l, E a, r) = split 6 (fromList [1,2,3,4,5]) in (toList l, a, toList r)
([1,2,3,4],5,[])

>>> let (l, E a, r) = split 42 (fromList [1..100]) in (toList l, a, toList r)
([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41],42,[43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100])
-}
split :: Sized a => Int -> FingerTree a -> (FingerTree a, a, FingerTree a)
split _ Empty = error "Cannot split an empty tree"
split pos (Single a) = (Empty, a, Empty)
split pos tree@(Deep s l t r)
    | slt < pos = let (rl, a, rr) = splitDigit (pos - slt) r
                  in  (deepR l t rl, a, maybe Empty digitToTree rr)
    | sl  < pos = let (tl, n, tr) = split (pos - sl) t
                      (nl, a, nr) = splitNode (pos - sl - size tl) n
                  in  (deepR l tl nl, a, deepL nr tr r)
    | otherwise = let (ll, a, lr) = splitDigit pos l
                  in  (maybe Empty digitToTree ll, a, deepL lr t r)
  where sl = size l
        slt = sl + size t

-- Precondition: pos < size digit
splitDigit :: Sized a => Int -> Digit a -> (Maybe (Digit a), a, Maybe (Digit a))
splitDigit _   (One a) = (Nothing, a, Nothing)
splitDigit pos (Two a b)
    | size a < pos = (Just (One a), b, Nothing)
    | otherwise    = (Nothing, a, Just (One b))
splitDigit pos (Three a b c)
    | sab < pos = (Just (Two a b), c, Nothing)
    | sa  < pos = (Just (One a), b, Just (One c))
    | otherwise = (Nothing, a, Just (Two b c))
  where sa  = size a
        sab = sa + size b
splitDigit pos (Four a b c d)
    | sabc < pos = (Just (Three a b c), d, Nothing)
    | sab  < pos = (Just (Two a b), c, Just (One d))
    | sa   < pos = (Just (One a), b, Just (Two c d))
    | otherwise  = (Nothing, a, Just (Three b c d))
  where sa   = size a
        sab  = sa  + size b
        sabc = sab + size c

splitNode :: Sized a => Int -> Node a -> (Maybe (Digit a), a, Maybe (Digit a))
splitNode pos (Node2 a b)
    | size a < pos = (Just (One a), b, Nothing)
    | otherwise    = (Nothing, a, Just (One b))
splitNode pos (Node3 a b c)
    | sab < pos = (Just (Two a b), c, Nothing)
    | sa  < pos = (Just (One a), b, Just (One c))
    | otherwise = (Nothing, a, Just (Two b c))
  where sa  = size a
        sab = sa + size b

deepL :: Sized a => Maybe (Digit a) -> FingerTree (Node a) -> Digit a -> FingerTree a
deepL (Just l) t r = deep l t r
deepL Nothing  t r = case viewL t of
    Just (Node3 a b c, t') -> deep (Three a b c) t' r
    Just (Node2 a b,   t') -> deep (Two   a b)   t' r
    Nothing                -> digitToTree r

deepR :: Sized a => Digit a -> FingerTree (Node a) -> Maybe (Digit a) -> FingerTree a
deepR l t (Just r) = deep l t r
deepR l t Nothing  = case viewR t of
    Just (t', Node3 x y z) -> deep l t' (Three x y z)
    Just (t', Node2   y z) -> deep l t' (Two     y z)
    Nothing                -> digitToTree l

digitToTree :: Sized a => Digit a -> FingerTree a
digitToTree (One   a)       = Single a
digitToTree (Two   a b)     = deep (One a)   Empty (One b)
digitToTree (Three a b c)   = deep (Two a b) Empty (One c)
digitToTree (Four  a b c d) = deep (Two a b) Empty (Two c d)



data Element a = E a
instance Sized (Element a) where size _ = 1

fromList :: [a] -> FingerTree (Element a)
fromList = foldr cons Empty . map E

toList :: FingerTree (Element a) -> [a]
toList t | Just (E a, t') <- viewL t = a : toList t'
         | otherwise                 = []
