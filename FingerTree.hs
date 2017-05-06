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

deep :: Sized a => Digit a -> FingerTree (Node a) -> Digit a -> FingerTree a
deep l t r = Deep (size l + size t + size r) l t r

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

viewR :: Sized a => FingerTree a -> Maybe (FingerTree a, a)
viewR = viewR -- Just the same as viewL


-- Left tree contains items < pos, right tree items >= pos
split :: Sized a => Int -> FingerTree a -> (FingerTree a, FingerTree a)
split 0 t = (Empty, t)
split _   Empty = (Empty, Empty)
split pos (Single a)
    | size a < pos = (Single a, Empty)
    | otherwise    = (Empty, Single a)
split pos (Deep s l t r)
    | s   < pos  = (Deep s l t r, Empty)
    | slt < pos  = case splitDigit (pos - slt) r of
        (Nothing, r') -> _
    | sl  < pos  = _
    | otherwise  = _
  where sl = size l
        slt = sl + size t

-- Precondition: pos < size digit
splitDigit :: Sized a => Int -> Digit a -> (Maybe (Digit a), Digit a)
splitDigit _   (One a) = (Nothing, One a)
splitDigit pos (Two a b)
    | size a < pos = (Just (One a), One b)
    | otherwise    = (Nothing, Two a b)
splitDigit pos (Three a b c)
    | sab < pos = (Just (Two a b), One c)
    | sa  < pos = (Just (One a), Two b c)
    | otherwise = (Nothing, Three a b c)
  where sa  = size a
        sab = sa + size b
splitDigit pos (Four a b c d)
    | sabc < pos = (Just (Three a b c), One d)
    | sab  < pos = (Just (Two a b), Two c d)
    | sa   < pos = (Just (One a), Three b c d)
    | otherwise  = (Nothing, Four a b c d)
  where sa   = size a
        sab  = sa  + size b
        sabc = sab + size c
