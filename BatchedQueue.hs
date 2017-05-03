{-# LANGUAGE LambdaCase #-}
module BatchedQueue where

data Queue a = Queue
    { front :: [a]
    , rear  :: [a] }

-- | Rotates the rear of the queue to the front if the front is empty
-- Ensures that the queue is empty iff the front is empty.
check :: Queue a -> Queue a
check (Queue [] rs) = Queue (reverse rs) []
check queue         = queue

-- | Inserts an element at the rear end.
insert :: a -> Queue a -> Queue a
insert a (Queue fs rs) = check (Queue fs (a : rs))

-- | Expects – and keeps – invariant: front empty ⇒ queue ampty
viewFront :: Queue a -> Maybe (a, Queue a)
viewFront = \case
    Queue []       _  -> Nothing -- Expecting the invariant!
    Queue (f : fs) rs -> Just (f, check (Queue fs rs))
