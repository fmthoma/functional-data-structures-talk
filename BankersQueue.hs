{-# LANGUAGE LambdaCase #-}
module BankersQueue where

data Queue a = Queue
    { flen  :: Int
    , front :: [a]
    , rlen  :: Int
    , rear  :: [a] }

check :: Queue a -> Queue a
check queue@(Queue fl fs rl rs)
    | fl >= rl  = queue
    | otherwise = Queue (fl + rl) (fs ++ reverse rs) 0 []

insert :: a -> Queue a -> Queue a
insert a = \case
    Queue _  [] _  [] -> Queue 1 [a] 0 []
    Queue fl fs rl rs -> check (Queue fl fs (rl + 1) (a : rs))
