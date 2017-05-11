module BankersQueue where

data Queue a = Queue
    { lenWrite :: !Int
    , write    :: [a]
    , lenRead  :: !Int
    , read     :: [a] }

check :: Queue a -> Queue a
check queue@(Queue {..})
    | lenWrite > lenRead = Queue 0 [] (lenRead + lenWrite) (read ++ reverse write))
    | otherwise          = queue

empty :: Queue a
empty = Queue 0 [] 0 []

insert :: a -> Queue a -> Queue a
insert a (Queue {..}) = check (Queue (lenWrite + 1) (a : write) lenRead read)


view :: Queue a -> (a, Queue a)
view (Queue {..}) = case read of
    [] -> error "Empty queue"
    a : read' -> (a, check (Queue lenWrite write (lenRead - 1) read'))
