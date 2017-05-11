module BatchedQueue where

data Queue a = Queue [a] [a]

check :: Queue a -> Queue a
check (Queue write []) = Queue [] (reverse write)
check queue            = queue

empty :: Queue a
empty = Queue [] []

insert :: a -> Queue a -> Queue a
insert a (Queue write read) = check (Queue (a : write) read)

view :: Queue a -> (a, Queue a)
view (Queue write (a : read')) = (a, check (Queue write read'))
