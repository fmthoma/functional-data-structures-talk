Purely Functional Data Structures
=================================

## Linked Lists

```haskell
-- Builtin syntax, but would be defined like that
data [a]           -- [a] = »List of items of type `a`«
    = []           -- Pronounced »Nil«: empty list
    | a : List a   -- Pronounced »Cons«: Prepends an element to a List
```

Syntactical sugar:

    [1,2,3] ≡ 1 : (2 : (3 : []))

    [1..10] ≡ 1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 : 10 : []

Pattern Matching (list deconstruction):

```haskell
(++) :: [a] -> [a] -> [a]
[]   ++ ys = ys
x:xs ++ ys = x : (xs ++ ys)
```

### Memory Layout

      x  :  []           x  :  y  :  z  :  []

    ┌───┐              ┌───┐ ┌───┐ ┌───┐
    │ x ├───[]         │ x ├─│ y ├─│ z ├───[]
    └───┘              └───┘ └───┘ └───┘

Each `Cons` cell contains an element, and a pointer to the next element.

... if the list is evaluated at all, because:


### Lazy Evaluation

Haskell is lazy:
* Expressions are only evaluated when the result is needed
* ... and only as far as needed!
* Results are *memoized*, i.e. only computed once.
* To be precise: Evaluation is driven by Pattern Matching.

This allows for infinite constructs like repeat:

```haskell
replicate 0 x = []
replicate n x = x : replicate (n - 1) x
```

The function creates a so-called »Thunk«, a reference to a computation to be
executed:

    ╭───────────────╮
    │ replicate 2 x │
    ╰───────────────╯

After matching on the first `Cons` cell, the memory is updated (memoization):

    ┌───┐ ╭───────────────╮
    │ x ├─│ replicate 1 x │
    └───┘ ╰───────────────╯

... and the second `Cons` cell:

    ┌───┐ ┌───┐ ╭───────────────╮
    │ x ├─│ x ├─│ replicate 0 x │
    └───┘ └───┘ ╰───────────────╯

The next pattern-match finds the final `Nil` cell:

    ┌───┐ ┌───┐
    │ x ├─│ x ├───[]
    └───┘ └───┘


### Efficiency of Lists

Lists are quite inefficient when compiled naively.

But laziness allows **Stream Fusion**, which basically allows the compiler to
rearrange and group list operations, so that often a list is never even written
to memory, but produces and transform elements as they are consumed.

```haskell
fac :: Int -> Int
fac n = product [1..n]
```

Q: How many memory allocations does `fac 100` perform?

A: None at all: The numbers are produced incrementally, as they are consumed by
`product`. No memory allocation required.


## How Lazy can you be?

Assume the Java equivalent of the `head` function:

```java
// public static <A> A head(List<A> list) { … }

head(asList(f(100), f(200), f(300)))
```

In what order are the calls evaluated?

1. Evaluate `f(100)`, `f(200)`, `f(300)`
2. Pass the result to `asList(•,•,•)`, evaluate
3. Pass the result to `head(•)`, evaluate


### Let's Procrastinate!

```haskell
head :: [a] -> a
head (a : as) = a

head [f 100, f 200, f 300]
```

In what order are the calls evaluated?

1. Enter head function, pass `[f 100, f 200, f 300]` as thunk
2. Encounter pattern `(a : as)`
3. Evaluate thunk to Weak Head Normal Form (WHNF: `• : •`)
4. Return `f 100` as thunk

We didn't even once call `f`!

(Now think what would happen for `f n = f (n-1) + f(n-2)` ...)



## Amortization

For algorithms with varying cost per step, worst-case bounds can be too
pessimistic, and the average case may not be an upper bound.

Goal: Find a better upper bound by balancing between different cost centers.


### Example for amortization: Array Lists

An Array List of fixed size `n` has `O(1)` insert.

                              7
                              ↓
    ┌───┬───┬───┬───┬───┬───┬───┬───┐
    │ 1 │ 2 │ 3 │ 4 │ 5 │ 6 │   │   │
    └───┴───┴───┴───┴───┴───┴───┴───┘

But if the list is too small, the `n+1`st element takes `O(n)` to insert,
because the entire list is copied to double the array size.

    ┌───┬───┬───┬───┬───┬───┬───┬───┐
    │ 1 │ 2 │ 3 │ 4 │ 5 │ 6 │ 7 │ 8 │
    └───┴───┴───┴───┴───┴───┴───┴───┘ 9
      ↓   ↓   ↓   ↓   ↓   ↓   ↓   ↓   ↓
    ┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐
    │ 1 │ 2 │ 3 │ 4 │ 5 │ 6 │ 7 │ 8 │   │   │   │   │   │   │   │   │
    └───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┘

* Worst-case time for insert: `O(n)`
* Amortized  time for insert: `O(1)`!


### The banker's and the Physicist's method

#### Banker's method:

* Place credits at each location in the data structure.
* Cheap operations may add credits (making them slightly more expensive).
* Expensive operations may consume credits (making them possibly cheaper).
* Credits can only be consumed after they have been placed, not in advance.

The goal is to find an accounting scheme where each expensive operation is
already paid for in credits.

#### Physicist's method:

* Define a potential `Φ` based on a property of each location.
* The total potential is an upper bound for the accumulated savings, it must
  never be negative.
* The amortized cost of an operation is the actual cost plus the change in
  potential:
    * Expensive operations can be made cheaper by drawing from the potential,
    * cheap operations can add to the potential.

The goal is to find a good definition for the potential, so that it will never
become negative, and so that expensive operations may draw from it.

Both methods are equivalent:
* Proofs using the Physicist's method are easier to write, because we can ignore
  the locations of the credits.
* Proofs using the Banker's method are easier to understand, because we known
  when and where the credits are placed and consumed.


### Array List insertion: Amortized Analysis

    ┌───┬───┬───┬───┬───┬───┬───┬───┐
    │ 1*│ 2*│ 3 │ 4 │ 5*│ 6*│   │   │
    └───┴───┴───┴───┴───┴───┴───┴───┘
    ┌───┬───┬───┬───┬───┬───┬───┬───┐
    │ 1*│ 2*│ 3*│ 4 │ 5*│ 6*│ 7*│   │
    └───┴───┴───┴───┴───┴───┴───┴───┘
    ┌───┬───┬───┬───┬───┬───┬───┬───┐
    │ 1*│ 2*│ 3*│ 4*│ 5*│ 6*│ 7*│ 8*│
    └───┴───┴───┴───┴───┴───┴───┴───┘ 9
    ┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐
    │ 1 │ 2 │ 3 │ 4 │ 5 │ 6 │ 7 │ 8 │   │   │   │   │   │   │   │   │
    └───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┘
    ┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐
    │ 1*│ 2 │ 3 │ 4 │ 5 │ 6 │ 7 │ 8 │ 9*│   │   │   │   │   │   │   │
    └───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┘

Account three credits for inserting element `m` into a list of length `N`:
* One paid directly for actually storing the element
* One saved for copying it to the next larger array
* One saved for copying element `m - 2^(N-1)` to the next larger array

When reaching the size limit, we have exactly one credit per cell to be copied.
In other words, the resizing operation has already been paid for!

With a constant three credits per insert operation, we have constant-time
insert.


### Haskell Example: Batched Queue

```haskell
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
```

Inserting an element always takes `O(1)`, but reversing the front end of the
queue takes `O(n)`!
=> The worst-case complexity for `view` is `O(n)`.

Can we do better?


### Amortization for Batched Queue

Use the Banker's method:

                                                        ┌───┐
    insert 1:       []                                  │ 1 ├───[]
                                                        └───┘
                    ┌───┐                               ┌───┐
    insert 2:       │ 2*├───[]                          │ 1 ├───[]
                    └───┘                               └───┘
                    ┌───┐ ┌───┐                         ┌───┐
    insert 3:       │ 3*├─│ 2*├───[]                    │ 1 ├───[]
                    └───┘ └───┘                         └───┘
                                                        ┌───┐ ┌───┐
    view:           []                                  │ 2 ├─│ 3 ├───[]
                                                        └───┘ └───┘
                    ┌───┐                               ┌───┐ ┌───┐
    insert 4:       │ 4*├───[]                          │ 2 ├─│ 3 ├───[]
                    └───┘                               └───┘ └───┘
                    ┌───┐                               ┌───┐
    view:           │ 4*├───[]                          │ 3 ├───[]
                    └───┘                               └───┘

* On `insert`, account one credit to each inserted item, to a total cost of 2.
* `view` without reversing does not consume any credits, so the total cost is 1.
* `view` with reversing costs `n+1` steps, consumes `n` credits, so the net
  total cost is 1.

=> Batched Queue has `O(1)` amortized `view` and `insert`.

Does it really?


### Problem with Amortization: Persistence

Immutable data structures are **persistent**, they can be reused. This breaks
our accounting balance:

```haskell
peek :: Queue a -> a
peek q = case view q of (a, _) -> a

enqueueHead queue = insert (peek queue) queue
```

In order to `view` the first element, `peek` potentially reverses the list,
spending all the saved credits, but immediately throws away the updated queue.
Then `enqueueHead` continues with the old queue and `insert`s the result.

If anyone `view`s the result of `enqueueHead`, the queue will be reversed
*again* - but the credits have already been spent!


### Laziness and Amortization

Idea: Use lazy evaluation, in particular memoization, to guarantee credits are
only spent once.

Lazy amortization using the Banker's method is a *layaway plan*:

* Each time a thunk is created, we assign it a number of credits proportional to
  the time it takes to evaluate it.
* We commit to save credits on each subsequent operation to pay for the
  evaluation.
* Goal is to prove that in each possible future, there are enough saved credits
  to pay for the thunk when it is evaluated.



## Banker's Queue

```haskell
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
```


### Amortization for Banker's Queue

                                                        ┌───┐
    insert 1:       []                                  │ 1 ├───[]
                                                        └───┘
                    ┌───┐                               ┌───┐
    insert 2:       │ 2 ├───[]                          │ 1 ├───[]
                    └───┘                               └───┘
                                                        ┌───┐ ╭───────────────────╮
    insert 3:       []                                  │ 1 ├─│ ** reverse [3, 2] │
                                                        └───┘ ╰───────────────────╯
                                                        ╭──────────────────╮
    view:           []                                  │ * reverse [3, 2] │
                                                        ╰──────────────────╯
                    ┌───┐                               ╭──────────────────╮
    insert 4:       │ 4 ├───[]                          │ * reverse [3, 2] │
                    └───┘                               ╰──────────────────╯
                    ┌───┐                               ┌───┐
    view:           │ 4 ├───[]                          │ 3 ├───[]
                    └───┘                               └───┘

#### `insert`:

* Trivial `insert` takes one credit
* `insert` with `reverse` takes one credit, and creates a thunk with `lenWrite`
  credits

=> `insert` runs in `O(1)`

#### `view`:

* `view` costs one credit, and we save one credit to pay for the next thunk.
* Condition for rotating the queue: `lenWrite > lenRead`. => When reaching the
  thunk of a `reverse` operation, we have saved at least `lenRead` credits to
  pay for the thunk.

=> `view` runs in amortized `O(1)`


### Constant Overhead

The Banker's Queue is slightly slower than the Batched Queue because of the
additional book-keeping. Hence, in reality the Batched Queue is preferred where
possible.

Example: `Control.Concurrent.STM.TQueue` from `stm` uses a Batched Queue, since
the queue is not used persistently, but ephemerally.



## Finger Trees

[Ralf Hinze and Ross Paterson, »Finger trees: a simple general-purpose data
structure«, Journal of Functional Programming 16:2 (2006).
](http://www.staff.city.ac.uk/~ross/papers/FingerTree.pdf)

Used in: `Data.Sequence`

    FingerTree a                             •                        1..8 items
                                   ┌─┬─┬─┬───┼───┬─┬─┐
                                   a b c d   │   X Y Z
                                             │
    FingerTree (Node a)                      │                       2..24 items
                       ┌───────┬───────┬─────┼─────┬──────┐
                     ┌─┼─┐   ┌─┼─┐   ┌─┼─┐   │   ┌─┼─┐   ┌┴┐
                     e f g   h i j   k l m   │   S T U   V W
                                             │
    FingerTree (Node (Node a))               │                       4..72 items
                     ┌───────────────┬───────┼─────────┐
              ┌──────┼──────┐     ┌──┴──┐    │     ┌───┴───┐
            ┌─┼─┐   ┌┴┐    ┌┴┐   ┌┴┐   ┌┴┐   │   ┌─┼─┐   ┌─┼─┐
            n o p   q r    s t   u v   w x   │   M N O   P Q R
                                             │
    FingerTree (Node (Node (Node a)))        │                      8..216 items
                                ┌────────────┼────────────┐
                             ┌──┴──┐     ┌───┴──┐      ┌──┴───┐
                            ┌┴┐   ┌┴┐   ┌┴┐   ┌─┼─┐   ┌┴┐   ┌─┼─┐
                            y z   A B   C D   E F G   H I   J K L


### Finger Tree: Data Types

```haskell
data FingerTree a
    = Empty
    | Single a
    | Deep !Int !(Digit a) (FingerTree (Node a)) !(Digit a)

data Digit a = One a | Two a a | Three a a a | Four a a a a

data Node a = Node2 a a | Node3 a a a

class Sized a where
    size :: Int

instance Sized a => Sized (FingerTree a)
instance Sized a => Sized (Digit a)
instance Sized a => Sized (Node a) where
    size (Node2 a b) = size a + size b
    size (Node3 a b c) = size a + size b + size c
```


### Finger Tree: Inserting Elements

```haskell
(<|)  -- also called `cons`
    :: Sized a => a -> FingerTree a -> FingerTree a
(|>)  -- also called `snoc` (`cons` backwards)
    :: Sized a => FingerTree a -> a -> FingerTree a

a <| Single b = Deep (One a) Empty (One b)
a <| Deep s l t r = case l of
    One   b       -> Deep s' (Two   a b)     t                  r
    Two   b c     -> Deep s' (Three a b c)   t                  r
    Three b c d   -> Deep s' (Four  a b c d) t                  r
    Four  b c d e -> t `seq` -- Push a node down the spine
                     Deep s' (Two   a b)     (Node3 c d e <| t) r
  where s' = s + size a
```


### Building a Tree

    1 <| 2 <| 3 <| 4 <| 5 <| 6 <| 7 <| 8 <| 9 <| Empty


### Building a Tree

    1 <| 2 <| 3 <| 4 <| 5 <| 6 <| 7 <| 8 <| Single 9

                          •
                          |
                          9


### Building a Tree

    1 <| 2 <| 3 <| 4 <| 5 <| 6 <| 7 <| Deep (One 8) Empty (One 9)

                          •
                      ┌───┴───┐
                      8       9

`Single` is split into a `Deep` tree with `One` on each side and `Empty` spine.


### Building a Tree

    1 <| 2 <| 3 <| 4 <| 5 <| 6 <| Deep (Two 7 8) Empty (One 9)

                          •
                    ┌─┬───┴───┐
                    7 8       9


### Building a Tree

    1 <| 2 <| 3 <| 4 <| 5 <| Deep (Three 6 7 8) Empty (One 9)

                          •
                  ┌─┬─┬───┴───┐
                  6 7 8       9


### Building a Tree

    1 <| 2 <| 3 <| 4 <| Deep (Four 5 6 7 8) Empty (One 9)

                          •
                ┌─┬─┬─┬───┴───┐
                5 6 7 8       9

Elements are inserted into the left `Digit` until it reaches `Four`.


### Building a Tree

    1 <| 2 <| 3 <| Deep (Two 4 5) (Single (Node3 6 7 8)) (One 9)

                          •
                    ┌─┬───┼───┐
                    4 5   │   9
                        ┌─┼─┐
                        6 7 8

When the left `Digit` is full, three elements are pushed down the spine as a
`Single Node3`, leaving `Two` on the left `Digit`.


### Building a Tree

    1 <| 2 <| Deep (Three 3 4 5) (Single (Node3 6 7 8)) (One 9)

                          •
                  ┌─┬─┬───┼───┐
                  3 4 5   │   9
                        ┌─┼─┐
                        6 7 8

Now we can insert two more elements into the left `Digit`.

### Building a Tree

    1 <| Deep (Four 2 3 4 5) (Single (Node3 6 7 8)) (One 9)

                          •
                ┌─┬─┬─┬───┼───┐
                2 3 4 5   │   9
                        ┌─┼─┐
                        6 7 8


### Building a Tree

    Deep (Two 1 2) (Deep (One (Node3 3 4 5)) Empty (One (Node3 6 7 8))) (One 9)

                          •
                    ┌─┬───┼───┐
                    1 2   │   9
                      ┌───┴───┐
                    ┌─┼─┐   ┌─┼─┐
                    3 4 5   6 7 8

One more `Node3` is pushed down the spine, recursively splitting the `Single`
into a `Deep` with two `One`s of `Node3`s.


### Building a Tree


* `<|` tries to pack as tighly as possible (always creates `Node3`)
* Always keeps two elements on either side, if possible (for fast `view`)
* `|>` works exactly the same


### Finger Tree: Decomposition

```haskell
viewL :: Sized a => FingerTree a -> Maybe (a, FingerTree a)
viewR :: Sized a => FingerTree a -> Maybe (FingerTree a, a)

viewL Empty = Nothing
viewL (Single a) = Just (a, Empty)
viewL (Deep s l t r) = Just $ case l of
    Four  a b c d              -> (a, Deep (s - size a) (Three b c d) t     r)
    Three a b c                -> (a, Deep (s - size a) (Two   b c)   t     r)
    Two   a b                  -> (a, Deep (s - size a) (One   b)     t     r)

    One   a -> case viewL t of  -- Pull a node up from the spine
        Just (Node3 b c d, t') -> (a, Deep (s - size a) (Three b c d) t'    r)
        Just (Node2 b c,   t') -> (a, Deep (s - size a) (Two   b c)   t'    r)

        Nothing -> case r of    -- If the spine is empty, balance with the right digit
            Four  b c d e      -> (a, Deep (s - size a) (Two b c)     Empty (Two d e))
            Three b c d        -> (a, Deep (s - size a) (Two b c)     Empty (One d))
            Two   b c          -> (a, Deep (s - size a) (One b)       Empty (One c))
            One   b            -> (a, Single b)
```


### Viewing or Deleting Elements from a Tree

    Deep (Two 1 2) (Deep (One (Node3 3 4 5)) Empty (One (Node3 6 7 8))) (Two 9 10)

                          •
                    ┌─┬───┼───┬─┐
                    1 2   │   9 10
                      ┌───┴───┐
                    ┌─┼─┐   ┌─┼─┐
                    3 4 5   6 7 8


### Viewing or Deleting Elements from a Tree

    Deep (One 2) (Deep (One (Node3 3 4 5)) Empty (One (Node3 6 7 8))) (Two 9 10)

                          •
                      ┌───┼───┬─┐
                      2   │   9 10
                      ┌───┴───┐
                    ┌─┼─┐   ┌─┼─┐
                    3 4 5   6 7 8


### Viewing or Deleting Elements from a Tree

    Deep (Three 3 4 5) (Single (Node3 6 7 8)) (Two 9 10)

                          •
                  ┌─┬─┬───┼───┬─┐
                  3 4 5   │   9 10
                        ┌─┼─┐
                        6 7 8

If the left `Digit` would become empty, recursively fetch a `Node` from the spine.


### Viewing or Deleting Elements from a Tree

    Deep (Two 4 5) (Single (Node3 6 7 8)) (Two 9 10)

                          •
                    ┌─┬───┼───┬─┐
                    4 5   │   9 10
                        ┌─┼─┐
                        6 7 8


### Viewing or Deleting Elements from a Tree

    Deep (One 5) (Single (Node3 6 7 8)) (Two 9 10)

                          •
                      ┌───┼───┬─┐
                      5   │   9 10
                        ┌─┼─┐
                        6 7 8


### Viewing or Deleting Elements from a Tree

    Deep (Three 6 7 8) Empty (Two 9 10)

                          •
                  ┌─┬─┬───┴───┬─┐
                  6 7 8       9 10

Fetch one more `Node` from the sping, leaving the spine `Empty`.


### Viewing or Deleting Elements from a Tree

    Deep (Two 7 8) Empty (Two 9 10)

                          •
                    ┌─┬───┴───┬─┐
                    7 8       9 10


### Viewing or Deleting Elements from a Tree

    Deep (One 8) Empty (Two 9 10)

                          •
                      ┌───┴───┬─┐
                      8       9 10


### Viewing or Deleting Elements from a Tree

    Deep (One 9) Empty (One 10)

                          •
                      ┌───┴───┐
                      9       10

If the spine is empty, try to fetch nodes from the right.

### Viewing or Deleting Elements from a Tree

    Single 10

                          •
                          │
                          10


### Viewing or Deleting Elements from a Tree

* Never leaves four elements on either side, for fast subsequent `<|`.
* `viewR` works exactly the same.

`viewL` performs in amortized `O(1)`.


### Finger Tree: Amortization

* Rewriting a `Deep` with its (strict) `Digit`s costs one credit.
* Every access to the spine (push down/pull up) costs one credit.

Access to the spine only happens for `One` and `Four`. Those `Digit`s are called
*dangerous*, the others are *safe*.

Note that every access to a dangerous digit makes it safe:
* Adding/removing elements: `One` -> `Two`, `Four` -> `Three`
* Accessing the spine: `One` -> `Three`/`Two`, `Four` -> `Two`


### Finger Tree: Layaway plan

#### Pushing a node down the spine:

* Costs one credit for writing the `Two`.
* Each recursion creates a thunk that will eventually cost one credit.
* Each recursion turns a dangerous digit into a safe one.

#### Pulling a node from the spine:

* Accessing the `Digit` costs one credit.
* Each recursion forces a thunk in the spine, which must already be paid for.
* Each recursion turns a dangerous digit into a safe one.

#### Layaway Plan

Charge two credits per `cons` and `viewL`: One to rewrite the `Digit`, and one
to pay the debt of a spine thunk.

**Invariant:** Every time a digit becomes dangerous, the debt for the spine
thunk is resolved.

Inductive proof:
* **Step:** When recursing down the spine, assuming its debt has been paid, we
  use our extra credit to pay for the spine of the first safe `Digit` we
  encounter. All the `Digit`s on the way are now safe.
* **Initial:** When creating a spine, we immediately use the extra credit to
  resolve its debt.

=> Spines of dangerous `Digit`s are guaranteed to be paid for, spines of safe
`Digit`s may have open debt.

=> Each time we access the spine, the debt has already been paid!


### Finger Tree: Splitting and appending

#### Splitting

`split :: Sized a => Int -> FingerTree a -> (FingerTree a, FingerTree a)` uses
the size annotation to decide which subtree/digit/node to split.

`O(log n)`: On each level, we create two digits and two lazy spines.

#### Merging

`merge :: FingerTree a -> FingerTree a -> FingerTree a` creates `Node`s from
adjacent `Digit`s and pushes them up the spine.

`O(log n)`: On each level, we merge two digits, and push at most four nodes up
the spine.

#### Random Access

`lookup :: Sized a => Int -> FingerTree a -> Maybe a` looks up the `i`-th
element by splitting the tree at `i` and returning the minimum element of the
right tree.

`O(log n)`: It's just a `split` plus a constant time operation.


### Finger Tree: Applications

* Sequences (`Data.Sequence`), Queues: `O(log n)` random access, `O(1)` access to
  both ends, O(log n) `append`.

* (Fair) Priority Queues: Replace the Size annotation by a Priority annotation.
  `O(log n)` insert and access to the minimum element.


## Other Widely-Used Functional Data Structures

### Size-Balanced Binary Trees

Used in `Data.Set` and `Data.Map` in `containers`.

* `Map`s (`Set`s) use the `Ord` instance of the keys (elements) to maintain
  ordering for `O(log n)` worst-case insert and lookup.
* To prevent degeneration to `O(n)` in pathological cases, two subtrees are
  rebalanced if their sizes differ by a factor greater than 3.

```haskell
data Map k v
    = Bin { size  :: !Int
          , key   :: !k
          , root  :: v
          , left  :: !(Map k v)
          , right :: !(Map k v) }
    | Tip
```

### Skew Binomial Heaps

Used in `Data.Heap` in Edward Kmett's `heaps`.

* A variant of Binomaial Heaps with worst-case instead of amortized bounds.
* Used as Priority Queues
* Same asymptotic bounds as Finger Trees, slightly faster, but not stable (fair).

```haskell
data Heap a
    = Empty
    | Tree { rank :: !Int
           , root :: a
           , forest :: [Heap a]
           -- ^ Zero or one tree of each rank smaller than this tree's rank,
           -- ordered by increasing rank. First two trees may have the same rank,
           -- to limit number of carries per operation.
           }
```



## Conclusion

* Immutability does not prevent efficient data structures.
* Smart compilers make stupid programs run fast.
* Being lazy can save you a lot of work.



## Thank you!
