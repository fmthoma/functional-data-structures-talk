Purely Functional Data Structures
=================================

## Lazy Evaluation

### Linked Lists

```haskell
data List a = Nil | Cons a (List a)
```

```
  x  :  []           x  :  y  :  z  :  []

┌───┐              ┌───┐ ┌───┐ ┌───┐
│ x ├───[]         │ x ├─│ y ├─│ z ├───[]
└───┘              └───┘ └───┘ └───┘
```


### Lazy Evaluation

```
repeat a = a : repeat a

╭──────────╮
│ repeat a │
╰──────────╯
┌───┐ ╭──────────╮
│ a ├─│ repeat a │
└───┘ ╰──────────╯
┌───┐ ┌───┐ ╭──────────╮
│ a ├─│ a ├─│ repeat a │
└───┘ └───┘ ╰──────────╯

         ...
```


```java
// public static <A> A head(List<A> list) { … }

head(asList(f(1), f(2), f(3)))
```

Evaluation order:
1. Evaluate `f(1)`, `f(2)`, `f(3)`
2. Pass the result to `asList(•,•,•)`, evaluate
3. Pass the result to `head(•)`, evaluate

```haskell
head :: [a] -> a
head (a : as) = a

head [f 1, f 2, f 3]
```

Evaluation order:
1. Enter head function, pass `[f 1, f 2, f 3]` (`f 1 : f 2 : f 3 : []`) as thunk
2. Encounter pattern `(a : as)`
3. Evaluate thunk to WHNF (`_ : _`)
4. Return `f 1` as thunk


## Amortization

For algorithms with varying cost per step, worst-case bounds can be too
pessimistic, and the average case may not be an upper bound.

Goal: Find a better upper bound by balancing between different cost centers.


### The banker's and the Physicist's method

#### Banker's method:

* Place credits at each location in the data structure.
* Cheap operations may add credits (making them slightly more expensive).
* Expensive operations may consume credits (making them possibly cheaper).
* Credits can only be consumed after they have been placed, not in advance.

The goal is to find an accounting scheme where each expensive operation is
already paid for in credits.

#### Physicist's method:

--- FIXME


### Example for amortization: Array Lists

An Array List of fixed size `n` has `O(1)` insert.

```
                          7
                          ↓
┌───┬───┬───┬───┬───┬───┬───┬───┐
│ 1 │ 2 │ 3 │ 4 │ 5 │ 6 │   │   │
└───┴───┴───┴───┴───┴───┴───┴───┘
```

But if the list is too small, the `n+1`st element takes `O(n)` to insert,
because the entire list is copied to double the array size.

```
┌───┬───┬───┬───┬───┬───┬───┬───┐
│ 1 │ 2 │ 3 │ 4 │ 5 │ 6 │ 7 │ 8 │
└───┴───┴───┴───┴───┴───┴───┴───┘ 9
  ↓   ↓   ↓   ↓   ↓   ↓   ↓   ↓   ↓ 
┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐
│ 1 │ 2 │ 3 │ 4 │ 5 │ 6 │ 7 │ 8 │   │   │   │   │   │   │   │   │
└───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┘
```

Worst-case time for insert: `O(n)`
Amortized  time for insert: `O(1)`!


### Array List insertion: Amortized Analysis

Account three credits for inserting element `m` into a list of length `N`:
* One paid directly for actually storing the element
* One saved for copying it to the next larger array
* One saved for copying element `m - 2^(N-1)` to the next larger array

```
┌───┬───┬───┬───┬───┬───┬───┬───┐
│ 1*│ 2*│ 3 │ 4 │ 5*│ 6*│   │   │
└───┴───┴───┴───┴───┴───┴───┴───┘
┌───┬───┬───┬───┬───┬───┬───┬───┐
│ 1*│ 2*│ 3*│ 4 │ 5*│ 6*│ 7*│   │
└───┴───┴───┴───┴───┴───┴───┴───┘
┌───┬───┬───┬───┬───┬───┬───┬───┐
│ 1*│ 2*│ 3*│ 4*│ 5*│ 6*│ 7*│ 8*│
└───┴───┴───┴───┴───┴───┴───┴───┘
┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐
│ 1*│ 2 │ 3 │ 4 │ 5 │ 6 │ 7 │ 8 │ 9*│   │   │   │   │   │   │   │
└───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┘
```

When reaching the size limit, we have exactly one credit per cell to be copied.
In other words, the resizing operation has already been paid for!

With a constant three credits per insert operation, we have constant-time
insert.


### Haskell Example: Batched Queue

```haskell
data Queue a = Queue ![a] ![a]

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

```
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
```

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
            
test queue = insert (peek queue) queue
```

In order to `view` the first element, `peek` potentially reverses the list,
spending all the saved credits, but immediately throws away the result. Then
`test` continues with the old queue and inserts the result.

If anyone `view`s the result of `test`, the queue will be reversed *again* - but
the credits have already been spent!


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


### Example: Banker's Queue

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

```
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
```

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



## More Data Structures

-- TODO: Data.Set/Data.Map might be interesting

### Binomial Heap

-- FIXME the spine of the list of trees should be strict, too

```haskell
-- | * The 'forest' of a 'Tree' of 'rank' r contains exactly r trees of 'rank' r-1, …, 0
--   * A 'Tree' of 'rank' n contains exactly 2^n elements
--   * Elements are in heap order (head is the smallest element)
data Tree a = Node
    { rank   :: !Int
    , root   :: a
    , forest :: ![Tree a] }

link :: Ord a => Tree a -> Tree a -> Tree a
link left right
    | rank left /= rank right = error "Only link trees of equal rank"
    | root left <= root right = Node rank' (root left)  (right : forest left)
    | otherwise               = Node rank' (root right) (left  : forest right)
  where rank' = rank left + 1
```

    ┌───┐
    │ 1 │
    └─┬─┘ ┌───┐                         ┌───┐          ┌───┐
      └───│ 2 ├─────────────────────────│ 6 ├──────────│ 8 ├─[]
          └─┬─┘ ┌───┐          ┌───┐    └─┬─┘ ┌───┐    └─┬─┘
            └───│ 3 ├──────────│ 5 ├─[]   └───│ 7 ├─[]   └───[]
                └─┬─┘ ┌───┐    └─┬─┘          └─┬─┘
                  └───│ 4 ├─[]   └───[]         └───[]
                      └─┬─┘
                        └───[]

          ╰─ rank 2 ──────────────────╯ ╰─ rank 1 ───╯ ╰─ r 0 ─╯

    ╰─ rank 4 ─────────────────────────────────────────────────╯


### Binomial Heap: Inserting Elements

```haskell
newtype Heap a = Heap [Tree a]

insert :: Ord a => a -> Heap a -> Heap a
insert a (Heap trees) = Heap (insertTree (Node 0 a []) trees)
  where insertTree s [] = [s]
        insertTree s (t : ts)
            | rank s < rank t  -- found a free spot
                = s : t : ts
            | otherwise -- must be equal rank (because of sorting) => carry
                = insertTree (link s t) ts
```

A binomial Heap is a list of trees of different `rank`, where some positions
might not be taken.

`insert`ing an element into a Binomial Heap is much like adding 1 to a binary
number: The 1's correspond to the taken positions, the 0's to open positions.

    10010 + 1 = 10011
    10011 + 1 = 10100  <- carry twice!


### Binomial Heap: Merging Trees

```haskell
merge :: Ord a => Heap a -> Heap a -> Heap a
merge (Heap left) (Heap right) = Heap (mergeTrees left right)
  where mergeTrees left   []    = left
        mergeTrees []     right = right
        mergeTrees (l:ls) (r:rs)
            | rank l > rank r  = l : mergeTrees ls (r:rs)
            | rank l < rank r  = r : mergeTrees (l:ls) rs
            | otherwise        = link l r : mergeTrees ls rs
```

### Binomial Heap: Decomposition

```haskell
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
```


### Binomial Heap: Amortized Analysis

--- FIXME


### Skew Binomial Heap

Used in: `Data.Heap` in Kmett's `heaps`

### Finger Tree

Used in: `Data.Sequence`

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
instance Sized a => Sized (Node a)
```


### Finger Tree: Visual

```
FingerTree a                                                          1..8 items
                                  ┌─┬─┬─┬─┐     ┌─┬─┬─┐
                                  │a│b│c│d├──┬──┤X│Y│Z│
                                  └─┴─┴─┴─┘  │  └─┴─┴─┘
FingerTree (Node a)                          │                       2..24 items
                  ┌───────┬───────┬───────┐  │  ┌───────┬─────┐
                  │┌─┬─┬─┐│┌─┬─┬─┐│┌─┬─┬─┐│  │  │┌─┬─┬─┐│┌─┬─┐│
                  ││e│f│g│││h│i│j│││k│l│m│├──┼──┤│S│T│U│││V│W││
                  │└─┴─┴─┘│└─┴─┴─┘│└─┴─┴─┘│  │  │└─┴─┴─┘│└─┴─┘│
                  └───────┴───────┴───────┘  │  └───────┴─────┘
                                             │
FingerTree (Node (Node a))                   │                       4..72 items
      ┌─────────────────────┬─────────────┐  │  ┌─────────────────┐
      │┌───────┬─────┬─────┐│┌─────┬─────┐│  │  │┌───────┬───────┐│
      ││┌─┬─┬─┐│┌─┬─┐│┌─┬─┐│││┌─┬─┐│┌─┬─┐││  │  ││┌─┬─┬─┐│┌─┬─┬─┐││
      │││n│o│p│││q│r│││s│t│││││u│v│││w│x││├──┼──┤││M│N│O│││P│Q│R│││
      ││└─┴─┴─┘│└─┴─┘│└─┴─┘│││└─┴─┘│└─┴─┘││  │  ││└─┴─┴─┘│└─┴─┴─┘││
      │└───────┴─────┴─────┘│└─────┴─────┘│  │  │└───────┴───────┘│
      └─────────────────────┴─────────────┘  │  └─────────────────┘
                                             │
FingerTree (Node (Node (Node a)))            │                      8..216 items
                     ┌───────────────────────┴───────────────────────┐
                     │┌─────────────┬───────────────┬───────────────┐│
                     ││┌─────┬─────┐│┌─────┬───────┐│┌─────┬───────┐││
                     │││┌─┬─┐│┌─┬─┐│││┌─┬─┐│┌─┬─┬─┐│││┌─┬─┐│┌─┬─┬─┐│││
                     ││││y│z│││A│B│││││C│D│││E│F│G│││││H│I│││J│K│L││││
                     │││└─┴─┘│└─┴─┘│││└─┴─┘│└─┴─┴─┘│││└─┴─┘│└─┴─┴─┘│││
                     ││└─────┴─────┘│└─────┴───────┘│└─────┴───────┘││
                     │└─────────────┴───────────────┴───────────────┘│
                     └───────────────────────────────────────────────┘
```


### Finger Tree: Inserting an Element

```haskell
cons :: Sized a => a -> FingerTree a -> FingerTree a
cons a Empty = Single a
cons a (Single b) = Deep (One a) Empty (One b)
cons a (Deep s l t r) = case l of
    One   b       -> Deep s' (Two   a b)     t                      r
    Two   b c     -> Deep s' (Three a b c)   t                      r
    Three b c d   -> Deep s' (Four  a b c d) t                      r
    Four  b c d e -> t `seq`
                     Deep s' (Two   a b)     (Node3 c d e `cons` t) r
  where s' = s + size a
```

* `cons` tries to pack as tighly as possible (always creates `Node3`)
* Always keeps two elements on either side, if possible (for fast `view`)
* `snoc` works exactly the same

`cons` performs in amortized `O(1)`: The »carry« (pushing a `Node` down the
spine) -- FIXME


### Finger Tree: Decomposition

```haskell
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
```

* If the left side would become empty, recursively fetch a `Node` from the spine.
* If the spine is empty, try to fetch nodes from the right.

`viewL` performs in amortized `O(1)`: -- FIXME


### Finger Tree: Splitting and appending

`split :: Sized a => Int -> FingerTree a -> (FingerTree a, FingerTree a)` uses the
size annotation to decide which subtree/digit/node to split.

`O(log n)`: -- FIXME

`(><) :: FingerTree a -> FingerTree a -> FingerTree a` creates `Node`s from
adjacent `Digit`s and pushes them up the spine.

`O(log n)`: -- FIXME


### Finger Tree: Applications

* Sequences (`Data.Seq`), Queues: `O(log n)` random access, `O(1)` access to
  both ends, O(log n) `append`.
* (Fair) Priority Queues: Replace the Size annotation by a Priority annotation.
  `O(log n)` insert and access to the minimum element.
  
Despite the good asymptotic properties, Finger Trees are quite slow.
* For Sequences, Lists are generally preferable, unless random access or access
  to both ends are explicitly required.
* For Priority Queues, Heaps generally perform much better, but they are usually
  not stable (fair).

### End
