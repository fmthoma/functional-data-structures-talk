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


# ## Array List insertion: Amortized Analysis

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



## Binomial Heaps

### Binomial Trees

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

               ╰─ rank 1 ───╯ ╰─ r 0 ─╯

          ╰─ rank 2 ──────────────────╯ ╰─ rank 1 ───╯ ╰─ r 0 ─╯

    ╰─ rank 3 ─────────────────────────────────────────────────╯


### Binomial Heaps

```haskell
newtype Heap a = Heap [Tree a]
```

A binomial Heap is a list of trees of different `rank`, where some positions
might not be taken.

`insert`ing an element into a Binomial Heap is much like adding 1 to a binary
number: The 1's correspond to the taken positions, the 0's to open positions.

    10010 + 1 = 10011
    10011 + 1 = 10100  <- carry twice!

For amortization, we maintain that that there is alway one credit on each tree
in the heap.


### Binomial Heap: Inserting Elements

```haskell
insert :: Ord a => a -> Heap a -> Heap a
insert a (Heap trees) = Heap (insertTree (Node 0 a []) trees)
  where insertTree s [] = [s]
        insertTree s (t : ts)
            | rank s < rank t  -- found a free spot
                = s : t : ts
            | otherwise -- must be equal rank (because of sorting) => carry
                = insertTree (link s t) ts
```

`insert` costs 2 credits:

* one spent directly for inserting the tree
* one left at the root
* When a carry occurs, two trees are `link`ed, each with one credit at the
  root. Use one of the credits to pay for linking, the other one is placed
  at the root of the resulting tree.

=> amortized `O(1)` insert


### Binomial Heap: Merging Heaps

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

`merge` costs `min(t₁, t₂)` credits, where `tᵢ` is the number of trees in the
`i`-th heap:

* Merging a tree into an empty spot costs 1 credit
* Linking two trees (carry) costs one credit (paid for by taking the credit
  at the root of one of the trees), and the resulting tree is inserted
  recursively (at the cost of one credit).

=> since `tᵢ` is bounded by `2(log₂ nᵢ)`, `merge` runs in `O(log n)`.


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

`viewMin` costs at most `3(log₂ n)` credits:

* one per tree in the heap for finding the minimum `root` by linearly
  traversing the trees
* one per tree in the `forest` of the min tree for reversing the `forest` of
  the extracted tree
* one per tree in the heap for mering the extracted tree into the heap.

=> `viewMin` runs in `O(log n)`.


## Skew Binomial Heaps

Brodal/Okasaki, 1996

Used in: `Data.Heap` in Edward Kmett's `heaps`

Problem: Binomial Heap has still amortized bounds. What if we want worst-case
bounds?

### Skew Binary Numbers

Skew binary numbers are incremented like binary numbers, but the lowest non-zero
digit may be incremented to `2`.

    ┌─────────┬─────────────────────────┬─────────────────────┐
    │ Decimal │ Ordinary binary numbers │ Skew binary numbers │
    ╞═════════╪═════════════════════════╪═════════════════════╡
    │       1 │                       1 │                   1 │
    │       2 │ 1x carry ->          10 │                   2 │
    │       3 │                      11 │ 1x carry ->      10 │
    │       4 │ 2x carry ->         100 │                  11 │
    │       5 │                     101 │                  12 │
    │       6 │ 1x carry ->         110 │ 1x carry ->      20 │
    │       7 │                     111 │ 1x carry ->     100 │
    │       8 │ 3x carry ->        1000 │                 101 │
    │       9 │                    1001 │                 102 │
    │      10 │ 1x carry ->        1010 │ 1x carry ->     110 │
    │      11 │                    1011 │                 111 │
    │      12 │ 2x carry ->        1100 │                 112 │
    │      13 │                    1101 │ 1x carry ->     120 │
    │      14 │ 1x carry ->        1110 │ 1x carry ->     200 │
    │      15 │                    1111 │ 1x carry ->    1000 │
    │      16 │ 4x carry ->       10000 │                1001 │
    ├─────────┼─────────────────────────┼─────────────────────┤
    │         │ 15 carries              │ 7 carries           │
    └─────────┴─────────────────────────┴─────────────────────┘

To reach a number `n`, we need up to `n-1` carries, and each increment
potentially takes up to `log₂ n` carries.

In Skew binary numbers, each increment takes at most one carry, so to reach a
number `n`, we need at most `n` carries!

Since inserting into a Binomial Heap coresponds to incrementing the binary
representation of the number of elements, `insert` into a Skew Binomial Heap
takes *worst-case* constant time.


### Skew Binomial Heap: Implementation

```haskell
-- FIXME
```



## Finger Trees

[Ralf Hinze and Ross Paterson, »Finger trees: a simple general-purpose data
structure«, Journal of Functional Programming 16:2 (2006).
](http://www.staff.city.ac.uk/~ross/papers/FingerTree.pdf)

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
    Four  b c d e -> t `seq` -- Push a node up the spine
                     Deep s' (Two   a b)     (Node3 c d e `cons` t) r
  where s' = s + size a
```

* `cons` tries to pack as tighly as possible (always creates `Node3`)
* Always keeps two elements on either side, if possible (for fast `view`)
* `snoc` works exactly the same

`cons` performs in amortized `O(1)`.


### Finger Tree: Decomposition

```haskell
viewL :: Sized a => FingerTree a -> Maybe (a, FingerTree a)
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

* If the left side would become empty, recursively fetch a `Node` from the spine.
* If the spine is empty, try to fetch nodes from the right.

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

**Invariant:** Every time a digit becomes dangerous, the debt for the spine
thunk must be resolved.

#### Pushing a node up the spine:

```haskell
cons a (Deep s l t r) = case l of
    ...
    Four  b c d e -> t `seq` -- Push a node up the spine
                     Deep s' (Two a b) (Node3 c d e `cons` t) r
```

* Costs one credit for writing the `Two`.
* Each recursion creates a thunk that will eventually cost one credit.
* Each recursion turns a dangerous digit into a safe one.

#### Pulling a node from the spine:

```haskell
viewL (Deep s l t r) = Just $ case l of
    ...
    One a -> case viewL t of  -- Pull a node down from the spine
        Just (Node3 b c d, t') -> (a, Deep (s - size a) (Three b c d) t' r)
        Just (Node2 b c,   t') -> (a, Deep (s - size a) (Two   b c)   t' r)
    ...
```

* Accessing the `Digit` costs one credit.
* Each recursion forces a thunk in the spine, which must already be paid for.
* Each recursion turns a dangerous digit into a safe one.

#### Layaway Plan

Charge two credits per `cons` and `viewL`: One to rewrite the `Digit`, and one
to pay the debt of a spine thunk.
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

`split :: Sized a => Int -> FingerTree a -> (FingerTree a, FingerTree a)` uses the
size annotation to decide which subtree/digit/node to split.

`O(log n)`: On each level, we create two digits and two lazy spines.

#### Merging

`merge :: FingerTree a -> FingerTree a -> FingerTree a` creates `Node`s from
adjacent `Digit`s and pushes them up the spine.

`O(log n)`: On each level, we merge two digits, and push at most four nodes up
the spine.

#### Random Access

`lookup :: Sized a => Int -> FingerTree a -> Maybe a` looks up the `i`-th element by
splitting the tree at `i` and returning the minimum element of the right tree.

`O(log n)`: It's just a `split` plus a constant time operation.

#### Implementation

... is not very instructive, mostly just matching on different combinations of
`Digit`s and `Node`s:

```haskell
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
```

```haskell
merge :: Sized a => FingerTree a -> FingerTree a -> FingerTree a
merge Empty tree = tree
merge tree Empty = tree
merge (Single a) tree = cons a tree
merge tree (Single z) = snoc tree z
merge (Deep ls ll lt lr) (Deep rs rl rt rr) = Deep (ls + rs) ll (merge4 lt lr rl rt) rr

merge4 :: Sized a => FingerTree (Node a) -> Digit a -> Digit a -> FingerTree (Node a) -> FingerTree (Node a)
merge4 left (One   a) (One   b)             right = merge left                      (cons (Node2 a b)         right)
merge4 left (One   a) (Two   b c)           right = merge left                      (cons (Node3 a b c)       right)
merge4 left (One   a) (Three b c d)         right = merge (snoc left (Node2 a b))   (cons (Node2     c d)     right)
merge4 left (One   a) (Four  b c d e)       right = merge (snoc left (Node3 a b c)) (cons (Node2       d e)   right)
merge4 left (Two   a b) (One   c)           right = merge left                      (cons (Node3 a b c)       right)
merge4 left (Two   a b) (Two   c d)         right = merge (snoc left (Node2 a b))   (cons (Node2     c d)     right)
merge4 left (Two   a b) (Three c d e)       right = merge (snoc left (Node3 a b c)) (cons (Node2       d e)   right)
merge4 left (Two   a b) (Four  c d e f)     right = merge (snoc left (Node3 a b c)) (cons (Node3       d e f) right)
merge4 left (Three a b c) (One   d)         right = merge (snoc left (Node2 a b))   (cons (Node2     c d)     right)
merge4 left (Three a b c) (Two   d e)       right = merge (snoc left (Node3 a b c)) (cons (Node2       d e)   right)
merge4 left (Three a b c) (Three d e f)     right = merge (snoc left (Node3 a b c)) (cons (Node3       d e f) right)
merge4 left (Three a b c) (Four  d e f g)   right = merge (snoc left (Node3 a b c)) (cons (Node2       d e) (cons (Node2 f g)   right))
merge4 left (Four  a b c d) (One   e)       right = merge (snoc left (Node3 a b c)) (cons (Node2       d e)   right)
merge4 left (Four  a b c d) (Two   e f)     right = merge (snoc left (Node3 a b c)) (cons (Node3       d e f) right)
merge4 left (Four  a b c d) (Three e f g)   right = merge (snoc left (Node3 a b c)) (cons (Node2       d e) (cons (Node2 f g)   right))
merge4 left (Four  a b c d) (Four  e f g h) right = merge (snoc left (Node3 a b c)) (cons (Node2       d e) (cons (Node3 f g h) right))
```


### Finger Tree: Applications

* Sequences (`Data.Sequence`), Queues: `O(log n)` random access, `O(1)` access to
  both ends, O(log n) `append`.

    ```haskell
    newtype Seq a = Seq (FingerTree (Element a))
    newtype Element a = Element a
    instance Sized (Element a) where
        size _ = 1
    ```

* (Fair) Priority Queues: Replace the Size annotation by a Priority annotation.
  `O(log n)` insert and access to the minimum element.
  
Despite the good asymptotic properties, Finger Trees are quite slow.
* For Sequences, Lists are generally preferable, unless random access or access
  to both ends are explicitly required.
* For Priority Queues, Heaps generally perform much better, but they are usually
  not stable (fair).



## End
