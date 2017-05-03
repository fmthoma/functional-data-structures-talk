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


### Array List insertion: Amortized analysis

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
```

When reaching the size limit, we have exactly one credit per cell to be copied.
In other words, the resizing operation has already been paid for!

With a constant three credits per insert operation, we have constant-time
insert.


### Haskell Example: Batched Queue

```haskell
data Queue a = Queue ![a] ![a]

check :: Queue a -> Queue a
check (Queue xs []) = Queue [] (reverse xs)
check queue         = queue

insert :: a -> Queue a -> Queue a
insert x (Queue xs ys) = check (Queue (x : xs) ys)

view :: Queue a -> (a, Queue a)
view (Queue xs (y : ys)) = (y, check (Queue xs ys))
```

Inserting an element always takes `O(1)`, but reversing the front end of the
queue takes `O(n)`!
=> The worst-case complexity for `view` is `O(n)`.

Can we do better?


### Amortization for Batched Queue

Use the Banker's method:

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

                                  
## Data Structures

### Banker's Queue

### Binary Heap

### Finger Tree
