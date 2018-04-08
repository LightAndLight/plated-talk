# Recursion and stuff

##

Recursion is boilerplate

<div class="notes">
* If you implement some function on a recursive datastructure, you're likely duplicating code
* Pre-order, in-order, post-order, bottom-up or top-down
* We keep re-implementing HOW to traverse the tree, but the only bit we actually care about is
  what to do at each node
</div>

##

```haskell
data Expr
  = Int Int
  | Add Expr Expr
  | Var String
```

<div class="notes">
* Demonstrate this with running example of arithmetic expressions
* integers, variables, and addition
* Define three functions, some common operations you might do on recursive data structures
</div>

##

```haskell
vars :: Expr -> [String]
vars (Var v) = [v]
vars (Add a b) = vars a <> vars b
vars (Int _) = []
```

<div class="notes">
1. "Summary" or fold

We want a list of all the variables in an expression.
</div>

##

```haskell
optimizeAdd0 :: Expr -> Expr
optimizeAdd0 (Add a b) =
  case (optimizeAdd0 a, optimizeAdd0 b) of
    (Int 0, b') -> b'
    (a', Int 0) -> a'
    _ -> Add a' b'
optimizeAdd0 a = a
```

<div class="notes">
2. Whole program transformation

Replace all the occurrences of something <> 0, or 0 <> something with just the something.
</div>

##

```haskell
foldConstants :: Expr -> Expr
foldConstants (Add a b) =
  case (foldConstants a, foldConstants b) of
    (Int a', Int b') -> Int (a' <> b')
    (Int a', Add (Int b') c') -> Add (Int $ a' <> b') c'
    (Add a' (Int b'), Int c') -> Add a' (Int $ b' <> c')
    _ -> Add a' b'
foldConstants a = a
```

<div class="notes">
3. Whole program transformation

When constants are being added, do the addition. This is a common compiler optimization.
There are probably some more cases you need to get complete folding, but this is enough for us.
</div>

##

```haskell
data Expr
  = Int Int
  | Add Expr Expr
  | Var String

vars :: Expr -> [String]
vars (Var v) = [v]
vars (Add a b) = vars a <> vars b
vars (Int _) = []

optimizeAdd0 :: Expr -> Expr
optimizeAdd0 (Add a b) =
  case (optimizeAdd0 a, optimizeAdd0 b) of
    (Int 0, b') -> b'
    (a', Int 0) -> a'
    _ -> Add a' b'
optimizeAdd0 a = a

foldConstants :: Expr -> Expr
foldConstants (Add a b) =
  case (foldConstants a, foldConstants b) of
    (Int a', Int b') -> Int (a' <> b')
    (Int a', Add (Int b') c') -> Add (Int $ a' <> b') c'
    (Add a' (Int b'), Int c') -> Add a' (Int $ b' <> c')
    _ -> Add a' b'
foldConstants a = a
```

<div class="notes">
These sorts of ideas - summarising trees and recursive bottom-up rewriting - are boilerplate.

They all follow the same general pattern. The only thing that changes is what we do to the nodes.

Let's explore the ways we can improve this.
</div>

##

```haskell
data ExprF a
  = IntF Int
  | AddF a a
  | VarF String
  deriving (Functor, Foldable)
```

<div class="notes">
The first way is the "recursion schemes" style - factor the recursion out of the data structure.

The idea is that we can talk about transformations at the level of a single node, and then have
that applied everywhere in the structure.
</div>

##

```haskell
AddF (IntF 1) (AddF (VarF "x") (IntF 3)) :: ExprF (ExprF (ExprF a))
```

<div class="notes">
Just using this data structure on its own will land you with a type that's as big as the term is
deep, so we need a helper type that re-introduces recursion.
</div>

##

```haskell
newtype Fix f = Fix { unfix :: f (Fix f) }
```

##

```haskell
Fix (AddF (Fix $ IntF 1) (Fix $ AddF (Fix $ VarF "x") (Fix $ IntF 3))) :: Fix ExprF
```
<div class="notes">
Now the term can be as deeply nested as we want and the type remains constant
</div>

##

```haskell
{-# language PatternSynonyms #-}

type Expr = Fix ExprF

{-# complete Int, Add, Var #-}
pattern Int a = Fix (IntF a)
pattern Add a b = Fix (AddF a b)
pattern Var a = Fix (VarF a)
```
<div class="notes">
Type synonym,

Nesting Fix and unfix everywhere gets tedious, so we can define pattern synonyms
and use a completeness pragma to make it all easier

Let's have a look at how we can use this new structure to eliminate some boilerplate
</div>

##

```haskell
vars :: Expr -> [String]
vars (Var v) = [v]
vars (Add a b) = vars a <> vars b
vars (Int _) = []
```
<div class="notes">
This function definition still works as is, but it's working over a Fixpoint structure.

We want to generalize the pattern of "monoidal summary of a tree"
</div>

##

```haskell
foldFix :: (Foldable f, Monoid m) => (forall x. f x -> m) -> Fix f -> m
foldFix fn (Fix a) = fn a <> foldMap (foldFix fn) a

vars :: Expr -> [String]
vars = foldFix fn
  where
    fn (Var v) = [v]
    fn _ = []
```
<div class="notes">
We can exploit this idea of "operating on a single node, and applying that everywhere"
for monoidal summaries, if the node is foldable

We summarise the current node, then recursively summarise its children by foldMap-ing.
</div>

##

```haskell
optimizeAdd0 :: Expr -> Expr
optimizeAdd0 expr =
  Fix $ case unfix expr of
    Add a b ->
      case (unfix a, unfix b) of
        (Int 0, _) -> optimizeAdd0 b
        (_, Int 0) -> optimizeAdd0 a
        _ -> Add (optimizeAdd0 a) (optimizeAdd0 b)
    a -> a
```
<div class="notes">
The essence of this function is "apply a transformation from the bottom up everywhere in the tree"
</div>

##

```haskell
transformFix :: Functor f => (f (Fix f) -> f (Fix f)) -> Fix f -> Fix f
transformFix fn = Fix . fn . fmap (transformFix fn) . unfix

optimizeAdd0 :: Expr -> Expr
optimizeAdd0 = transformFix fn
  where
    fn (Add a b) =
      case (unfix a, unfix b) of
        (Int 0, b') -> b'
        (a', Int 0) -> a'
        _ -> Add a b
    fn a = a
```

##

```haskell
foldConstants :: Expr -> Expr
foldConstants expr =
  Fix $ case unfix expr of
    Add a b ->
      case (foldConstants a, foldConstants b) of
        (Int a', Int b') -> Int (a' <> b')
        (Int a', Add (Int b') c') -> Add (Int $ a' <> b') c'
        (Add a' (Int b'), Int c') -> Add a' (Int $ b' <> c')
        _ -> Add a' b'
    a -> a
```
<div class="notes">
This one is similar to the last, in that it's a bottom up transformation, but if we wrote
it in terms of `transformFix`, it wouldn't behave as we indended.

This is because transformFix only applies a transformation once to each node, but this
transformation might need to be applied multiple times to reach "most optimized" form
</div>

##

```haskell
rewriteFix :: Functor f => (f (Fix f) -> Maybe (f (Fix f))) -> Fix f -> Fix f
rewriteFix fn =
  (\a -> maybe (Fix a) (rewriteFix fn . Fix) $ fn a) .
  fmap (rewriteFix fn) .
  unfix

foldConstants :: Expr -> Expr
foldConstants = rewriteFix fn
  where
    fn (Add a b) =
      case (unfix a, unfix b) of
        (Int a', Int b') -> Just $ Int (a' + b')
        (Int a', Add (Fix (Int b')) c') -> Just $ Add (Fix . Int $ a' + b') c'
        (Add a' (Fix (Int b')), Int c') -> Just $ Add a' (Fix . Int $ b' + c')
        _ -> Nothing
    fn _ = Nothing
```
<div class="notes">
We capture this behaviour by making the rule a partial function. rewriteFix will stop
when the function returns Nothing for every subterm in the tree.

'transformFix Just' will spin forever
</div>

##

http://hackage.haskell.org/package/fixplate

<div class="notes">
This approach is elaborated in the fixplate package

With the fixplate/recursion schemes style, we have abstracted over certain ways of doing
recursive operations, and as users only have to write the core transformation logic.

The cost is that we had to modify our data structure to so. We regained usability
with type- and pattern-synonyms, so it's not a horrible way to go about it.

Next I'll present an equally powerful way of writing these general recursive operations,
without having to modify the underlying datatype.

To get there, though, we're going to discuss some lens-theory
</div>

##

`Traversal`
<div class="notes">
Traversals are one of the oldest concepts in lens
</div>

##

```haskell
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
```
<div class="notes">
They look like this.

Literally, this type signature says "if you tell me how to (effectfully) transform an A into a B,
then I will (effectfully) transform an S into a T.

More intuitively, you could say that the S *contains* some As, and if you transform all those As into
Bs, you end up with a T.

We can make this more apparent by considering the simplest traversal: traverse
</div>

##

```haskell
traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
```
<div class="notes">
Traverse says says "if you tell me how to (effectfully) transform an A into a B,
then I will (effectfully) transform a TA into a TB

And to hook into that "intuitive" explanation of traversals- a TA contains some As,
and if you transform those As intos Bs, you get a TB
</div>

##

```haskell
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s   -> f t

traverse ::       (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)

traverse :: Traversable t => Traversal (t a) (t b) a b
```
<div class="notes">
So the type of traverse can also be written like this

ASK FOR QUESTIONS
</div>

##

```haskell
--                    forall f. Applicative f => (a -> f a) -> s -> f s
type Traversal' s a = Traversal s s a a
```
<div class="notes">
We will also use this Traversal Prime type to express when you can't change the type
of the target of the traversal

This less-stabby version also helps lead into what traversals will mean for us:

A traversal prime S A represents a PATH that touchs some, or all of the As in an S
</div>

##

```haskell
data Expr
  = Int Int
  | Add Expr Expr
  | Var String
  
--               forall f. Applicative f => (Expr -> f Expr) -> Expr -> f Expr
traverseExprs :: Traversal' Expr Expr
traverseExprs f (Add a b) = Add <$> f a <*> f b
traverseExprs _ expr = pure expr
```
<div class="notes">
So we'll go back to our original definition of Expr, and define a traversal which says how to
get at all the Exprs one level down. for add, you say how to get at the left and right nodes.
Var and Int don't contain Exprs, and we say this with pure.

This is where it contrasts with the fixplate style- in recursion schemes, the DATA STRUCTURE
only knows about a single level of chidren because we factored out the recursion

But in this approach, we write a FUNCTION that says how to work over the children one level down.

So how do we use it?
</div>

##

```haskell
vars :: Expr -> [String]
vars (Var v) = [v]
vars (Add a b) = vars a <> vars b
vars (Int _) = []
```
<div class="notes">
Back to our monoidal summary function
</div>


##

```haskell
foldTraversal
  :: Monoid m
  -- (forall f. Applicative f => (a -> f a) -> a -> f a)
  => Traversal' a a
  -> (a -> m) -> a -> m
foldTraversal t fn a =
  fn a <> getConst (t (Const . foldTraversal t fn) a)

vars :: Expr -> [String]
vars = foldTraversal traverseExprs fn
  where
    fn (Var v) = [v]
    fn _ = []
```
<div class="notes">
This definition's a bit more of a brain-ful.

The important part of Traversals is that they're quantified over all applicatives,
so we can pick our applicative every time we call it.

We choose to work in the Const applicative
</div>

##

```haskell
newtype Const a b = Const a

instance Monoid a => Applicative (Const a) where
  pure _ = Const mempty
  Const a <*> Const b = Const (a <> b)
```
<div class="notes">
Because apply for Const will accumulate its contents if its contents are a monoid
</div>

##

```haskell
foldTraversal
  :: Monoid m
  -- (forall f. Applicative f => (a -> f a) -> a -> f a)
  => Traversal' a a
  -> (a -> m) -> a -> m
foldTraversal t fn a =
  fn a <> getConst (t (Const . foldTraversal t fn) a)

vars :: Expr -> [String]
vars = foldTraversal traverseExprs fn
  where
    fn (Var v) = [v]
    fn _ = []
```
<div class="notes">
So we summarise the current node, then recursively summarise the node's children via
the traversal, which will summarise each child node, and visit their children and so on.
</div>

##

```haskell
optimizeAdd0 :: Expr -> Expr
optimizeAdd0 (Int i) = Int i
optimizeAdd0 (Add a b) =
  case (a, b) of
    (Int 0, _) -> optimizeAdd0 b
    (_, Int 0) -> optimizeAdd0 a
    _ -> Add (optimizeAdd0 a) (optimizeAdd0 b)
optimizeAdd0 (Var v) = Var v
```
<div class="notes">
We can do a similar trick for this next one, but pick a difference applicative
</div>

##

```haskell
transformTraversal
  -- (forall f. Applicative f => (a -> f a) -> a -> f a)
  :: Traversal' a a
  -> (a -> a) -> a -> a
transformTraversal t fn =
  fn . runIdentity . t (Identity . transformTraversal t fn)

optimizeAdd0 :: Expr -> Expr
optimizeAdd0 = transformTraversal traverseExprs fn
  where
    fn (Add a b) =
      case (a, b) of
        (Int 0, _) -> b
        (_, Int 0) -> a
        _ -> Add a b
    fn a = a
```
<div class="notes">
In this case: identity

So transformTraversal says "first, recursively transform the children via our Traversal,
then tranform the result of that"
</div>

##

```haskell
foldConstants :: Expr -> Expr
foldConstants (Int i) = Int i
foldConstants (Add a b) =
  case (foldConstants a, foldConstants b) of
    (Int a', Int b') -> Int (a' <> b')
    (Int a', Add (Int b') c') -> Add (Int $ a' <> b') c'
    (Add a' (Int b'), Int c') -> Add a' (Int $ b' <> c')
    _ -> Add a' b'
foldConstants (Var v) = Var v
```
<div class="notes">
So again for foldConstants we want that behaviour of "keep running this tranformation
until it no longer applies"
</div>

##

```haskell
rewriteTraversal 
  -- (forall f. Applicative f => (a -> f a) -> a -> f a)
  :: Traversal' a a
  -> (a -> Maybe a) -> a -> a
rewriteTraversal t fn =
  (\x -> maybe x (rewriteTraversal t fn) $ fn x) .
  runIdentity . t (Identity . rewriteTraversal t fn)

foldConstants :: Expr -> Expr
foldConstants = rewriteTraversal traverseExprs fn
  where
    fn (Add a b) =
      case (foldConstants a, foldConstants b) of
        (Int a', Int b') -> Just $ Int (a' <> b')
        (Int a', Add (Int b') c') -> Just $ Add (Int $ a' <> b') c'
        (Add a' (Int b'), Int c') -> Just $ Add a' (Int $ b' <> c')
        _ -> Nothing
    fn _ = Nothing
```
<div class="notes">
It proceeds similarly to the fixplate case, but instead of fmapping we pick the identity
applicative again so we can recursively apply the tranformation using the traversal.

ASK FOR QUESTIONS

This is the core of plated- generalised recursive operations that use traversals to figure
out how to find the children inside a datatype.
</div>
