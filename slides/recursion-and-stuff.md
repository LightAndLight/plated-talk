# Recursion and stuff

##

Recursion is boilerplate

##

```haskell
data Expr
  = Int Int
  | Add Expr Expr
  | Var String
```

<div class="notes">
To demonstrate this, we'll use arithmetic expressions consisting of integers,
variables, and addition

We'll define three functions which illustrate some common operations you might
do on recursive data structures
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

##

```haskell
transformFix :: Functor f => (f (Fix f) -> f (Fix f)) -> Fix f -> Fix f
transformFix fn = Fix . fn . fmap (transformFix fn) . unfix
```

##

```haskell
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

##

```haskell
rewriteFix :: Functor f => (f (Fix f) -> Maybe (f (Fix f))) -> Fix f -> Fix f
rewriteFix fn =
  (\a -> maybe (Fix a) (rewriteFix fn . Fix) $ fn a) .
  fmap (rewriteFix fn) .
  unfix
```

##

```haskell
foldConstants :: Expr -> Expr
foldConstants = rewriteFix fn
  where
    fn (Add (Fix a) (Fix b)) =
      case (a, b) of
        (Int a', Int b') -> Just $ Int (a' <> b')
        (Int a', Add (Fix (Int b')) c') -> Just $ Add (Fix . Int $ a' <> b') c'
        (Add a' (Fix (Int b')), Int c') -> Just $ Add a' (Fix . Int $ b' <> c')
        _ -> Nothing
    fn _ = Nothing
```

##

http://hackage.haskell.org/package/fixplate

<div class="notes">
This approach is elaborated in the fixplate package
</div>

<div class="notes">
You can ease the pain of having Fix everywhere by using pattern synonyms,
but there is another approach that is just as powerful, but doesn't require
you to modify your datatype
</div>

##

`Traversal`

##

```haskell
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
```

##

```haskell
traverse :: Traversable t => (a -> f b) -> t a -> f (t b)
```

##

```haskell
traverse :: Traversable t => Traversal (t a) (t b) a b
```

##

```haskell
--                    forall f. Applicative f => (a -> f a) -> s -> f s
type Traversal' s a = Traversal s s a a
```

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

##

```haskell
vars :: Expr -> [String]
vars (Var v) = [v]
vars (Add a b) = vars a <> vars b
vars (Int _) = []
```

##

```haskell
foldTraversal
  :: Monoid m
  -- (forall f. Applicative f => (a -> f a) -> a -> f a)
  => Traversal' a a
  -> (a -> m) -> a -> m
foldTraversal t fn a =
  fn a <> getConst (t (\x -> Const (fn x) <*> Const (foldTraversal t fn x)) a)
```

<div class="notes">
This is called foldMapOf in lens, and is a bit more polymorphic
</div>

##

```haskell
vars :: Expr -> [String]
vars = foldTraversal traverseExprs fn
  where
    fn (Var v) = [v]
    fn _ = []
```

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

##

```haskell
transformTraversal
  -- (forall f. Applicative f => (a -> f a) -> a -> f a)
  :: Traversal' a a
  -> (a -> a) -> a -> a
transformTraversal t fn =
  fn .
  runIdentity . t (Identity . transformTraversal t fn)
```

##

```haskell
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

##

```haskell
rewriteTraversal 
  -- (forall f. Applicative f => (a -> f a) -> a -> f a)
  :: Traversal' a a
  -> (a -> Maybe a) -> a -> a
rewriteTraversal t fn =
  (\x -> maybe x (rewriteTraversal t fn) $ fn x) .
  runIdentity . t (Identity . rewriteTraversal t fn)
```

##

```haskell
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
