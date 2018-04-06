# Recursion and stuff

##

```haskell
data Expr
  = Int Int
  | Add Expr Expr
  | Var String
  
vars :: Expr -> [String]
vars (Var v) = [v]
vars (Add a b) = vars a ++ vars b
vars (Int _) = []

optimize_add_0 :: Expr -> Expr
optimize_add_0 (Int i) = Int i
optimize_add_0 (Add a b) =
  case (a, b) of
    (Int 0, _) -> optimize_add_0 b
    (_, Int 0) -> optimize_add_0 a
    _ -> Add (optimize_add_0 a) (optimize_add_0 b)
optimize_add_0 (Var v) = Var v

fold_constants :: Expr -> Expr
fold_constants (Int i) = Int i
fold_constants (Add a b) =
  case (fold_constants a, fold_constants b) of
    (Int a', Int b') -> Int (a' + b')
    (Int a', Add (Int b') c') -> Add (Int $ a' + b') c'
    (Add a' (Int b'), Int c') -> Add a' (Int $ b' + c')
    _ -> Add a' b'
fold_constants (Var v) = Var v
```

<div class="notes">
</div>

##

```haskell
data Expr' a
  = Int Int
  | Add a a
  | Var String
  deriving (Functor, Foldable)
```

##
```haskell
data Fix f = Fix { unfix :: f (Fix f) }
```

##
```haskell
type Expr = Fix Expr'
```

##

```haskell
vars :: Expr -> [String]
vars expr =
  case unfix expr of
    Var v -> [v]
    Add a b -> vars a ++ vars b
    Int _ -> []
```

##

```haskell
foldFix :: (Functor f, Foldable f, Monoid m) => (f m -> m) -> Fix f -> m
foldFix fn = fold . fmap (foldFix fn) . unfix
```

##

```haskell
vars :: Expr -> [String]
vars = foldFix fn
  where
    fn :: Expr' [String] -> [String]
    fn (Var v) = [v]
    fn (Add a b) = a ++ b
    fn (Int _) = []
```

##

```haskell
optimize_add_0 :: Expr -> Expr
optimize_add_0 expr =
  Fix $ case unfix expr of
    Int i -> Int i
    Add a b ->
      case (unfix a, unfix b) of
        (Int 0, _) -> optimize_add_0 b
        (_, Int 0) -> optimize_add_0 a
        _ -> Add (optimize_add_0 a) (optimize_add_0 b)
    Var v -> Var v
```

##

```haskell
transformFix :: Functor f => (f (Fix f) -> f (Fix f)) -> Fix f -> Fix f
transformFix fn = Fix . fn . fmap (transformFix fn) . unfix
```

##

```haskell
optimize_add_0 :: Expr -> Expr
optimize_add_0 = transformFix fn
  where
    fn (Int i) = Int i
    fn (Add a b) =
      case (unfix a, unfix b) of
        (Int 0, b') -> b'
        (a', Int 0) -> a'
        _ -> Add a b
    fn (Var v) = Var v
```

##

```haskell
fold_constants :: Expr -> Expr
fold_constants (Int i) = Int i
fold_constants (Add a b) =
  case (fold_constants a, fold_constants b) of
    (Int a', Int b') -> Int (a' + b')
    (Int a', Add (Int b') c') -> Add (Int $ a' + b') c'
    (Add a' (Int b'), Int c') -> Add a' (Int $ b' + c')
    _ -> Add a' b'
fold_constants (Var v) = Var v
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
fold_constants :: Expr -> Expr
fold_constants = rewriteFix fn
  where
    fn (Add (Fix a) (Fix b)) =
      case (a, b) of
        (Int a', Int b') -> Just $ Int (a' + b')
        (Int a', Add (Fix (Int b')) c') -> Just $ Add (Fix . Int $ a' + b') c'
        (Add a' (Fix (Int b')), Int c') -> Just $ Add a' (Fix . Int $ b' + c')
        _ -> Nothing
    fn _ = Nothing
```

##

http://hackage.haskell.org/package/fixplate

<div class="notes">
This approach is elaborated in the fixplate package
</div>

##

```haskell
{-# language PatternSynonyms #-}

data ExprF a
  = IntF Int
  | AddF a a
  | VarF String
  deriving (Functor, Foldable)

type Expr = Fix ExprF

{-# complete Int, Add, Var #-}
pattern Int a = Fix (IntF a)
pattern Add a b = Fix (AddF a b)
pattern Var a = Fix (Var a)
```

<div class="notes">
You can ease the pain of having Fix everywhere by using pattern synonyms,
but there is another approach that is just as powerful, but doesn't require
you to modify your datatype
</div>

##

`Plated`

##

