# Plated

##

```haskell
class Plated a where
  plate :: Traversal' a a
```

##

```haskell
foldMapOf :: Monoid m => Traversal s t a b -> (a -> m) -> s -> m

transformOf :: Traversal' a a -> (a -> a) -> a -> a

rewriteOf :: Traversal' a a -> (a -> Maybe a) -> a -> a
```

##

```haskell
transform :: Plated a => (a -> a) -> a -> a
transform = transformOf plate

rewrite :: Plated a => (a -> Maybe a) -> a -> a
rewrite = rewriteOf plate
```

##

```haskell
universeOf :: Traversal' a a -> a -> [a]
universe :: Plated a => a -> [a]

contextsOf :: Traversal' a a -> a -> [Context a a a]
contexts :: Plated a => a -> [Context a a a]

contextsOf :: Traversal' a a -> a -> [Context a a a]
contexts :: Plated a => a -> [Context a a a]

holesOf :: Traversal' a a -> a -> [Pretext {-# nope #-}]
holes :: Plated a => a -> [Pretext {-# nope #-}]
```
