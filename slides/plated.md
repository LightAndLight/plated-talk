# Plated

##

https://hackage.haskell.org/package/lens

<div class="notes">
Plated is part of the lens package
</div>

##

`Control.Lens.Plated`
<div class="notes">
In this module
</div>

##

```haskell
-- Control.Lens.Fold
foldMapOf :: Monoid m => Traversal s t a b -> (a -> m) -> s -> m

transformOf :: Traversal' a a -> (a -> a) -> a -> a

rewriteOf :: Traversal' a a -> (a -> Maybe a) -> a -> a
```
<div class="notes">
foldTraversal, transformTraversal and rewriteTraversal are all there, but by different
names. foldTraversal is foldMapOf, which is actually found in Control.Lens.Fold because is
a really-really- general operator

transformTraversal and rewriteTraversal are called transformOf and rewriteOf respectively,
which follows a broader naming convention found throughout lens.
</div>

##

```haskell
class Plated a where
  plate :: Traversal' a a

transform :: Plated a => (a -> a) -> a -> a
transform = transformOf plate

rewrite :: Plated a => (a -> Maybe a) -> a -> a
rewrite = rewriteOf plate

...
```
<div class="notes">
Control.Lens.Plated also defines the Plated typeclass, and convenience functions
which call the X-Of functions with the `plate` traversal.

The use of a typeclass is justified because in some sense there is a "canonical"
traversal over the recursive children of a datatype.
</div>

##

```haskell
gplate :: (Generic a, GPlated a (Rep a)) => Traversal' a a
```
<div class="notes">
This is witnessed by gplate, which will create a plated traversal for a recursive
datatype using GHC's generics mechanism.
</div>

##

```haskell
data Expr
  = Int Int
  | Add Expr Expr
  | Var String
  deriving Generic

instance Plated Expr where
  plate = gplate
```
<div class="notes">
So you can get plated for free for a type that has a Generic instance

It's this, coupled with the extensive lens integration that makes Plated win for me.
Because plate is a traversal, it's also a getter, setter and fold, which means you can
use it with all the other lens operations
</div>

##

```haskell
universeOf :: Traversal' a a -> a -> [a]
universe :: Plated a => a -> [a]

-- Context a a a ~ (a, a -> a)
contextsOf :: Traversal' a a -> a -> [Context a a a]
contexts :: Plated a => a -> [Context a a a]

holesOf :: Traversal' a a -> a -> [Pretext {-# NOPE #-}]
holes :: Plated a => a -> [Pretext {-# NOPE #-}]
```
<div class="notes">
There's more than just rewrite and transform. Here's some cool ones:

universe: collect all the As in a tree transitively

contexts: collect all the As in a tree transitively, but also enumerate
their contexts. That is, make holes where their children should go so that
you can substitute in a different child.

holes: like contexts, but only a single level down. You could use this for
zippers. I left out that type because it's a bit funny.

I don't really want to explain, just to "wow" you with what's possible
</div>
