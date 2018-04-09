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
transformOf :: Traversal' a a -> (a -> a) -> a -> a

rewriteOf :: Traversal' a a -> (a -> Maybe a) -> a -> a

cosmosOf :: Traversal' a a -> Fold a a
```
<div class="notes">
transformTraversal and rewriteTraversal are there by different
names. foldTraversal is foldMapOf, which is actually found in Control.Lens.Fold because is
a really-really- general operator

transformTraversal and rewriteTraversal are called transformOf and rewriteOf respectively,
which follows a broader naming convention found throughout lens.

fold - read only traversal
given a traversal that targets the immediate children,
cosmosOf creates a fold over the transitive children

You can write the "vars" function with this in a much more compositional way.
Later.
</div>

##

```haskell
class Plated a where
  plate :: Traversal' a a

transform :: Plated a => (a -> a) -> a -> a
transform = transformOf plate

rewrite :: Plated a => (a -> Maybe a) -> a -> a
rewrite = rewriteOf plate

cosmos :: Plated a => Fold a a
cosmos = cosmosOf plate
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
-- Control.Lens.Fold
toListOf :: Fold s a -> s -> [a]

_Int :: Prism' Expr Int
_Add :: Prism' Expr (Expr, Expr)
_Var :: Prism' Expr String

vars :: Expr -> [String]
vars = toListOf (cosmos._Var)
```
<div class="notes">
If you have prisms on your constructors, you can use cosmos to "find" all
occurrences of a particular construtor.
</div>

##

```haskell
-- Context a a a ~ (a, a -> a)
contextsOf :: Traversal' a a -> a -> [Context a a a]
contexts :: Plated a => a -> [Context a a a]

holesOf :: Traversal' a a -> a -> [Pretext {-# NOPE #-}]
holes :: Plated a => a -> [Pretext {-# NOPE #-}]
```
<div class="notes">
There's more than just rewrite transform and cosmos. Here's some cool ones:

contexts: collect all the As in a tree transitively, but also enumerate
their contexts. That is, make holes where their children should go so that
you can substitute in a different child.

holes: like contexts, but only a single level down. You could use this for
zippers. I left out that type because it's a bit funny.

I don't really want to explain, just to "wow" you with what's possible
</div>
