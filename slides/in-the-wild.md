# In the Wild

##

```haskell
data Statement
  = IfThenElse Expr [Statement] [Statement]
  | While Expr [Statement]
  | Return Expr
  | ... 
  deriving Generic
  
data Expr
  = Int Int
  | Bool Bool
  | BinOp Op Expr Expr
  | ...
  deriving Generic
  
instance Plated Statement where ...
instance Plated Expr where ...
```

<div class="notes">
This is a simplified representation of an imperative language, similar to what I
use in hpython. You can make these datatypes instances of Plated
</div>

##

```haskell
fixMutableDefaultArguments :: Statement -> Maybe Statement

optimizeTailRecursion :: Statement -> Maybe Statement

foldConstants :: Statement -> Maybe Statement
```

<div class="notes">
Mechanical refactoring can be expressed as tree transformations
</div>

##

```haskell
rewrite fixMutableDefaultArguments :: Statement -> Statement

rewrite optimizeTailRecursion :: Statement -> Statement

rewrite foldConstants :: Statement -> Statement
```

<div class="notes">
And then the plated combinators will make sure they're applied everywhere
</div>

##

Lämmel, R., & Jones, S. P. (2003).
Scrap your boilerplate: a practical design pattern for generic programming
(Vol. 38, No. 3, pp. 26-37). ACM.

Mitchell, N., & Runciman, C. (2007, September).
Uniform boilerplate and list processing.
In Proceedings of the ACM SIGPLAN workshop on Haskell workshop (pp. 49-60). ACM.

https://hackage.haskell.org/package/uniplate

https://hackage.haskell.org/package/fixplate

https://hackage.haskell.org/package/lens

<div class="notes">
Links

SYB paper: the origin of this traversal-ish technique. Before applicatives were discovered!

Uniplate paper: origin of the kind of API that Control.Lens.Plated exposes

Uniplate library: companion library for the paper

These things are sort of archaic now

Fixplate library: uniplate style API for recursion schemes- based datatypes

Lens library: modern incarnation of uniplate in the lens ecosystem
</div>
