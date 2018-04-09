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
  
instance Plated Statement where; plate = gplate
instance Plated Expr where; plate = gplate
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
