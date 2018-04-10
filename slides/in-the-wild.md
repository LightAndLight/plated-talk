# In the Wild

##

```haskell
data Expr
  = Int Int
  | Bool Bool
  | BinOp Op Expr Expr
  | ...
  deriving Generic

instance Plated Expr where; plate = gplate
```
<div class="notes">
This is a simplified representation of an imperative language, similar to what I
use in hpython. You can make these datatypes instances of Plated
</div>

##

```haskell
data Statement
  = IfThenElse Expr [Statement] [Statement]
  | While Expr [Statement]
  | Return Expr
  | ... 
  deriving Generic
  
instance Plated Statement where; plate = gplate
```

##

```haskell
_Indents :: Traversal' Statement [Whitespace]

indentFourSpaces :: Statement -> Statement
indentFourSpaces = transform (_Indents .~ replicate 4 Space)
```
<div class="notes">
_Indents is a traversal targeting a single level of indentation inside a statement

because of the way the syntax tree stores indentation, you can use transform to set all of
the indentations
</div>

##

```python
>>> printStatement myStatement

def some_function(a):
  for x in a:
    for y in x:
      print(y)
```

##

```python
>>> printStatement (indentFourSpaces myStatement)

def some_function(a):
    for x in a:
        for y in x:
            print(y)
```

##

```haskell
isMutable :: Expr -> Bool
isMutable None{} = False
isMutable Int{} = False
isMutable List{} = True
isMutable Deref{} = True
isMutable ...
```

##

<style>
.reveal pre code { max-height: 600px; }
</style>

```haskell
fixMutableDefaultArguments :: Statement -> Maybe Statement
fixMutableDefaultArguments input = do
  (name, params, body) <- input ^? _Fundef

  let paramsList = toList params
  targetParam <- paramsList ^? folded._KeywordParam.filtered (isMutable._kpExpr)

  let
    pname = targetParam ^. kpName.identValue

    newparams =
      paramsList &
      traverse._KeywordParam.filtered (isMutable._kpExpr).kpExpr .~ none_

    fixed =
      fmap (\value -> if_ (var_ pname `is_` none_) [ var_ pname .= value ]) $
      paramsList ^.. folded._KeywordParam.kpExpr.filtered isMutable

  pure $ def_ name newparams (NonEmpty.fromList $ fixed <> (body ^.. _Statements))
```

##

```python
>>> printStatement myStatement

def bad(arg1=[]):
    def bad_bad(arg2=[1,2,3]):
        arg2 += arg1
        return arg2
    return bad_bad()
```

##

```python
>>> printStatement (rewrite fixMutableDefaultArguments myStatement)

def bad(arg1=None):
    if arg1 is None:
        arg1 = []
    def bad_bad(arg2=None):
        if arg2 is None:
            arg2 = [1,2,3]
        arg2 += arg1
        return arg2
    return bad_bad()
```
