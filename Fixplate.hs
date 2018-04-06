{-# language RankNTypes #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language StandaloneDeriving, UndecidableInstances #-}
import Data.Foldable

data Fix f = Fix { unfix :: f (Fix f) }
deriving instance Show (f (Fix f)) => Show (Fix f)

data Expr' a
  = Int Int
  | Add a a
  | Var String
  deriving (Show, Functor, Foldable, Traversable)

type Expr = Fix Expr'

foldEverywhere :: (Functor f, Foldable f, Monoid m) => (f m -> m) -> Fix f -> m
foldEverywhere fn = fold . fmap (foldEverywhere fn) . unfix

vars :: Expr -> [String]
vars = foldEverywhere fn
  where
    fn (Var v) = [v]
    fn (Add a b) = a ++ b
    fn (Int _) = []

transformFix :: Functor f => (f (Fix f) -> f (Fix f)) -> Fix f -> Fix f
transformFix fn = Fix . fn . fmap (transformFix fn) . unfix

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

rewriteFix :: Functor f => (f (Fix f) -> Maybe (f (Fix f))) -> Fix f -> Fix f
rewriteFix fn =
  (\a -> maybe (Fix a) (rewriteFix fn . Fix) $ fn a) .
  fmap (rewriteFix fn) .
  unfix

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
