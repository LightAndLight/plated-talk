{-# language RankNTypes #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language StandaloneDeriving, UndecidableInstances #-}
import Data.Foldable

import Data.Functor.Const
import Data.Functor.Identity
import Data.Monoid

data Fix f = Fix { unfix :: f (Fix f) }
deriving instance Show (f (Fix f)) => Show (Fix f)

data Expr' a
  = Int Int
  | Add a a
  | Var String
  deriving (Show, Functor, Foldable, Traversable)

type Expr = Fix Expr'

foldEverywhere
  :: (Foldable f, Monoid m)
  => (forall x. f x -> m)
  -> Fix f -> m
foldEverywhere fn (Fix a) = fn a <> foldMap (foldEverywhere fn) a

vars :: Expr -> [String]
vars = foldEverywhere fn
  where
    fn (Var v) = [v]
    fn _ = []

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

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type Traversal' s a = Traversal s s a a

data Expr2
  = Int2 Int
  | Add2 Expr2 Expr2
  | Var2 String
  deriving Show

traverseExprs :: Traversal' Expr2 Expr2
traverseExprs f (Add2 a b) = Add2 <$> f a <*> f b
traverseExprs _ expr = pure expr

vars2 :: Expr2 -> [String]
vars2 (Var2 v) = [v]
vars2 (Add2 a b) = vars2 a ++ vars2 b
vars2 (Int2 _) = []

foldTraversal
  :: Monoid m
  => (forall f. Applicative f => (a -> f a) -> a -> f a)
  -> (a -> m) -> a -> m
foldTraversal t fn a =
  fn a <> getConst (t (Const . foldTraversal t fn) a)

vars2' :: Expr2 -> [String]
vars2' = foldTraversal traverseExprs fn
  where
    fn (Var2 v) = [v]
    fn _ = []

transformTraversal
  -- (forall f. Applicative f => (a -> f a) -> a -> f a)
  :: Traversal' a a
  -> (a -> a) -> a -> a
transformTraversal t fn =
  fn . runIdentity . t (Identity . transformTraversal t fn)

optimize_add_0' :: Expr2 -> Expr2
optimize_add_0' = transformTraversal traverseExprs fn
  where
    fn (Add2 a b) =
      case (a, b) of
        (Int2 0, _) -> b
        (_, Int2 0) -> a
        _ -> Add2 a b
    fn a = a

rewriteTraversal 
  -- (forall f. Applicative f => (a -> f a) -> a -> f a)
  :: Traversal' a a
  -> (a -> Maybe a) -> a -> a
rewriteTraversal t fn =
  (\x -> maybe x (rewriteTraversal t fn) $ fn x) .
  runIdentity . t (Identity . rewriteTraversal t fn)

fold_constants' :: Expr2 -> Expr2
fold_constants' = rewriteTraversal traverseExprs fn
  where
    fn (Add2 a b) =
      case (a, b) of
        (Int2 a', Int2 b') -> Just $ Int2 (a' + b')
        (Int2 a', Add2 (Int2 b') c') -> Just $ Add2 (Int2 $ a' + b') c'
        (Add2 a' (Int2 b'), Int2 c') -> Just $ Add2 a' (Int2 $ b' + c')
        _ -> Nothing
    fn _ = Nothing
