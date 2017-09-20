{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}
module Folds where

import Data.Monoid
import Data.Bifunctor
import Data.Foldable (foldl')


data Pair a b = Pair !a !b
  deriving (Show, Eq, Ord)

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
  mempty = Pair mempty mempty
  mappend (Pair a b) (Pair a' b') = Pair (mappend a a') (mappend b b')

instance Bifunctor Pair where
  bimap f g (Pair a b) = Pair (f a) (g b)



type Fold i o = [i] -> o


-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------

data FoldLeft i o = forall a. FoldLeft
  { _combine :: a -> i -> a
  , _zero :: a
  , _post :: a -> o
  }

runFoldLeft :: Foldable f => FoldLeft i o -> f i -> o
runFoldLeft (FoldLeft c z p) = p . foldl' c z

instance Functor (FoldLeft i) where
  fmap f (FoldLeft c z p) = FoldLeft c z (f . p)

instance Applicative (FoldLeft i) where
  pure x = FoldLeft const x id
  FoldLeft c z p <*> FoldLeft c' z' p' = FoldLeft combine zero post
    where
      combine (Pair a b) i = Pair (c a i) (c' b i)
      zero = Pair z z'
      post (Pair f a) = p f (p' a)


lengthL :: Num o => FoldLeft i o
lengthL = FoldLeft (const . (+1)) 0 id

sumL :: Num n => FoldLeft n n
sumL = FoldLeft (+) 0 id

averageL :: Fractional n => FoldLeft n n
averageL = (/) <$> sumL <*> lengthL

lengthL' :: Num o => FoldLeft i o
lengthL' = FoldLeft (const . succ) (0 :: Int) (fromInteger . fromIntegral)

averageL' :: FoldLeft Int Double
averageL' = (/) . fromInteger . fromIntegral <$> sumL <*> lengthL


-- ----------------------------------------------------------------------------
--
-- ----------------------------------------------------------------------------

data FoldMonoid i o = forall m. Monoid m => FoldMonoid
  { _tally :: i -> m
  , _summarize :: m -> o
  }

runFoldMonoid :: Foldable f => FoldMonoid i o -> f i -> o
runFoldMonoid (FoldMonoid t s) = s . foldMap t

instance Functor (FoldMonoid i) where
  fmap f (FoldMonoid t s) = FoldMonoid t (f . s)

instance Applicative (FoldMonoid i) where
  pure x = FoldMonoid (const ()) (const x)
  FoldMonoid tF sF <*> FoldMonoid tX sX = FoldMonoid t s
    where
      t i = Pair (tF i) (tX i)
      s (Pair mF mX) = sF mF (sX mX)

lengthM :: Num o => FoldMonoid i o
lengthM = FoldMonoid (Sum . const 1) getSum

sumM :: Num n => FoldMonoid n n
sumM = FoldMonoid Sum getSum

averageM :: Fractional n => FoldMonoid n n
averageM = (/) <$> sumM <*> lengthM

lengthM' :: Num o => FoldMonoid i o
lengthM' = FoldMonoid (Sum . const (1 :: Int)) (fromInteger . fromIntegral . getSum)

averageM' :: FoldMonoid Int Double
averageM' = (/) . fromInteger . fromIntegral <$> sumM <*> lengthM

