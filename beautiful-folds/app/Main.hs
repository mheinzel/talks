{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Prelude hiding (length, sum)
import System.Environment (getArgs)
import Data.Monoid
import Data.Foldable (foldl')
import Data.List.Split (chunksOf)
import Control.Parallel.Strategies

divide :: Int -> Int -> Double
divide x y = intToDouble x / intToDouble y
  where
    intToDouble = fromInteger . fromIntegral

length :: [a] -> Int
length = foldl' (const . (+1)) 0

sum :: [Int] -> Int
sum = foldl' (+) (0)

average :: [Int] -> Double
average list = divide (sum list) (length list)

average' :: [Int] -> Double
average' xs = divide su le
  where
    Pair su le = foldl' f (Pair 0 0) xs
    f (Pair s l) n = Pair (s + n) (l + 1)


data Pair a b = Pair !a !b
  deriving (Show, Eq, Ord)

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
  mempty = Pair mempty mempty
  mappend (Pair a b) (Pair a' b') = Pair (mappend a a') (mappend b b')


data Fold i o = forall m. Monoid m => Fold (i -> m) (m -> o)

compose :: Fold i o -> Fold i o' -> Fold i (o, o')
compose (Fold pre1 post1) (Fold pre2 post2) = Fold pre post
  where
    pre i = Pair (pre1 i) (pre2 i)
    post (Pair m1 m2) = (post1 m1, post2 m2)

instance Functor (Fold i) where
  fmap f (Fold pre post) = Fold pre (f . post)

instance Applicative (Fold i) where
  pure x = Fold (const ()) (const x)
  Fold preF postF <*> Fold preX postX = Fold pre post
    where
      pre i = Pair (preF i) (preX i)
      post (Pair mF mX) = postF mF (postX mX)

run :: Fold i o -> [i] -> o
run (Fold pre post) = post . foldl' mappend mempty . map pre

lengthF :: Fold i Int
lengthF = Fold (Sum . const 1) getSum

sumF :: Fold Int Int
sumF = Fold Sum getSum

averageF :: Fold Int Double
averageF = divide <$> sumF <*> lengthF


runInChunksOf :: Int -> Fold i o -> [i] -> o
runInChunksOf n (Fold pre post) =
  post . reduce . parMap rseq inner . chunksOf n
  where
    reduce = foldl' mappend mempty
    inner = reduce . fmap pre


main :: IO ()
main = do
  (switch:_) <- getArgs
  case switch of
    "sum"      -> print $ sum [1 .. 100000000 ]
    "average-naive"    -> print $ average [1 .. 100000000 ]
    "average-fixed"    -> print $ average' [1 .. 100000000 ]
    "average-monoid"   -> print $ run averageF [1 .. 100000000 ]
    "average-parallel" -> print $ runInChunksOf 100000 averageF [1 .. 100000000 ]
    _ -> putStrLn "not found"
