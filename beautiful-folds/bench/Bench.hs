{-# LANGUAGE NumDecimals #-}
module Main where

import Criterion.Main

import Folds

lists :: Num n => [(String, [n])]
lists =
  --[ ("1e3", fromIntegral <$> [0..1e3 :: Int])
  [ ("1e5", fromIntegral <$> [0..1e5 :: Int])
  --, ("1e7", fromIntegral <$> [0..1e7 :: Int])
  ]

main :: IO ()
main = defaultMain
  [ bgroup "FoldLeft"
    [ bgroup "length" $ flip map lists $ \(n,l) ->
      bench n $ whnf (runFoldLeft lengthL :: [Int] -> Int) l
    , bgroup "sum" $ flip map lists $ \(n,l) ->
      bench n $ whnf (runFoldLeft sumL :: [Int] -> Int) l
    , bgroup "average" $ flip map lists $ \(n,l) ->
      bench n $ whnf (runFoldLeft averageL :: [Double] -> Double) l
    , bgroup "average'" $ flip map lists $ \(n,l) ->
      bench n $ whnf (runFoldLeft averageL') l
    ]
  , bgroup "FoldMonoid"
    [ bgroup "length" $ flip map lists $ \(n,l) ->
      bench n $ whnf (runFoldMonoid lengthM :: [Int] -> Int) l
    , bgroup "sum" $ flip map lists $ \(n,l) ->
      bench n $ whnf (runFoldMonoid sumM :: [Int] -> Int) l
    , bgroup "average" $ flip map lists $ \(n,l) ->
      bench n $ whnf (runFoldMonoid averageM :: [Double] -> Double) l
    , bgroup "average'" $ flip map lists $ \(n,l) ->
      bench n $ whnf (runFoldMonoid averageM') l
    ]
  ]
