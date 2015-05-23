module Data where

import Data.Array
import Data.Foldable
import Data.Tuple
import Control.MonadPlus

even :: Number -> Boolean
even 0 = true
even 1 = false
even n = even (n - 2)

countEvenInArray :: [Number] -> Number
countEvenInArray [] = 0
countEvenInArray (x : xs) = (countEvenInArray xs) + 
  case even x of
       true -> 1
       false -> 0

squareArray :: [Number] -> [Number]
squareArray array = square <$> array
  where
    square n = n * n


removeNegatives :: [Number] -> [Number]
removeNegatives array = filter nonnegative array
  where
    nonnegative x = x >= 0


(<$?>) = filter

removeNegatives' :: [Number] -> [Number]
removeNegatives' array = nonnegative <$?> array
  where
    nonnegative x = x >= 0


factors :: Number -> [[Number]]
factors n = filter (\xs -> product xs == n) $ do
  i <- 1 .. n
  j <- i .. n
  return [i, j]

isPrime :: Number -> Boolean
isPrime n = (length $ factors n) == 1

cartesianProduct :: forall a b. [a] -> [b] -> [Tuple a b]
cartesianProduct as bs = do
  a <- as
  b <- bs
  [Tuple a b]


pythagoreanTriples :: Number -> [[Number]]
pythagoreanTriples n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard $ a * a + b * b == c * c
  return [a, b, c]

-- factors' :: Number -> [[Number]]
-- factors' n =

-- factorizations :: Number -> [[Number]]
-- factorizations 1 = [[1]]
-- factorizations n = do
--   a <- factors n
--   return factorizations <$> head <$> tail a


allTrue :: [Boolean] -> Boolean
allTrue xs = foldl (&&) true xs

-- True for arrays of Boolean that have an number of occurances of false
ex2 :: forall a. [Boolean] -> Boolean
ex2 xs = foldl (==) false xs

