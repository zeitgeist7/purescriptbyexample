module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

-- for guard
import Control.MonadZero (guard)

import Math((%))

import Data.Foldable (product, foldl, foldr)
import Data.Array (null, filter, (..), length, (:))

import Data.Array.Partial (tail, head)
import Partial.Unsafe (unsafePartial)

length1 :: forall a. Array a -> Int
length1 arr =
  if null arr
    then 0
    else 1 + length1 (unsafePartial tail arr)


-- main :: forall e. Eff (console :: CONSOLE | e) Unit
-- main = do
--   log "Hello sailor!"


{-
List does not have a map function but can use the <$> infx operator
For example : (\n -> n * 3) <$> 1..4
-}
-- Recursion exercises
isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven n = isEven (n - 2)

countEven :: Array Int -> Int
countEven [] = 0
countEven xs =
  currentContribution (unsafePartial head xs)
  + countEven (unsafePartial tail xs)
    where
    currentContribution :: Int -> Int
    currentContribution x =
      if isEven x then 1 else 0

-- Map, Filter Exercises
-- 1. calculate squares of an array of a number
squares :: Array Number -> Array Number
squares =  map (\n -> n * n)
-- If there is a typeclass that represents
-- Int and Number that would be greate!

-- Removenegative numbers
filterNegatives :: Array Number -> Array Number
filterNegatives = filter ((<=) 0.0)

-- infix synonym for filter
infix 8 filter as <$?>

anotherNegativeFilter :: Array Number -> Array Number
anotherNegativeFilter xs = ((<=) 0.0) <$?> xs


{-
That line: concatMap (\i -> 1 .. n) (1 .. n)
seems like a for-loop in imperative languages.
concatMap, as said before, will flatten the arrays. To see clearer:
Try map (\i -> 1 .. n) (1 .. n) for 3: [[1,2,3],[1,2,3],[1,2,3]]
- i is an element from the outer array: (1 .. n).

Note:
ˆˆˆˆˆ
map and concatMap are specialization of the more general
map and bind operators. map and concatMap are for array comprehensions
while map and bind are for monad comprehensions. 💥
-}

-- Writing factors using do notatie
factors1 :: Int -> Array (Array Int)
factors1 n = filter (\xs -> product xs == n) $ do
  i <- 1 .. n
  j <- i .. n
  pure [i,j]

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ n ==  i * j
    -- Seems that it works like Swift's guard
    -- Not sure if I see the correspondence with Haskell's
  pure [i, j]

-- Exercises: Array Comprehensions, do notatie, guards
-- ˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆ
{-
1.
If n is prime it means that the array
will contain only one array with a pair
-}
isPrime :: Int -> Boolean
-- isPrime 1 = false  -- this kind of pattern matching throws an error
isPrime = factors >>> length >>> ((==) 1)
-- try this: filter isPrime (1..100) -- Blow away

-- 2. Cartesian product
cartesianProduct :: Array Int -> Array Int -> Array (Array Int)
cartesianProduct xs ys = do
  i <- xs
  j <- ys
  [[i,j]]

-- 3. Pythagorean triple
triples :: Int -> Array (Array Int)
triples n = do
  i <- 1 .. n
  j <- i .. n
  k <- j .. n
  guard $ (i*i + j*j) == k*k
  [[i,j,k]]

-- 4. Factorizations
{-
-> take all the numbers from 1 to n
-> filter the primes
-> guard against the product that matches n
-- Coming up next
-}
{-
use factors then on all the pairs,
 find factors on each element on the inner array
 till they are reduced to prime nummers and that should be it.
 Might even need to consider kust [1,n]
-}
-- Will come back to that later with more maths knowledge


-- Folds, Tail Recursion, Accumulators
-- ˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆ

-- 1.
truthy :: Array Boolean -> Boolean
truthy = foldl (&&) true  -- foldl (==) true

-- 2. An array of Boolean that has AT LEAST ONE false value in it

-- 3. Tail recursive form
count :: forall a. (a -> Boolean) -> Array a -> Int
count _ [] = 0
count p xs = if p (unsafePartial head xs)
                then count p (unsafePartial tail xs)  + 1
                else count p (unsafePartial tail xs)

count' :: forall a. (a -> Boolean) -> Array a -> Int
count' = count'' 0
  where
    count'' acc _ [] = acc
    count'' acc p xs =
      if p (unsafePartial head xs)
      then count'' (1 + acc) p (unsafePartial tail xs)
      else count'' acc p (unsafePartial tail xs)

-- 4. reverse using foldl
reverse_using_foldl :: forall a. Array a -> Array a
reverse_using_foldl = foldl (\xs -> \x -> [x] <> xs) []