module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Array (null, filter)
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