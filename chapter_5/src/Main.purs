module Main where

import Prelude

-- import Control.Monad.Eff (Eff)
-- import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Foldable(sum)
import Data.Array.Partial (tail)
import Partial.Unsafe (unsafePartial)

import Data.Picture (Point(Point), Shape(Circle))-- (Point(..), Shape(..), Picture, bounds, showBounds)

-- main :: forall e. Eff (console :: CONSOLE | e) Unit
-- main = do
--   log "Hello sailor!"

gcd' :: Int -> Int -> Int
gcd' 0 n = n
gcd' m 0 = m
gcd' n m =
  if n > m
    then gcd' (n - m) m
    else gcd' n (m - n)

{-
  Credit: https://matheducators.stackexchange.com/questions/12103/greatest-common-divisor-applications

Here's a word problem for the greatest common divisor:

12 boys and 15 girls are to march in a parade.
The organizer wants them to march in rows,
with each row having the same number of children,
and with each row composed of children with the same gender.
What is the largest number of children per row that satisfies these constraints?
ˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆ
There should be gcd(12,15) = 3
ˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆ
gcd(12,15) = 3 children per row (4 rows of boys and 5 rows of girls).
ˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆ

Also, I know that you didn't ask for it,
but when I teach the gcd I also teach the least common multiple.
Here's a word problem for the lcm.

A child is having a birthday party and knows that either 12 or 15 guests would attend it.
The child's father wants to buy candies for the guests,
with each guest having the same number of candies,
but doesn't want to buy more candies than is necessary.
What is the minimum number of candies that the father should buy?
ˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆ
The father should buy lcm(12,15)=60
ˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆ
lcm⁡(12,15) = 60 candies,
so that if 12 friends come, they each get 5 candies,
and if 15 come, they each get 4
-}

-- Exercises: Pattern Matching & Guards
-- ˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆ

-- 1.
fact :: Int -> Int
-- Could have been done with tail recursie
fact 0 = 1
fact n = n * fact (n-1)

-- 2.
-- Will come back to that later


-- Array Patterns
-- ˆˆˆˆˆˆˆˆˆˆˆˆˆˆ

isEmpty :: forall a. Array a -> Boolean
isEmpty [] = false
isEmpty _ = true

-- Record Patterns and Row Polymorphism
-- ˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆ

showPerson :: { first :: String, last :: String } -> String
showPerson { first: firstName, last: lastName } = firstName <> ", " <> lastName

-- showPerson' { first: firstName, last: lastName } = firstName <> ", " <> lastName

-- Nested Patterns
-- ˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆ
type Address = { street :: String, city :: String, country :: String }
type Person = { first:: String, last :: String, address :: Address }

livesInMauritius :: Person -> Boolean
-- livesInMauritius { first: _, last: _, location: {street: _, city: _, country: "Mauritius" } } = true
livesInMauritius { address: { country: "Mauritius" } } = true -- Short and sweet form
livesInMauritius _ = false

-- Named Patterns
-- ˆˆˆˆˆˆˆˆˆˆˆˆˆˆ
sortPair :: Array Int -> Array Int
sortPair arr@[x,y]
  | x <= y = arr
  | otherwise = [y,x]
sortPair arr = arr
-- This one packs a punch, need more examples for the idea to gel

-- Exercises
-- ˆˆˆˆˆˆˆˆˆ
--1.
sameCity :: Person -> Person -> Boolean
sameCity { address: {city: city1}} { address: {city: city2}} = city1 == city2

-- 2.
-- Got it by commenting the function definition and doing :t sameCity, this is cheating
-- forall t22 t25 t28 t31 t33. Eq t33 =>
--       { address :: { city :: t33 | t25 } | t22 }
--   ->  { address :: { city :: t33 | t31 } | t28 }
--   ->  Boolean

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [x] = x
fromSingleton default _ = default

-- Case expressions
lzs :: Array Int -> Array Int
lzs xs = case sum xs of
  0 -> xs
  _ -> lzs (unsafePartial tail xs)

-- Exercises
-- ˆˆˆˆˆˆˆˆˆ
-- 1.
c1 :: Shape
c1 = Circle origin 10.0
  where
    origin = Point { x, y }
    x = 0.0
    y = 0.0