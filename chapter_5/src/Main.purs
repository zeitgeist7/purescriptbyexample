module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

-- main :: forall e. Eff (console :: CONSOLE | e) Unit
-- main = do
--   log "Hello sailor!"

gcd :: Int -> Int -> Int
gcd 0 n = n
gcd m 0 = m
gcd n m =
  if n > m
    then gcd (n - m) m
    else gcd n (m - n)

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