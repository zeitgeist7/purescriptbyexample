module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Picture (Point(..), Shape(..), Picture)
import Data.Foldable (class Foldable, foldMap, foldl, foldr, maximum)

import Data.Array((:))
import Data.Array as Array
import Data.Array.Partial as Partial

import Data.List (List(..), filter, head, null, nubBy)

import Data.Maybe

import Data.Monoid (class Monoid, mempty)


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"


{-
`show` is defined in the Prelude module called Show
The definition might look like this:

class Show a where
  show :: a -> String
-}

-- Testing understanding:
class Define a where
  define :: a -> Int

data Direction = North | East | South | West

instance defineDirection :: Define Direction where
  define North = 0
  define East = 1
  define South = 2
  define West = 3

-- Exercise
-- 1.
-- Test data

-- Defined a Show instance for Point also

origin :: Point
origin = Point { x, y }
  where
    x = 0.0
    y = 0.0

cricle1 :: Shape
cricle1 = Circle origin 10.0

picture1 = [
  Line origin (Point {x: 1.0, y: 2.0}),
  Text (Point {x: 100.0, y: 100.0}) "Bonzy Paro Gamegame Boule et Ti-Ozoz",
  Circle (Point {x: -200.0, y: -25.0}) 30.0
] :: Picture

clipped = Clipped picture1 (Rectangle origin 25.0 25.0) :: Shape

-- Fields
{-
Field type identifies those types which support numeric operations such
addtion, subtraction, multiplication and division
-}

-- Exercises
-- 1.
data Complex = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex { real, imaginary }) = show real <> stringifyImaginary imaginary <> "i"
    where
      stringifyImaginary :: Number -> String
      stringifyImaginary i =
        if i < 0.0
        then " - " <> show (negate i)
        else " + " <> show i

instance equalComplex :: Eq Complex where
  eq (Complex c1) (Complex c2) = c1.real == c2.real && c1.imaginary == c2.imaginary

complex1 = Complex { real: 2.0, imaginary: 1.0 } :: Complex
complex2 = Complex { real:  negate 2.0, imaginary: negate 1.0 } :: Complex

-- Notes about showCompare
-- Tried to implement the same functie using guards, 💂‍
showCompare :: forall a. Ord a => Show a => a -> a -> String
showCompare a1 a2
  | a1 < a2 = show a1 <> " is less than " <> show a2
  | a1 > a2 = show a1 <> " is greater than " <> show a2
  | otherwise = show a1 <> " is equal to " <> show a2

-- Exercises
{-
Test data:
ˆˆˆˆˆˆˆˆˆˆ
ne1 = NonEmpty "1" (["1"]) :: NonEmpty String
ne2 = NonEmpty "2" (["1","2"]) :: NonEmpty String
ne3 = NonEmpty "3" (["1", "10", "7"]) :: NonEmpty String
-}
data NonEmpty a = NonEmpty a (Array a)
{-
`a` is a type argument, for example here is the type constructor for `Maybe`
data Maybe a = Nothing | Just a
This is a non empty array since we need to pass at least
the first element, typed a
then an array (empty or not), typed Array a
These are values that are possible:
> NonEmpty 7 []
> NonEmpty "Bonzy" []
> NonEmpty 10 [7, 9, 23]
> NonEmpty "Bonzy" ["Paro", "Gympy"]
-}
-- Some functions to work on NonEmpty Int
incr :: Int -> Int
incr n = n + 1

square :: Int -> Int
square n = n * n

-- Bonus
instance showNonEmpty :: (Show a) => Show (NonEmpty a) where
  show (NonEmpty x xs) = show x <> " followed by " <> show xs

-- 1.
instance equatableNonEmpty :: (Eq a) => Eq (NonEmpty a) where
  eq (NonEmpty x xs) (NonEmpty y ys) = (x == y && xs == ys) -- ==
-- Tough this one!

-- 2.
-- Int does not form a semigroup
instance semiGroupNonEmpty :: Semigroup a => Semigroup (NonEmpty a) where
  append (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x (xs <> [y] <> ys) -- <>

-- 3.
instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty x xs) = NonEmpty (f x) (map f xs) -- <$>

-- ˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆ
data Extended a = Finite a | Infinite
-- ˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆ
-- 4.
instance orderingExtended :: (Ord a) => Ord (NonEmpty a) where
  compare (NonEmpty x (xs)) (NonEmpty y (ys))
    | x < y = LT
    | x > y = GT
    | otherwise = compare xs ys

-- 5.
instance foldableNonEmpty :: Foldable NonEmpty where
  -- Do not know of (:) is less performant
  foldr f y (NonEmpty x xs) = foldr f y (x:xs)
  foldl f y (NonEmpty x xs) = foldl f y (x:xs)
  foldMap f (NonEmpty x xs) = foldMap f (x:xs)

-- ˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆ
data OneMore f a = OneMore a (f a)
-- ˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆ
-- 6.
-- NOTE: We are pattern matching on (OneMore a c) where
--   c represents `f a`
--   f is the foldable ordered container
instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldr f' y (OneMore x xs) = f' x (foldr f' y xs)
  foldl f' y (OneMore x xs) = foldl f' (f' y x) xs
  foldMap f' (OneMore a xs) = f' a <> (foldMap f' xs)
{-
Explanation for each case:
ˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆ

ˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆ
foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
ˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆ
We need to fold from the right. Since in `OneMore a (f a)`,
we have an `x` at the front (exercise specs), we can foldr on the (ordered) container, c, (f a), first
hence: (foldr f' y xs).
(foldr f' y xs) :: `b` since we used `f'` for the foldr.
Let partialResult = (foldr f' y xs) :: b
We are left with an `x`, and the partial result from above (:: `b`).
We can complete the fold with `f' x partialResult` that will give a reduced value,
of type `b`. DONE!

ˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆ
foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
ˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆ
We are folding from the left starting with `y` which is of type `b`.
Then we have a front element `x` which is of type `a`.
We can use the function `f'` on y and x, f' y x to get an element of type `b`.
Since we are now left with xs, we can just foldl this ordered container. DONE!

ˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆ
foldMap :: forall a, m. Monoid m => (a -> m) -> f a -> m
ˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆ
Same thing, and simpler; we apply `f'` on x and we concat the result
with the `foldMap` of xs using `f'` since xs is an ordered container (foldable). DONE!
-}

-- Exercises (page 78)
-- ˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆˆ
-- 1.
max' :: Partial => Array Int -> Int
max' = maximum >>> force_unwrap
  where
    force_unwrap :: Partial => Maybe Int -> Int
    force_unwrap (Just x) = x
{-
Explanation
ˆˆˆˆˆˆˆˆˆˆˆ
Data.Foldable.maximum returns a Maybe Int,
therefore it is forward composed with the auxiliary,
force_unwrap that is itself partial (does not handle the Nothing case).
To use it in the REPL;

> import Partial.Unsafe
> unsafePartial max' youArray
> -- if yourArray is empty it will throw an error
-}

-- 2.
class Monoid m <= Activity m a where
  act :: m -> a -> a