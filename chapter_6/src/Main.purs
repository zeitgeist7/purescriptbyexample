module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Picture (Point(..), Shape(..), Picture)

-- main :: forall e. Eff (console :: CONSOLE | e) Unit
-- main = do
--   log "Hello sailor!"


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

{-}
instance showPoint :: Show Point where
  show (Point { x, y }) = "(" <> show x <> ", " <> show y <> ")"
    -}

c1 = Complex { real: 2.0, imaginary: 1.0 } :: Complex
c2 = Complex { real:  negate 2.0, imaginary: negate 1.0 } :: Complex