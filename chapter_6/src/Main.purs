module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Picture

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

c1 :: Shape
c1 = Circle origin 10.0

picture1 = [
  Line origin (Point {x: 1.0, y: 2.0}),
  Text (Point {x: 100.0, y: 100.0}) "Bonzy Paro Gamegame Boule et Ti-Ozoz",
  Circle (Point {x: -200.0, y: -25.0}) 30.0
]

clipped = Clipped picture1 (Rectangle origin 25.0 25.0)
