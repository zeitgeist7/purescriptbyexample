module Data.Applicative.Exercises where

import Prelude
import Data.Maybe
import Data.Field
import Control.Apply

-- 1.
data Operatie = Addition | Subtraction | Multiplication | Division

liftedOperatie :: Operatie -> Maybe Number -> Maybe Number -> Maybe Number
liftedOperatie = lift2 <<< op
  where
    op :: Operatie -> Number -> Number -> Number
    op Addition = (+)
    op Subtraction = (-)
    op Multiplication = (*)
    op Division = (/)

-- Could have defined another one for `Int`
-- but that is left as an exercise for my future self.

-- 2.
-- See Notes.md for an in-depth answer

-- 3.
