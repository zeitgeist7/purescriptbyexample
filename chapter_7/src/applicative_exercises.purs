module Data.Applicative.Exercises where

import Prelude
import Data.Maybe
import Data.List
import Control.Apply

combineList :: forall a f. Applicative f => List (f a) -> f (List a)
combineList Nil = pure Nil
combineList (Cons x xs) = Cons <$> x <*> combineList xs

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
combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just wrapped) = Just <$> wrapped
{-
EXPLANATIE:
ˆˆˆˆˆˆˆˆˆˆˆ
The first pattern is easy and similar to `combineList`
  For the second, we need to consider the type signatures:
-> Just :: a -> Maybe a
-> map :: (a -> b) -> f a -> f b
therefore:

map Just :: f a -> f (Maybe a)

The line `combine (Just wrapped)` is actually `combine (Just (f a))`
where wrapped :: f a.

Which means in our case of we apply wrapped :: f a to `map Just`,
we will get f (Maybe a).

In other words, in the second pattern,
we know we have a `f a`,
and with `map` we drill inside of this functor (box),
grab that `a`, transform it with the `Maybe`'s constructor `Just`,
and return the same box `f` with a `Maybe a inside of it.`
-}
