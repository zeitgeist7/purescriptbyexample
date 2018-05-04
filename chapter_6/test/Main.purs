module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import Data.List (List(..), (:))
import Stream (foldstream)
import Main-- (Multiply, act)
import Data.Monoid(mempty)

-- Stream tests
assert :: forall a. Eq a => String -> a -> a -> String
assert message a b
  | a == b = "✅ :" <> message
  | otherwise = "❌ :" <> message

{-
Below are a list of streams and functions that take
the elements of each type of these streams and transforms them into a monoidal element
A list of non-exhaustive tests are there. Lots of things to play with.
-}
-- Array streams
stream1 = [1,2,3,4] :: Array Int
stream2 = [Just 7, Just 9, Nothing, Just 23, Just 10, Nothing] :: Array (Maybe Int)
stream3 = ["Bonzy", "Paro", "Gympy"] :: Array String
stream4 = [Just "Bonzy, ", Just "Paro, ", Just "Gamegame, ", Nothing, Just "Boule, ", Just "Ti-Ozoz"] :: Array (Maybe String)
-- String stream
stream5 = "Bonzy Paro Gympy" :: String
-- List streams
stream6 = (1:2:Nil) :: List Int
stream7 = (Just 7 : Just 9 : Just 23 : Just 10 : Nothing : Nil) :: List (Maybe Int)
stream8 = ("Bonzy, " : "Paro, " : "Gympy" : Nil) :: List String
stream9 = (Just "Bonzy, " : Just "Paro, " : Just "Gamegame, " : Nothing:Just "Boule, " : Just "Ti-Ozoz" : Nil) :: List (Maybe String)

int_to_string_monoid :: Int -> String
int_to_string_monoid n = show $ n * 2

int_to_array_monoid :: Int -> Array Int
int_to_array_monoid = pure

int_to_list_monoid :: Int -> List Int
int_to_list_monoid n = n : Nil

maybe_int_to_string_monoid :: Maybe Int -> String
maybe_int_to_string_monoid Nothing = "Nothing" <> ", "
maybe_int_to_string_monoid (Just n) = "Nummer: " <> int_to_string_monoid n <> ", "

maybe_int_to_array_monoid :: Maybe Int -> Array Int
maybe_int_to_array_monoid Nothing = []
maybe_int_to_array_monoid (Just n) = pure n

maybe_int_to_list_monoid :: Maybe Int -> List Int
maybe_int_to_list_monoid Nothing = Nil
maybe_int_to_list_monoid (Just n) = n : Nil

string_to_string_monoid :: String -> String
string_to_string_monoid s = s <> "-monoid, "

string_to_array_monoid :: String -> Array String
string_to_array_monoid = pure

string_to_list_monoid :: String -> List String
string_to_list_monoid n = n : Nil

maybe_string_to_string_monoid :: Maybe String -> String
maybe_string_to_string_monoid Nothing = "Nothing"
maybe_string_to_string_monoid (Just n) = "Just: " <> string_to_string_monoid n

maybe_string_to_array_monoid :: Maybe String -> Array String
maybe_string_to_array_monoid Nothing = []
maybe_string_to_array_monoid (Just str) = pure str

maybe_string_to_list_monoid :: Maybe String -> List String
maybe_string_to_list_monoid Nothing = Nil
maybe_string_to_list_monoid (Just str) = str : Nil

emptyMultiply = mempty :: Multiply

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  -- Array a tests
  -- stream1 = [1,2,3,4] :: Array Int
  log $ assert "folding Array Int with `Int -> String`"
    (foldstream int_to_string_monoid stream1) "2468"
  log $ assert "folding Array Int with `Int -> Array Int`"
    (foldstream int_to_array_monoid stream1) [1,2,3,4]
  log $ assert "folding Array Int with `Int -> List Int`"
    (foldstream int_to_list_monoid stream1) (1 : 2 : 3 : 4 : Nil)
  -- stream2 = [Just 7, Just 9, Nothing, Just 23, Just 10, Nothing]
  log $ assert "folding Array (Maybe Int) with `Int -> String`"
    (foldstream maybe_int_to_string_monoid stream2)
    "Nummer: 14, Nummer: 18, Nothing, Nummer: 46, Nummer: 20, Nothing, "
  log $ assert "folding Array (Maybe Int) with `Int -> Array Int`"
    (foldstream maybe_int_to_array_monoid stream2) [7,9,23,10]
  log $ assert "folding Array (Maybe Int) with `Int -> List Int`"
    (foldstream maybe_int_to_list_monoid stream2) (7 : 9 : 23 : 10 : Nil)
  -- stream3 = ["Bonzy", "Paro", "Gympy"]
  log $ assert "folding Array String with `String -> String"
    (foldstream string_to_string_monoid stream3) "Bonzy-monoid, Paro-monoid, Gympy-monoid, "
  -- Exerccise tests about Multiply, Activity, etc (page 78)
  log $ assert "First law: `act mempty a = a`"
    (act (mempty :: Multiply) "Bonzy ") "Bonzy "
    -- Had to type annotate mempty for the compiler to be happy
    -- The book covers this on page 75
  log $ assert "Second law: act (m1 <> m2) a = act m1 (act m2 a)"
    (act ((Multiply 2) <> (Multiply 3)) "Paro ")
    (act (Multiply 2) (act (Multiply 3) "Paro "))