module Stream where

import Prelude
import Data.Array as Array
import Data.Maybe
import Data.String as String
import Data.Monoid (class Monoid, mempty)
import Data.Semigroup (class Semigroup, append)
import Data.List as List
import Data.List (List(..), (:))

class Stream stream element where
  uncons :: stream -> Maybe { head :: element, tail :: stream }

instance streamArray :: Stream (Array a) a where
  uncons  = Array.uncons

instance stringStream :: Stream String Char where
  uncons = String.uncons

instance listStream :: Stream (List.List a) a where
  uncons = List.uncons

-- Implementatie of foldstream
foldstream :: forall l e m. Stream l e => Monoid m => (e -> m) -> l -> m
foldstream f list =
  case uncons list of
    Nothing -> mempty
    Just cons -> f cons.head <> foldstream f cons.tail
-- See test file for the related tests
