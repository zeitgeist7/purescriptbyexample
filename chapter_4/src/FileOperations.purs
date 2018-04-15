module FileOperations where

import Prelude

import Data.Path (Path, ls)
import Data.Array (concatMap, (:))

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

-- Author says that
-- solutions to the exercises can be completed in this file