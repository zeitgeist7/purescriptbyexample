module FileOperations where

import Prelude

import Data.Ord
import Data.Maybe (Maybe(..))
import Data.Path (Path, ls, isDirectory, size)
import Data.Array (concatMap, (:), filter, foldr, foldl, head)

import Data.Path
import Control.MonadZero (guard)

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

-- 1.
onlyFiles :: Path -> Array Path
onlyFiles = allFiles >>> filter isFile where isFile = not isDirectory

-- 2.
-- Lenses would be helpful here for comparison but need to master that
compare ::
  (Maybe Int -> Maybe Int -> Boolean)
  -> Path
  -> Maybe Path
  -> Maybe Path
compare comparator f1 (Just f2) =
  if comparator (size f1) (size f2)
  then Just f1
  else Just f2
compare _ f1 Nothing = Just f1

smaller = (compare (<))
larger = (compare (>))

smallest_largest_files :: Path -> Array (Maybe Path) -- Could have used a Tuple
-- Add an auxilliary functie
smallest_largest_files path = [
  path # onlyFiles >>> foldr smaller Nothing,
  path # onlyFiles >>> foldr larger Nothing
  ]
  -- '#' is like the pipe (|>)

whereIs :: String -> Maybe Path
whereIs fileToSearch = head $ do
  path <- allFiles' root
  child <- ls path
  guard $ filename child == fileToSearch
  [path]

-- Had some trouble with this one.
-- Credit:
-- https://github.com/kvsm/purescript-by-example/blob/master/chapter4/src/FileOperations.purs


-- To understand how array comprehensions work:
matrix = do
  i <- [1,2,3,4]
  j <- [4,3,2,1,0]
  [[i,j]]
{-
Outputs:
[
  [1,4],[1,3],[1,2],[1,1],[1,0],
  [2,4],[2,3],[2,2],[2,1],[2,0],
  [3,4],[3,3],[3,2],[3,1],[3,0],
  [4,4],[4,3],[4,2],[4,1],[4,0]
]
-}
