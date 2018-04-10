module Main where
-- Other examples of module definition include:
  -- module My.First.Module where

import Control.Monad.Eff.Console
import Math(sqrt)
import Prelude

diagonal w h = sqrt(w*w + h*h)

main = logShow (diagonal 3.0 4.0)

{-
Pulp can be used to convert our PureScript code into JavaScript 
to run on the web browser:
To do that run pulp browserify
-}
