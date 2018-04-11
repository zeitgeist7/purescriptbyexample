module Main where
-- Other examples of module definition include:
  -- module My.First.Module where

import Control.Monad.Eff.Console
import Math(sqrt, pi)
import Prelude((*), (+))

diagonal :: Number -> Number -> Number
diagonal w h = sqrt(w*w + h*h)


-- Solutions to exercises
circleArea :: Number -> Number
circleArea r = pi * r * r

main = logShow (diagonal 3.0 4.0)

{-
Pulp can be used to convert our PureScript code into JavaScript 
to run on the web browser:
To do that run pulp browserify
-}
