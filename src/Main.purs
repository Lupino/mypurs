module Main where

import Prelude hiding (add)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import MyFunc (add, add1, randInt, randInt1, maybeBigZero, eitherBigZero, readText, readTextAff, readTextAff1)
import Data.Either (Either (..))
import Control.Monad.Eff.Exception (message)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (launchAff_)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ show $ add 1 2
  log $ show $ add1 1 2
  r0 <- randInt 0 10
  r1 <- randInt1 0 10
  log $ show $ r0
  log $ show $ r1
  log $ show $ maybeBigZero 10
  log $ show $ eitherBigZero 10
  readText "bower.json" $ \r ->
    case r of
         Left e -> log $ message e
         Right s -> log s

  launchAff_ $ do
    r3 <- readTextAff "bower.json"
    liftEff $ log r3
    r4 <- readTextAff1 "bower.json"
    liftEff $ log r4
