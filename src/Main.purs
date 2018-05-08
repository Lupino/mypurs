module Main where

import Prelude hiding (add)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import MyFunc (add, add1, randInt, randInt1, maybeBigZero, eitherBigZero)

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
  log "Hello sailor!"
