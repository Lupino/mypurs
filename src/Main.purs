module Main where

import Prelude hiding (add)
import Effect (Effect)
import Effect.Console (log)
import MyFunc (add, add1, randInt, randInt1, maybeBigZero, eitherBigZero, readText, readTextAff, readTextAff1, readTextAff2)
import Data.Either (Either (..))
import Effect.Exception (message)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff (launchAff_)
import MyMonad as MyMonad
import MyMonad (WhitelistM, runWhitelistM)
import MyMonadTrans as MyMonadT
import MyMonadTrans (WhitelistT, runWhitelistT)

logWhitelistM :: WhitelistM Unit
logWhitelistM = MyMonad.get >>= (liftEffect <<< log <<< show)

logWhitelistT :: forall m. MonadEffect m => WhitelistT m Unit
logWhitelistT = MyMonadT.get >>= (liftEffect <<< log <<< show)

main :: Effect Unit
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
    liftEffect $ log "test readTextAff"
    r3 <- readTextAff "bower.json"
    liftEffect $ log r3
    liftEffect $ log "test readTextAff1"
    r4 <- readTextAff1 "bower.json"
    liftEffect $ log r4
    liftEffect $ log "test readTextAff2"
    r5 <- readTextAff2 "bower.json"
    liftEffect $ log r5

  runWhitelistM $ do
    MyMonad.add "test0"
    MyMonad.add "test1"
    MyMonad.add "test2"
    logWhitelistM
    MyMonad.add "test3"
    logWhitelistM
    MyMonad.del "test2"
    logWhitelistM

  runWhitelistT $ do
    MyMonadT.add "test0 trans Effect"
    MyMonadT.add "test1 trans Effect"
    MyMonadT.add "test2 trans Effect"
    logWhitelistT
    MyMonadT.add "test3 trans Effect"
    logWhitelistT
    MyMonadT.del "test2 trans Effect"
    logWhitelistT
  launchAff_ $ runWhitelistT $ do
    MyMonadT.add "test0 trans Aff"
    MyMonadT.add "test1 trans Aff"
    MyMonadT.add "test2 trans Aff"
    logWhitelistT
    MyMonadT.add "test3 trans Aff"
    logWhitelistT
    MyMonadT.del "test2 trans Aff"
    logWhitelistT
