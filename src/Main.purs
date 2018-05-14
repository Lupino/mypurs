module Main where

import Prelude hiding (add)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import MyFunc (add, add1, randInt, randInt1, maybeBigZero, eitherBigZero, readText, readTextAff, readTextAff1, readTextAff2)
import Data.Either (Either (..))
import Control.Monad.Eff.Exception (message)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Aff (launchAff_)
import MyMonad as MyMonad
import MyMonad (WhitelistM, runWhitelistM)
import MyMonadTrans as MyMonadT
import MyMonadTrans (WhitelistT, runWhitelistT)
import Control.Monad.Eff.Ref (REF)

logWhitelistM :: forall eff. WhitelistM (ref :: REF, console :: CONSOLE | eff) Unit
logWhitelistM = MyMonad.get >>= (liftEff <<< log <<< show)

logWhitelistT :: forall eff m. MonadEff (ref :: REF, console :: CONSOLE | eff) m => WhitelistT m Unit
logWhitelistT = MyMonadT.get >>= (liftEff <<< log <<< show)

main :: forall e. Eff (ref :: REF, console :: CONSOLE | e) Unit
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
    liftEff $ log "test readTextAff"
    r3 <- readTextAff "bower.json"
    liftEff $ log r3
    liftEff $ log "test readTextAff1"
    r4 <- readTextAff1 "bower.json"
    liftEff $ log r4
    liftEff $ log "test readTextAff2"
    r5 <- readTextAff2 "bower.json"
    liftEff $ log r5

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
    MyMonadT.add "test0 trans Eff"
    MyMonadT.add "test1 trans Eff"
    MyMonadT.add "test2 trans Eff"
    logWhitelistT
    MyMonadT.add "test3 trans Eff"
    logWhitelistT
    MyMonadT.del "test2 trans Eff"
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
