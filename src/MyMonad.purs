module MyMonad
  ( WhitelistM (..)
  , runWhitelistM
  , get
  , add
  , del
  ) where

import Prelude
import Effect.Ref (Ref, new, read, modify)
import Data.Newtype (class Newtype)
import Effect.Class (class MonadEffect, liftEffect)
import Effect (Effect)
import Data.Array ((:), delete)

newtype WhitelistM a = WhitelistM (Ref (Array String) -> Effect a)

runWhitelistM :: forall a. WhitelistM a -> Effect a
runWhitelistM (WhitelistM f) = f =<< new []

derive instance newtypeWhitelistM :: Newtype (WhitelistM a) _

instance functorWhitilistM :: Functor WhitelistM where
  map f (WhitelistM g) = WhitelistM $ map f <<< g

instance applicativeWhitelistM :: Applicative WhitelistM where
  pure = WhitelistM <<< const <<< pure

instance monadWhitelistM :: Monad WhitelistM

instance bindWhitelistM :: Bind WhitelistM where
  bind (WhitelistM m) k = WhitelistM $ \ref -> do
     r <- m ref
     case k r of
       WhitelistM m0 -> m0 ref

instance applyWhitelistM :: Apply WhitelistM where
  apply = ap

instance monadEffectWhitelistM :: MonadEffect WhitelistM where
  liftEffect = WhitelistM <<< const

add_ :: String -> Ref (Array String) -> Effect Unit
add_ s = void <<< modify (s : _)

del_ :: String -> Ref (Array String) -> Effect Unit
del_ s = void <<< modify (delete s)

get_ :: Ref (Array String) -> Effect (Array String)
get_ = read

add :: String -> WhitelistM Unit
add s = WhitelistM $ add_ s

del :: String -> WhitelistM Unit
del s = WhitelistM $ del_ s

get :: WhitelistM (Array String)
get = WhitelistM $ get_
