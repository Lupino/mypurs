module MyMonad
  ( WhitelistM (..)
  , runWhitelistM
  , get
  , add
  , del
  ) where

import Prelude
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef, modifyRef)
import Data.Newtype (class Newtype)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff (Eff)
import Data.Array ((:), delete)

newtype WhitelistM eff a = WhitelistM (Ref (Array String) -> Eff eff a)

runWhitelistM :: forall eff a. WhitelistM (ref :: REF | eff) a -> Eff (ref :: REF | eff) a
runWhitelistM (WhitelistM f) = f =<< newRef []

derive instance newtypeWhitelistM :: Newtype (WhitelistM eff a) _

instance functorWhitilistM :: Functor (WhitelistM eff) where
  map f (WhitelistM g) = WhitelistM $ map f <<< g

instance applicativeWhitelistM :: Applicative (WhitelistM eff) where
  pure = WhitelistM <<< const <<< pure

instance monadWhitelistM :: Monad (WhitelistM eff)

instance bindWhitelistM :: Bind (WhitelistM eff) where
  bind (WhitelistM m) k = WhitelistM $ \ref -> do
     r <- m ref
     case k r of
       WhitelistM m0 -> m0 ref

instance applyWhitelistM :: Apply (WhitelistM eff) where
  apply = ap

instance monadEffWhitelistM :: MonadEff eff (WhitelistM eff) where
  liftEff = WhitelistM <<< const

add_ :: forall eff. String -> Ref (Array String) -> Eff (ref :: REF | eff) Unit
add_ s ref = modifyRef ref $ (s : _)

del_ :: forall eff. String -> Ref (Array String) -> Eff (ref :: REF | eff) Unit
del_ s ref = modifyRef ref $ delete s

get_ :: forall eff. Ref (Array String) -> Eff (ref :: REF | eff) (Array String)
get_ = readRef

add :: forall eff. String -> WhitelistM (ref :: REF | eff) Unit
add s = WhitelistM $ add_ s

del :: forall eff. String -> WhitelistM (ref :: REF | eff) Unit
del s = WhitelistM $ del_ s

get :: forall eff. WhitelistM (ref :: REF | eff) (Array String)
get = WhitelistM $ get_
