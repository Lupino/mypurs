module MyMonadTrans
  ( WhitelistT (..)
  , runWhitelistT
  , get
  , add
  , del
  ) where

import Prelude
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef, modifyRef)
import Data.Newtype (class Newtype)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Trans.Class (class MonadTrans)
import Control.Monad.Eff (Eff)
import Data.Array ((:), delete)

newtype WhitelistT m a = WhitelistT (Ref (Array String) -> m a)

runWhitelistT :: forall eff m a. MonadEff (ref :: REF| eff) m => WhitelistT m a -> m a
runWhitelistT (WhitelistT f) = f =<< liftEff (newRef [])

derive instance newtypeWhitelistT :: Newtype (WhitelistT m a) _

instance functorWhitelistT :: Functor m => Functor (WhitelistT m) where
  map f (WhitelistT g) = WhitelistT $ map f <<< g

instance applicativeWhitelistT :: Monad m => Applicative (WhitelistT m) where
  pure = WhitelistT <<< const <<< pure

instance monadWhitelistT :: Monad m => Monad (WhitelistT m)

instance bindWhitelistT :: Monad m => Bind (WhitelistT m) where
  bind (WhitelistT m) k = WhitelistT $ \ref -> do
     r <- m ref
     case k r of
       WhitelistT m0 -> m0 ref

instance applyWhitelistT :: Monad m => Apply (WhitelistT m) where
  apply = ap

instance monadEffWhitelistT :: MonadEff eff m => MonadEff eff (WhitelistT m) where
  liftEff = WhitelistT <<< const <<< liftEff

instance monadAffWhitelistT :: MonadAff eff m => MonadAff eff (WhitelistT m) where
  liftAff = WhitelistT <<< const <<< liftAff

instance monadTransWhitelistT :: MonadTrans WhitelistT where
  lift = WhitelistT <<< const

add_ :: forall eff. String -> Ref (Array String) -> Eff (ref :: REF | eff) Unit
add_ s ref = modifyRef ref $ (s : _)

del_ :: forall eff. String -> Ref (Array String) -> Eff (ref :: REF | eff) Unit
del_ s ref = modifyRef ref $ delete s

get_ :: forall eff. Ref (Array String) -> Eff (ref :: REF | eff) (Array String)
get_ = readRef

add :: forall eff m. MonadEff (ref :: REF | eff) m => String -> WhitelistT m Unit
add s = WhitelistT $ liftEff <<< add_ s

del :: forall eff m. MonadEff (ref :: REF | eff) m => String -> WhitelistT m Unit
del s = WhitelistT $ liftEff <<< del_ s

get :: forall eff m. MonadEff (ref :: REF | eff) m => WhitelistT m (Array String)
get = WhitelistT $ liftEff <<< get_
