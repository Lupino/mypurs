module MyMonadTrans
  ( WhitelistT (..)
  , runWhitelistT
  , get
  , add
  , del
  ) where

import Prelude
import Effect.Ref (Ref, new, read, modify)
import Data.Newtype (class Newtype)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Trans.Class (class MonadTrans)
import Effect (Effect)
import Data.Array ((:), delete)

newtype WhitelistT m a = WhitelistT (Ref (Array String) -> m a)

runWhitelistT :: forall m a. MonadEffect m => WhitelistT m a -> m a
runWhitelistT (WhitelistT f) = f =<< liftEffect (new [])

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

instance monadEffectWhitelistT :: MonadEffect m => MonadEffect (WhitelistT m) where
  liftEffect = WhitelistT <<< const <<< liftEffect

instance monadAffWhitelistT :: MonadAff m => MonadAff (WhitelistT m) where
  liftAff = WhitelistT <<< const <<< liftAff

instance monadTransWhitelistT :: MonadTrans WhitelistT where
  lift = WhitelistT <<< const

add_ :: String -> Ref (Array String) -> Effect Unit
add_ s = void <<< modify (s : _)

del_ :: String -> Ref (Array String) -> Effect Unit
del_ s = void <<< modify (delete s)

get_ :: Ref (Array String) -> Effect (Array String)
get_ = read

add :: forall m. MonadEffect m => String -> WhitelistT m Unit
add s = WhitelistT $ liftEffect <<< add_ s

del :: forall m. MonadEffect m => String -> WhitelistT m Unit
del s = WhitelistT $ liftEffect <<< del_ s

get :: forall m. MonadEffect m => WhitelistT m (Array String)
get = WhitelistT $ liftEffect <<< get_
