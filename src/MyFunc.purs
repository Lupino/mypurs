module MyFunc
  ( add
  , add1
  , randInt
  , randInt1
  ) where

import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (Fn2, runFn2)

foreign import add :: Int -> Int -> Int
foreign import _add1 :: Fn2 Int Int Int

add1 :: Int -> Int -> Int
add1 a b = runFn2 _add1 a b

foreign import randInt :: forall eff. Int -> Int -> Eff eff Int
foreign import _randInt1 :: forall eff. Fn2 Int Int (Eff eff Int)

randInt1 :: forall eff. Int -> Int -> Eff eff Int
randInt1 a b = runFn2 _randInt1 a b
