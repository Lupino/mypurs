module MyFunc
  ( add
  , add1
  , randInt
  , randInt1
  , maybeBigZero
  , eitherBigZero
  , readText
  ) where

import Prelude
import Control.Monad.Eff.Exception (Error)
import Data.Maybe (Maybe (..))
import Data.Either(Either (..))
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

foreign import _maybeBigZero :: Int -> Maybe Int -> (Int -> Maybe Int) -> Maybe Int
foreign import _eitherBigZero :: Int -> (String -> Either String Int) -> (Int -> Either String Int) -> Either String Int

maybeBigZero :: Int -> Maybe Int
maybeBigZero a = _maybeBigZero a Nothing Just

eitherBigZero :: Int -> Either String Int
eitherBigZero a = _eitherBigZero a Left Right

foreign import _readText
  :: forall eff. String
  -> (Error -> Either Error String)
  -> (String -> Either Error String)
  -> (Either Error String -> Eff eff Unit)
  -> Eff eff Unit

readText :: forall eff. String -> (Either Error String -> Eff eff Unit) -> Eff eff Unit
readText fn = _readText fn Left Right
