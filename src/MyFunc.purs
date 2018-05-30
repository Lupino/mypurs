module MyFunc
  ( add
  , add1
  , randInt
  , randInt1
  , maybeBigZero
  , eitherBigZero
  , readText
  , readTextAff
  , readTextAff1
  , readTextAff2
  ) where

import Prelude
import Effect.Exception (Error)
import Data.Maybe (Maybe (..))
import Data.Either(Either (..))
import Effect (Effect)
import Data.Function.Uncurried (Fn2, runFn2)
import Effect.Aff (Aff, makeAff, nonCanceler, Canceler)
import Effect.Aff.Compat (fromEffectFnAff, EffectFnAff)
import Effect.Class (liftEffect)
import Control.Promise (Promise, toAff)

foreign import add :: Int -> Int -> Int
foreign import _add1 :: Fn2 Int Int Int

add1 :: Int -> Int -> Int
add1 a b = runFn2 _add1 a b

foreign import randInt :: Int -> Int -> Effect Int
foreign import _randInt1 :: Fn2 Int Int (Effect Int)

randInt1 :: Int -> Int -> Effect Int
randInt1 a b = runFn2 _randInt1 a b

foreign import _maybeBigZero :: Int -> Maybe Int -> (Int -> Maybe Int) -> Maybe Int
foreign import _eitherBigZero :: Int -> (String -> Either String Int) -> (Int -> Either String Int) -> Either String Int

maybeBigZero :: Int -> Maybe Int
maybeBigZero a = _maybeBigZero a Nothing Just

eitherBigZero :: Int -> Either String Int
eitherBigZero a = _eitherBigZero a Left Right

foreign import _readText
  :: String
  -> (Error -> Either Error String)
  -> (String -> Either Error String)
  -> (Either Error String -> Effect Unit)
  -> Effect Unit

readText :: String -> (Either Error String -> Effect Unit) -> Effect Unit
readText fn = _readText fn Left Right

readTextAff :: String -> Aff String
readTextAff fn = makeAff wrapper
  where wrapper :: (Either Error String → Effect Unit) → Effect (Canceler)
        wrapper cb = readText fn cb $> nonCanceler

foreign import _readTextAff1 :: String -> EffectFnAff String

readTextAff1 :: String -> Aff String
readTextAff1 = fromEffectFnAff <<< _readTextAff1

foreign import _readTextAff2 :: String -> Effect (Promise String)

readTextAff2 :: String -> Aff String
readTextAff2 fn = liftEffect (_readTextAff2 fn) >>= toAff
