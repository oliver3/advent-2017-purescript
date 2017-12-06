module Day04 where

import Prelude

import Control.MonadZero (guard)
import Data.Array (filter, length, mapWithIndex)
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..))

pairs :: forall a. Array a -> Array (Tuple a a)
pairs xs = do
    Tuple i x <- mapWithIndex Tuple xs
    Tuple j y <- mapWithIndex Tuple xs
    guard $ i /= j
    pure $ Tuple x y

isValid :: String -> Boolean
isValid = split (Pattern " ")
  >>> pairs
  >>> filter (\(Tuple a b) -> a == b)
  >>> length
  >>> (eq 0)

