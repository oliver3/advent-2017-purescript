module Day04 where

import Prelude

import Control.MonadZero (guard)
import Data.Array (filter, length, mapWithIndex, sort)
import Data.String (Pattern(..), split, toCharArray)
import Data.Tuple (Tuple(..))

pairs :: forall a. Array a -> Array (Tuple a a)
pairs xs = do
    Tuple i x <- mapWithIndex Tuple xs
    Tuple j y <- mapWithIndex Tuple xs
    guard $ i /= j
    pure $ Tuple x y

isValid :: String -> Boolean
isValid = isValid' (==)

isValidAnagram :: String -> Boolean
isValidAnagram = isValid' isAnagram

isAnagram :: String -> String -> Boolean
isAnagram x y = sort (toCharArray x) == sort (toCharArray y)

isValid' :: (String -> String -> Boolean) -> String -> Boolean
isValid' same = split (Pattern " ")
  >>> pairs
  >>> filter (\(Tuple a b) -> a `same` b)
  >>> length
  >>> (eq 0)

