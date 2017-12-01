module Day01 (captcha, captchaHalfway) where

import Prelude

import Data.Array (drop, elemIndex, take, zipWith)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Data.String (length, toCharArray)

captcha :: String -> Int
captcha str = captcha' 1 str

captchaHalfway :: String -> Int
captchaHalfway str = captcha' (length str / 2) str

captcha' :: Int -> String -> Int
captcha' offset str =
  sum equalNumbers
  where
    numbers = toCharArray str <#> charToInt
    numbers' = rotate offset numbers
    equalNumbers = zipWith (\n n' -> if n == n' then n else 0) numbers numbers'

rotate :: forall a. Int -> Array a -> Array a
rotate n xs = (drop n xs) <> (take n xs)

charToInt :: Char -> Int
charToInt c =
  case elemIndex c (toCharArray "0123456789") of
    Just i -> i
    Nothing -> 0
