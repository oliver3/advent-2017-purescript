module Day02 where

import Prelude

import Control.MonadZero (guard)
import Data.Array (catMaybes, length, (!!), (..))
import Data.Foldable (maximum, minimum, sum)
import Data.Maybe (fromMaybe)

checksum :: Array (Array Int) -> Int
checksum = sum <<< map checksumRow
  where
    checksumRow :: Array Int -> Int
    checksumRow row = fromMaybe 0 do
      max <- maximum row
      min <- minimum row
      pure $ max - min

checksumDiv :: Array (Array Int) -> Int
checksumDiv = sum <<< map checksumRowDiv
  where
    checksumRowDiv :: Array Int -> Int
    checksumRowDiv row = sum $ catMaybes do
      i <- 0 .. (length row - 1)
      j <- 0 .. (length row - 1)
      guard $ i /= j
      pure $ do
        n <- row !! i
        m <- row !! j
        guard $ mod n m == 0
        pure $ n / m
