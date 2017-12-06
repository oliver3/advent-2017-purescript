module Day05 where

import Prelude

import Data.Array (updateAt, (!!))
import Data.Maybe (Maybe(..), fromMaybe)

escape :: Array Int -> Int
escape = go 0 0
  where
    go :: Int -> Int -> Array Int -> Int
    go ip jumps instructions =
      case instructions !! ip of
        Nothing -> jumps
        Just instruction ->
          let newInstructions = fromMaybe instructions $ updateAt ip (instruction + 1) instructions
          in go (ip + instruction) (jumps + 1) newInstructions

