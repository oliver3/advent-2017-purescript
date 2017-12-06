module Day03 where

import Prelude

import Data.Map (Map, insert, lookup, singleton)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (abs)
import Data.Tuple (Tuple(..))

type Position = Tuple Int Int
data Heading = North | East | South | West

type Turtle = { position :: Position, heading :: Heading }
type TurtleGrid = { turtle :: Turtle, grid :: Map Position Int }

newTurtle :: Turtle
newTurtle = { position: Tuple 0 0, heading: South }

newTurtleGrid :: TurtleGrid
newTurtleGrid = { turtle: newTurtle, grid: singleton (Tuple 0 0) 1}

read :: TurtleGrid -> Maybe Int
read {turtle, grid} = lookup turtle.position grid

write :: Int -> TurtleGrid -> TurtleGrid
write value {turtle, grid} = {turtle, grid: grid' }
  where grid' = insert turtle.position value grid

vector :: Heading -> Tuple Int Int
vector North = Tuple 0 (-1)
vector East = Tuple 1 0
vector South = Tuple 0 1
vector West = Tuple (-1) 0

left :: Heading -> Heading
left North = West
left East = North
left South = East
left West = South

look :: (Heading -> Heading) -> TurtleGrid -> Maybe Int
look rotation {turtle, grid} = lookup position grid
  where position = turtle.position + vector (rotation turtle.heading)

turn :: (Heading -> Heading) -> TurtleGrid -> TurtleGrid
turn rotation tg = tg {turtle {heading = rotation tg.turtle.heading}}

forward :: TurtleGrid -> TurtleGrid
forward tg = tg {turtle {position = position'}}
  where position' = tg.turtle.position + vector tg.turtle.heading

walkSpiral :: TurtleGrid -> TurtleGrid
walkSpiral turtlegrid =
  case look left turtlegrid of
    Nothing -> (turn left >>> forward) turtlegrid
    _ -> forward turtlegrid

createTurtleGrid :: (TurtleGrid -> Int) -> Int -> TurtleGrid
createTurtleGrid calc until = go newTurtleGrid
  where
    go :: TurtleGrid -> TurtleGrid
    go grid =
      if fromMaybe 0 (read grid) >= until
        then grid
        else go grid'
          where
            value' = calc grid
            grid' = (walkSpiral >>> write value') grid

increaseValue :: TurtleGrid -> Int
increaseValue tg = fromMaybe 0 (read tg) + 1

manhattan :: TurtleGrid -> Int
manhattan {turtle: {position: Tuple x y}} = abs x + abs y