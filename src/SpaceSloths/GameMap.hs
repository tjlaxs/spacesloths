module SpaceSloths.GameMap where

import Protolude

import Data.Array (Array)

data Cell
  = Vacuum
  | Wall
  | Air
  deriving (Show, Eq, Ord)

charToCell c =
  case c of
    ' ' -> Vacuum
    '.' -> Air
    '#' -> Wall

data GameMap
  = GameMap
  { mapWidth    :: Int
  , mapHeight   :: Int
  , mapContents :: Array (Int, Int) Cell
  } deriving Show
