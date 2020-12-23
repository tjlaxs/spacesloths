module SpaceSloths.GameMap where

import Protolude

import Data.Array (Array)

data Cell
  = Vacuum
  | Air
  | Wall
  deriving Show

charToCell c =
  case c of
    ' ' -> Vacuum
    '.' -> Air
    '#' -> Wall

data GameMap
  = GameMap
  { mapWidth    :: Int
  , mapHeight   :: Int
  , mapContents :: Array Int Cell
  } deriving Show
