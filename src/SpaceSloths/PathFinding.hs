module SpaceSloths.PathFinding where

import Protolude

import SpaceSloths.GameMap
import SpaceSloths.Sloth

data Node = Node (Int, Int) deriving Show

type Path = [Node]

findPath :: GameMap -> Sloth -> (Int, Int) -> Maybe Path
findPath m (Sloth n s _) t = Nothing
