module SpaceSloths.Renderer where

import Protolude

import Data.Array (Array, listArray, assocs)
import Data.Function ((&), id)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import SpaceSloths.Assets (Assets(..))
import SpaceSloths.GameMap (Cell, GameMap(..))

findTile :: Assets -> Cell -> Element
findTile (Assets missing tiles) c = maybe missing id (Map.lookup c tiles)

renderTile :: UI.Canvas -> Assets -> (Int, Int) -> Cell -> UI ()
renderTile canvas assets (y,x) cell = UI.drawImage (findTile assets cell) (fromIntegral x * 32, fromIntegral y * 32) canvas

renderGameView :: UI.Canvas -> Assets -> GameMap -> UI ()
renderGameView canvas assets (GameMap w h arr) =
  mapM_ (uncurry (renderTile canvas assets)) $ assocs arr
