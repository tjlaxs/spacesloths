module SpaceSloths.Assets where

import Protolude

import Control.Monad (sequence)
import qualified Data.Map as Map
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import SpaceSloths.GameMap (Cell)

data Assets
  = Assets
  { miss :: Element
  , tiles :: Map Cell Element
  }

loadTile :: Text -> UI Element
loadTile file = do
  imgUrl <- UI.loadFile "image/png" (toS file)
  UI.img & set UI.src imgUrl

loadAssets :: Text -> [(Cell, Text)] -> UI Assets
loadAssets miss xs = do
  missing <- loadTile miss
  tiles <- fmap Map.fromList . sequence . fmap sequence $ fmap (fmap loadTile) xs
  return $ Assets missing tiles
