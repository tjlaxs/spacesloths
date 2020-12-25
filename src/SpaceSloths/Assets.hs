module SpaceSloths.Assets where

import Protolude

import Control.Monad (sequence)
import qualified Data.Map as Map
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import SpaceSloths.GameMap (Cell)

loadTile :: Text -> UI Element
loadTile file = do
  imgUrl <- UI.loadFile "image/png" (toS file)
  UI.img & set UI.src imgUrl

loadAssets :: [(Cell, Text)] -> UI (Map Cell Element)
loadAssets xs = fmap Map.fromList . sequence . fmap sequence $ fmap (fmap loadTile) xs
