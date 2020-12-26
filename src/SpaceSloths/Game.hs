module SpaceSloths.Game where

import Protolude

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core (Element)

import SpaceSloths.Assets (Assets)

data GameStatics
  = GameStatics
  { _gameView :: Element
  , _gameAssets :: Assets
  }
