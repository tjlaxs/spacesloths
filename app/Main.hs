module Main where

import Protolude

import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar, modifyTVar)
import Data.Array (Array, listArray, assocs)
import Data.Function ((&), id)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import SpaceSloths.Assets (loadAssets)
import SpaceSloths.GameMap (GameMap(..), Cell(..), charToCell)
import SpaceSloths.PathFinding
import SpaceSloths.Renderer (renderGameView)
import SpaceSloths.Sloth

treat = Graphics.UI.Threepenny.Core.on
tpGet = Graphics.UI.Threepenny.Core.get

gameMap = GameMap 10 10 (listArray ((1,1), (10,10)) lst)
  where
    lst = fmap charToCell (mconcat
      [ "##########"
      , "#........#"
      , "#...#....#"
      , "#...#....#"
      , "#...######"
      , "#........#"
      , "#...######"
      , "#...#....#"
      , "#........#"
      , "##########"
      ])

missFile = "static/assets/missing_32x32.png"
assetFiles =
  [ (Vacuum, "static/assets/tiles/spacesloths_0.png")
  , (Wall,   "static/assets/tiles/spacesloths_1.png")
  , (Air,    "static/assets/tiles/spacesloths_2.png")
  ]

canvasSize = 400

commandPrompt = UI.input & set (attr "placeholder") "Command"
gameView = do
  canvas <- UI.canvas
    & set UI.height canvasSize
    & set UI.width canvasSize
    & set style [("border", "solid black 1px"), ("background", "#eee")]
  return canvas

setup :: Window -> UI ()
setup rootWin = do
  return rootWin & set UI.title "Space Sloths"
  cp <- commandPrompt
  gv <- gameView
  getBody rootWin & set children [ cp, gv ]
  assets <- loadAssets missFile assetFiles
  treat UI.sendValue cp $ \input -> do
    case input of
      "render" -> renderGameView gv assets gameMap
      _ -> return ()

main :: IO ()
main = do
  startGUI settings setup

  where
    settings = defaultConfig
      { jsPort = Just 1983
      , jsStatic = Just "./static"
      }
