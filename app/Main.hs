module Main where

import Protolude

import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar, modifyTVar)
import Data.Array (Array, listArray, assocs)
import Data.Map (Map)
import Data.Function ((&), id)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import SpaceSloths.Assets (loadAssets)
import SpaceSloths.GameMap (GameMap(..), Cell(..), charToCell)
import SpaceSloths.PathFinding
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

-- FIXME: pass default tile here so we can return something instead of undefined
findTile :: Map Cell Element -> Cell -> Element
findTile assets c = maybe undefined id (Map.lookup c assets)

renderTile :: UI.Canvas -> Map Cell Element -> (Int, Int) -> Cell -> UI ()
renderTile canvas assets (y,x) cell = UI.drawImage (findTile assets cell) (fromIntegral x * 32, fromIntegral y * 32) canvas

renderGameView :: UI.Canvas -> Map Cell Element -> GameMap -> UI ()
renderGameView canvas assets (GameMap w h arr) =
  mapM_ (uncurry (renderTile canvas assets)) $ assocs arr

setup :: Window -> UI ()
setup rootWin = do
  return rootWin & set UI.title "Space Sloths"
  cp <- commandPrompt
  gv <- gameView
  getBody rootWin & set children [ cp, gv ]

  assets <- loadAssets assetFiles

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
      , jsStatic = Just "./wwwroot"
      }
