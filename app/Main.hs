module Main where

import Protolude

import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar, modifyTVar)
import Data.Array (Array, listArray, assocs)
import Data.Function (id)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import qualified Text.ParserCombinators.ReadP as Parser

import SpaceSloths.Assets (Assets(..), loadAssets)
import SpaceSloths.GameMap (GameMap(..), Cell(..), charToCell)
import SpaceSloths.PathFinding
import SpaceSloths.Renderer (clearGameView, renderGameView)
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

commandPrompt = UI.input # set (attr "placeholder") "Command"
gameView = do
  canvas <- UI.canvas
    # set UI.height canvasSize
    # set UI.width canvasSize
    # set style [("border", "solid black 1px"), ("background", "#eee")]
  return canvas

data Command
  = Render
  | Clear
  deriving (Show, Eq)

data GameStatics
  = GameStatics
  { _gameView :: Element
  , _gameAssets :: Assets
  }

evalCommand (GameStatics v a) m cmd =
  case cmd of
    Render -> renderGameView v a m
    Clear -> clearGameView v

parseCommand input = do
  case mres of
    Just res -> Right res
    Nothing -> Left $ "Failed to parse: " <> input
  where
    mres = fst <$> head parse
    parse =
      Parser.readP_to_S commands input
    commands =
      Parser.choice
        [ render
        , clear
        ]
    clear = do
      Parser.string "Clear"
      return Clear
    render = do
      Parser.string "Render"
      return Render

setup :: Window -> UI ()
setup rootWin = do
  return rootWin # set UI.title "Space Sloths"
  cp <- commandPrompt
  cpText <- UI.div #. "command-prompt-ok" # set text ""
  gv <- gameView
  getBody rootWin # set children [ cp, cpText, gv ]
  assets <- loadAssets missFile assetFiles
  let statics = GameStatics gv assets
  treat UI.sendValue cp $ \input -> do
    let cmd = parseCommand input
    case cmd of
      Right c -> do
        evalCommand statics gameMap c
        element cp # set UI.value ""
        element cpText # set text ("Evaluated " <> show c)
           # set UI.style [("border", "2px solid black")]
      Left e ->
        element cpText
          # set UI.style [("border", "2px solid red")]
          # set text e

main :: IO ()
main = do
  startGUI settings setup

  where
    settings = defaultConfig
      { jsPort = Just 1983
      , jsStatic = Just "./static"
      }
