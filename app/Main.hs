module Main where

import Protolude

import Data.Array (Array, listArray)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import SpaceSloths.GameMap (GameMap(..), charToCell)
import SpaceSloths.PathFinding
import SpaceSloths.Sloth

treat = Graphics.UI.Threepenny.Core.on

gameMap = GameMap wth hgt (listArray (1, sz) lst)
  where
    wth = 10
    hgt = 10
    sz = wth * hgt
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

setup :: Window -> UI ()
setup win = do
  return win # set UI.title "Hello, world!"
  button <- UI.button # set UI.text "Click me!"
  getBody win #+ [element button]
  treat UI.click button $ const $ do
    element button # set UI.text "I have been clicked!"

main :: IO ()
main = do
  startGUI settings setup

  where
    settings = defaultConfig
      { jsPort = Just 1983
      , jsStatic = Just "./wwwroot"
      }
