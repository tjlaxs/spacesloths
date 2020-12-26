module SpaceSloths.Commands where

import Protolude

import qualified Text.ParserCombinators.ReadP as Parser

import SpaceSloths.Game (GameStatics(..))
import SpaceSloths.Renderer (clearGameView, renderGameView)

data Command
  = Render
  | Clear
  deriving (Show, Eq)

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
