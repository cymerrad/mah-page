module Main where

import GameLogic
import Models
import Prelude

import Color (hsl)
import Data.Foldable (foldMap)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Console (log)
import MahUtils.Vector (Vector)
import Signal (Signal, foldp, runSignal, sampleOn, (~>))
import Signal.DOM (animationFrame, mousePos)
import Signal.Time (Time, every)

frameRateConst :: Number
frameRateConst = 33.0

frameRate :: Signal Number
frameRate = every frameRateConst

foreign import renderObject :: GameObject -> Effect Unit

foreign import width :: Number

foreign import height :: Number

drawObjects :: GameEnvironment -> Effect Unit
drawObjects env = foldMap renderObject env.objects

-- controller taking inputs on each frame and calculating new state with gameLogic
controller :: Signal Input -> Signal Model
controller input =
  foldp gameLogic (initialModel width height)
    (sampleOn mouseInp input)
  where
    mouseInp = map snd input -- perform calculations only when the pointer moves

-- view draws the current model state
view :: Model -> Effect Unit
view model = drawObjects model.env

main :: Effect Unit
main = do
  frames <- animationFrame
  pos <- mousePos
  log $ "width " <> (show width) <> " ;height " <> (show height)
  let
    inputs :: Signal Input
    inputs = Tuple <$> frames <*> pos
    scene = controller inputs
  runSignal $ scene ~> view
