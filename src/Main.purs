module Main where

import GameLogic
import Models
import Prelude

import Data.Foldable (foldMap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Signal (Signal, foldp, runSignal, sampleOn, (~>), (<~), constant, merge, filter, unwrap)
import Signal.DOM (animationFrame, mousePos)
import Signal.Time (every)

frameRateConst :: Number
frameRateConst = 100.0 -- 10fps

frameRate :: Signal Number
frameRate = every frameRateConst

timeout :: Number
timeout = 1000.0

foreign import renderObject :: GameObject -> Effect Unit

foreign import isHidden :: Effect Boolean

foreign import width :: Effect Number

foreign import height :: Effect Number

drawObjects :: GameEnvironment -> Effect Unit
drawObjects env = foldMap renderObject env.objects

-- controller taking inputs on each frame and calculating new state with gameLogic
-- performs calculations only when the tickOn ticks
controller :: Signal Input -> Signal _ -> Model -> Signal Model
controller input tickOn initModel = foldp gameLogic initModel (sampleOn tickOn input)

-- view draws the current model state
view :: Model -> Effect Unit
view model = drawObjects model.env

main :: Effect Unit
main = do
  frames <- animationFrame
  pos <- mousePos
  w <- width
  h <- height

  signalIsHiddenAnimate <- unwrap (every frameRateConst ~> \_ -> isHidden)
  signalIsHiddenSlow <- unwrap (every 2000.0 ~> \_ -> isHidden)
  let
    -- visibilityChange = dropRepeats signalIsHidden
    whenVisible = filter not true signalIsHiddenAnimate
    whenVisibleSlow = filter not true signalIsHiddenSlow

    inputs :: Signal Input
    inputs = Tuple <$> frames <*> pos

    -- ticks when mouse moves or every 'frameRate' ms but only when visible
    tickOn :: Signal _
    tickOn = merge ((const true) <~ pos) whenVisible

    scene :: Signal Model
    scene = controller inputs tickOn $ initialModel w h

  runSignal $ scene ~> view
  runSignal
    $ ( sampleOn (whenVisibleSlow) $ (constant 1)
          ~> ( \_ -> do
                w_ <- width
                h_ <- height
                log $ "width " <> (show w_) <> " ;height " <> (show h_)
                hidden <- isHidden
                log $ "isHidden " <> (show hidden)
            )
      )
