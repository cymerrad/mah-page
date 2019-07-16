module Main where

import Prelude
import Color (white, hsl)
import Data.Array (take)
import Data.Foldable (foldMap)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromJust)
import Data.Unfoldable (replicateA)
import Effect (Effect)
import Effect.Random (random, randomRange)
import Flare (UI, buttons, foldp, intSlider_, lift, runFlareWith)
import Flare.Drawing (Drawing, circle, fillColor, filled)
import Graphics.Canvas (Context2D, getCanvasElementById, getContext2D, getCanvasWidth, getCanvasHeight, clearRect)
import Graphics.Drawing (Point, render)
import Math (Radians, log, cos, sin, sqrt, pi)
import Partial.Unsafe (unsafePartial)
import Signal.DOM (animationFrame)
import Signal.Time (Time)

type Object
  = { id :: String
    , css :: String -- sprite definition
    , x :: Number
    , y :: Number
    , vx :: Number
    , vy :: Number
    }

type Model
  = { ctx :: Context2D
    , width :: Number
    , height :: Number
    , objects :: Array Object
    , time :: Time
    }

startObj :: Effect Object
startObj = pure { id: "object", css: "", x: 0.0, y: 0.0, vx: 0.0, vy: 0.0 }

move :: Object -> Time -> Object
move o dt = o -- TODO spreading only some properties of the object?

drawObjects :: Array Object -> Time -> Drawing
drawObjects objects time = foldMap makeCircle objects
  where
  makeCircle o = filledCircle o.x o.y 23.0

filledCircle :: Number -> Number -> Number -> Drawing
filledCircle pos_x pos_y radius = filled (fillColor col) (circle pos_x pos_y radius)
  where
  col = hsl 0.0 0.8 0.4 -- hue sth sth

controller :: Model -> Effect Unit
controller model = do
  clearRect model.ctx { x: 0.0, y: 0.0, width: model.width, height: model.height }
  render model.ctx $ drawObjects model.objects model.time

-- data Action
--   = Start
--   | Reset
-- label :: Action -> String
-- label Start = "start"
-- label Reset = "reset"
-- isAction :: Maybe Action -> Action -> Boolean
-- isAction (Just a) b = label a == label b
-- isAction Nothing _ = false
view :: Model -> UI Model
view model =
  -- mouse events here
  state <$> intSlider_ 0 (floor model.width) (floor model.width)
    <*> lift animationFrame
  where
  state n time =
    model
      { objects = map (\o -> o { x = (toNumber n) }) model.objects
      , time = time
      }

main :: Effect Unit
-- main = runFlareDrawing "controls" "output" ui
main = do
  mcanvas <- getCanvasElementById "output"
  let
    pcanvas = unsafePartial $ fromJust mcanvas
  ctx <- getContext2D pcanvas
  width <- getCanvasWidth pcanvas
  height <- getCanvasHeight pcanvas
  objects <- replicateA 1 $ startObj
  runFlareWith "controls" controller
    $ view
        { ctx: ctx
        , width: width
        , height: height
        , objects: objects
        , time: 0.0
        }
