module Main where

import Prelude

import Data.Array (take)
import Data.Foldable (foldMap)
import Data.Unfoldable (replicateA)
import Partial.Unsafe (unsafePartial)
import Data.Maybe (Maybe(..), fromJust)
import Math (Radians, log, cos, sin, sqrt, pi)
import Effect (Effect)
import Effect.Random (random, randomRange)
import Color (hsl)
import Flare (UI, numberSlider, runFlareWith, buttons, foldp, intSlider_, lift)
import Flare.Drawing (runFlareDrawing, Drawing, filled, fillColor, circle)
import Color (white)
import Graphics.Canvas (Context2D,
                        getCanvasElementById, getContext2D,
                        getCanvasWidth, getCanvasHeight, clearRect)
import Graphics.Drawing (Point, render)

import Signal.Time (Time)
import Signal.DOM (animationFrame)

type Model =  { ctx :: Context2D
              , width :: Number
              , height :: Number
              , flies :: Array Fly
              , time :: Time
              , start :: Boolean
              , reset :: Boolean
}

type Velocity = Point

scale :: Number -> Point -> Point
scale s {x: x, y: y} = {x: s*x, y: s*y}
infixr 5 scale as ⋆

rotate :: Point -> Radians -> Point
rotate {x: x, y: y} r = {x: cos r * x - sin r * y, y: sin r * x + cos r * y}
infixr 6 rotate as ∠

unit :: Point -> Point
unit p = (1.0/(sqrt $ p.x*p.x + p.y*p.y)) ⋆ p

type Fly = {p :: Point, v :: Velocity}

fly :: Fly -> Time -> Point
fly {p: p, v: v} t = g t ⋆ p ∠  f t
  where f t = a * log (g t)
        g t = 1.0 + b * t
        a   = (p.x * v.y - p.y * v.x) / (p.x * v.x + p.y * v.y)
        b   = (p.x * v.x + p.y * v.y) / (p.x * p.x + p.y * p.y)

randomFly :: Number -> Number -> Effect Fly
randomFly width height = do
  x <- randomRange (-width/2.0) (width/2.0)
  y <- randomRange (-height/2.0) (height/2.0)
  r <- randomRange (pi/2.0 + 0.05) (pi/2.0 + 0.1)
  c <- random
  let p = {x: x, y: y}
  let v = 0.5 ⋆ unit if c < 0.5 then p ∠  r
                                else p ∠ -r
  pure {p: p, v: v}

drawFlies :: Point -> Array Fly -> Time -> Drawing
drawFlies origin flies time = foldMap makeCircle flies
  where makeCircle f = filled (fillColor white) $ circle (p.x + origin.x) (p.y + origin.y) 2.0
          where p = fly f time

controller :: Model -> Effect Unit
controller model = do
  clearRect model.ctx {x: 0.0, y: 0.0, width: model.width, height: model.height}
  render model.ctx $ filled (fillColor white) $
                         circle (model.width/2.0) (model.height/2.0) 10.0
  render model.ctx $ drawFlies {x: model.width/2.0, y: model.height/2.0}
                                   model.flies
                                   model.time
data Action = Start | Reset

label :: Action -> String
label Start = "start"
label Reset = "reset"

isAction :: Maybe Action -> Action -> Boolean
isAction (Just a) b = label a == label b
isAction Nothing  _ = false

view :: Model -> UI Model
view model = state <$> buttons [Start, Reset] label
                   <*> intSlider_ 0 200 20
                   <*> lift animationFrame
  where state action n time = model { flies = take n model.flies
                                    , time  = time
                                    , start = action `isAction` Start
                                    , reset = action `isAction` Reset }

resetView :: Model -> UI Model
resetView model = foldp acc model $ view model
  where acc ma mb = if not mb.start || ma.reset
                       then ma{start = next, time = 0.0}
                       else ma{start = next, time = ma.time - mb.time}
                    where next = (ma.start || mb.start) && not ma.reset


main :: Effect Unit
-- main = runFlareDrawing "controls" "output" ui
main = do
  mcanvas     <- getCanvasElementById "output"
  let pcanvas =  unsafePartial $ fromJust mcanvas
  ctx         <- getContext2D pcanvas
  width       <- getCanvasWidth pcanvas
  height      <- getCanvasHeight pcanvas
  flies       <- replicateA 200 $ randomFly width height
  runFlareWith "controls" controller $ resetView { ctx: ctx
                                                 , width  : width
                                                 , height : height
                                                 , flies  : flies
                                                 , time   : 0.0
                                                 , start  : false
                                                 , reset  : false }