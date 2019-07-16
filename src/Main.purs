module Main where

import Prelude

import Color (white, hsl)
import Control.Monad.Error.Class (withResource)
import Data.Array (take)
import Data.Foldable (foldMap)
import Data.Int (floor, toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicateA)
import Effect (Effect)
import Effect.Random (random, randomRange)
import Flare (UI, buttons, foldp, intSlider_, lift, runFlareWith)
import Flare.Drawing (Drawing, circle, fillColor, filled)
import Graphics.Canvas (Context2D, clearRect, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, moveTo)
import Graphics.Drawing (Point, render)
import Math (Radians, log, cos, sin, sqrt, pi)
import Partial.Unsafe (unsafePartial)
import Signal.DOM (CoordinatePair, animationFrame, mousePos)
import Signal.Time (Time)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document (doctype)
import Web.DOM.Internal.Types (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (HTMLDocument, HTMLElement, window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.HTMLElement (offsetTop, offsetLeft)
import Web.HTML.Window (document)

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
    , baseX :: Number
    , baseY :: Number
    , objects :: Map.Map String Object
    , time :: Time
    }

startObjects :: Number -> Number -> Effect (Map.Map String Object)
startObjects width height =
  pure
    $ Map.fromFoldable
        [ Tuple "bait" { id: "bait", css: "", x: 0.0, y: 0.0, vx: 0.0, vy: 0.0 }
        , Tuple "character" { id: "character", css: "", x: (width / 2.0), y: height, vx: 0.0, vy: 0.0 }
        ]

moveObj :: Object -> Time -> Object
moveObj o dt = o -- TODO spreading only some properties of the object?

moveObjTo :: Object -> Number -> Number -> Object
moveObjTo o n_x n_y = o { x = n_x, y = n_y }

drawObjects :: Map.Map String Object -> Time -> Drawing
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

-- view updates
mouseMoved :: CoordinatePair -> Model -> Model
mouseMoved new_coord m =
  m
    { objects = Map.update (\v -> pure $ moveObjTo v (toNumber new_coord.x - m.baseX) (toNumber new_coord.y - m.baseY)) "bait" m.objects
    }

sliderMoved :: Int -> Model -> Model
sliderMoved new_pos m =
  m
    { objects = Map.update (\v -> pure $ moveObjTo v (toNumber new_pos) v.y) "character" m.objects
    }

timePassed :: Number -> Model -> Model
timePassed new_time m = m { time = new_time }

--   state m_coord n time =
--     model
--       { objects = map (\o -> o { x = (toNumber m_coord.x), y = (toNumber m_coord.y) }) model.objects
--       , time = time
--       }
view :: Model -> UI Model
view model =
  state <$> lift animationFrame
    <*> lift mousePos
    <*> intSlider_ 0 (floor model.width) (floor model.width)
    <*> pure model
  where
  state time coord n =
    timePassed time
      <<< mouseMoved coord
      <<< sliderMoved n

-- | purescript-web-html SUCKS ASS
toHTMLElement :: Element -> HTMLElement
toHTMLElement = unsafeCoerce

getHTMLElementbyId :: String -> Effect HTMLElement
getHTMLElementbyId id = do
  win <- window
  doc <- document win
  let docEl =  toNonElementParentNode doc
  wut <- getElementById id docEl
  let kurwa = unsafePartial $ fromJust wut
  pure $ toHTMLElement kurwa


main :: Effect Unit
-- main = runFlareDrawing "controls" "output" ui
main = do
  mcanvas <- getCanvasElementById "output"
  let
    pcanvas = unsafePartial $ fromJust mcanvas
  ctx <- getContext2D pcanvas
  width <- getCanvasWidth pcanvas
  height <- getCanvasHeight pcanvas
  canvasEl <- getHTMLElementbyId "output"
  baseX <- offsetLeft canvasEl
  baseY <- offsetTop canvasEl
  objects <- startObjects width height
  runFlareWith "controls" controller
    $ view
        { ctx: ctx
        , width: width
        , height: height
        , baseX: baseX
        , baseY: baseY
        , objects: objects
        , time: 0.0
        }
