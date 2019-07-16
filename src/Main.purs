module Main where

import Prelude
import Color (hsl)
import Data.Foldable (foldMap)
import Data.Int (floor, toNumber)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Flare (UI, intSlider_, lift, runFlareWith)
import Flare.Drawing (Drawing, circle, fillColor, filled)
import Graphics.Canvas (Context2D, clearRect, getCanvasHeight, getCanvasWidth, getContext2D)
import Graphics.Drawing (render)
import MahUtils.HTML (getSaneCanvasElementById, getSaneElementbyId)
import Signal.DOM (CoordinatePair, animationFrame, mousePos)
import Signal.Time (Time)
import Web.HTML.HTMLElement (offsetLeft, offsetTop)

type Object
  = { id :: String
    , hueVal :: Number -- some sprite definition
    , x :: Number
    , y :: Number
    , vx :: Number
    , vy :: Number
    }

type ObjectMap
  = Map.Map String Object

type Model
  = { ctx :: Context2D
    , width :: Number
    , height :: Number
    , baseX :: Number
    , baseY :: Number
    , pX :: Number
    , pY :: Number
    , objects :: Map.Map String Object
    , time :: Time
    , dTime :: Number
    }

movementFactor :: Number
movementFactor = 0.1

waterLevel :: Model -> Number
waterLevel m = m.height

startObjects :: Model -> Effect ObjectMap
startObjects m =
  pure
    $ Map.fromFoldable
        [ Tuple "bait" { id: "bait", hueVal: 0.0, x: 0.0, y: 0.0, vx: 0.0, vy: 0.0 }
        , Tuple "fish" { id: "fish", hueVal: 200.0, x: (m.width / 2.0), y: (waterLevel m), vx: 0.0, vy: 0.0 }
        ]

moveObj :: Object -> Time -> Object
moveObj o dt = o -- TODO spreading only some properties of the object?

moveObjTo :: Object -> Number -> Number -> Object
moveObjTo o n_x n_y = o { x = n_x, y = n_y }

drawObjects :: ObjectMap -> Time -> Drawing
drawObjects objects time = foldMap makeCircle objects
  where
  makeCircle o = filledCircle o.hueVal o.x o.y 23.0

filledCircle :: Number -> Number -> Number -> Number -> Drawing
filledCircle hue_val pos_x pos_y radius = filled (fillColor col) (circle pos_x pos_y radius)
  where
  col = hsl hue_val 0.8 0.4 -- hue sth sth

-- TODO rod is still equal to bait, in future bait will follow rod
-- instantaneous move
rodMoves :: Model -> Model
rodMoves m =
  m
    { objects = Map.update moveToPointer "bait" m.objects
    }
  where
  moveToPointer obj =
    pure
      $ obj
          { x = m.pX
          , y = m.pY
          }

-- aplies momentum to rod
baitFollowsRod :: Model -> Model
baitFollowsRod = identity

-- applies momentum to fish
fishFollowsBait :: Model -> Model
fishFollowsBait m =
  let
    baitM = Map.lookup "bait" m.objects
  in
    m
      { objects = Map.update (fishFollow baitM) "fish" m.objects
      }
  where
  fishFollow bM f = fishFollowSimple <$> bM <*> pure f
    where
    fishFollowSimple :: Object -> Object -> Object
    fishFollowSimple bait fish =
      fish
        { vx = bait.x - fish.x
        , vy = bait.y - fish.y
        }

-- execute momentum of objects
objectsMove :: Model -> Model
objectsMove m =
  m
    { objects = Map.mapMaybe momentum m.objects
    }
  where
  momentum obj =
    pure
      $ obj
          { x = obj.x + movementFactor * obj.vx
          , y = obj.y + movementFactor * obj.vy
          }

-- TODO or else it catches its pray and stuff happens
fishStaysInWaterOrElse :: Model -> Model
fishStaysInWaterOrElse m =
  m
    { objects = Map.update (\v -> pure $ moveObjTo v v.x (waterLevel m)) "fish" m.objects
    }

-- controller for controlling?
controller :: Model -> Effect Unit
controller model = do
  clearRect model.ctx { x: 0.0, y: 0.0, width: model.width, height: model.height }
  render model.ctx $ drawObjects model.objects model.time

-- view updates
mouseMoved :: CoordinatePair -> Model -> Model
mouseMoved new_coord m =
  m
    { pX = toNumber new_coord.x - m.baseX
    , pY = toNumber new_coord.y - m.baseY
    }

-- sliderMoved :: Int -> Model -> Model
-- sliderMoved new_pos m =
--   m
--     { objects = Map.update (\v -> pure $ moveObjTo v (toNumber new_pos) v.y) "fish" m.objects
--     }
timePassed :: Number -> Model -> Model
timePassed new_time m =
  m
    { time = new_time
    , dTime = new_time - m.time
    }

objectsInteract :: Model -> Model
objectsInteract =
  identity -- just to preserve indentation
    >>> rodMoves
    >>> baitFollowsRod
    >>> fishFollowsBait
    >>> objectsMove
    >>> fishStaysInWaterOrElse

view :: Model -> UI Model
view model =
  -- gather data each frame
  state
    <$> lift animationFrame
    <*> lift mousePos
    <*> intSlider_ 0 (floor model.width) (floor model.width)
    -- apply to current model

    <*> pure model
  where
  state time coord n =
    identity -- state changes
      >>> timePassed time
      >>> mouseMoved coord
      >>> objectsInteract

main :: Effect Unit
main = do
  pcanvas <- getSaneCanvasElementById "output"
  ctx <- getContext2D pcanvas
  width <- getCanvasWidth pcanvas
  height <- getCanvasHeight pcanvas
  canvasEl <- getSaneElementbyId "output" -- this is some other canvas abstraction
  baseX <- offsetLeft canvasEl -- only this allows us to read css computed values
  baseY <- offsetTop canvasEl -- yeah
  let
    model =
      { ctx: ctx
      , width: width
      , height: height
      , baseX: baseX
      , baseY: baseY
      , pX: baseX
      , pY: baseY
      , objects: Map.empty
      , time: 0.0
      , dTime: 0.0
      }
  objects <- startObjects model
  runFlareWith "controls" controller
    $ view (model { objects = objects })
