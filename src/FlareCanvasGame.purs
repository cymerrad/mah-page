module FlareCanvasGame where

import MahUtils.Vector
import Prelude
import Color (hsl)
import Data.Foldable (foldMap)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Flare (UI, lift, runFlareWith)
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
    , position :: Vector
    , velocity :: Vector
    }

type ObjectMap
  = Map.Map String Object

type GameEnvironment
  = { objects :: ObjectMap }

type Model
  = { ctx :: Context2D
    , canvasSize :: Vector
    , canvasPos :: Vector
    , pointerPos :: Vector
    , env :: GameEnvironment
    , time :: Time
    , dTime :: Number
    , ticks :: Int
    }

updateObjects :: Model -> (Object -> Maybe Object) -> String -> Model
updateObjects model updateFunc key =
  model
    { env
      { objects = Map.update updateFunc key model.env.objects
      }
    }

updateObjectsAll :: Model -> (Object -> Maybe Object) -> Model
updateObjectsAll model updateFunc =
  model
    { env
      { objects = Map.mapMaybe updateFunc model.env.objects
      }
    }

movementFactor :: Number
movementFactor = 0.5

waterLevel :: Model -> Number
waterLevel m = m.canvasSize.y

startObjects :: Model -> Effect ObjectMap
startObjects m =
  pure
    $ Map.fromFoldable
        [ Tuple "bait" { id: "bait", hueVal: 0.0, position: { x: 0.0, y: 0.0 }, velocity: { x: 0.0, y: 0.0 } }
        , Tuple "fish" { id: "fish", hueVal: 200.0, position: { x: (m.canvasSize.x / 2.0), y: (waterLevel m) }, velocity: { x: 0.0, y: 0.0 } }
        ]

moveObj :: Object -> Time -> Object
moveObj o dt = o -- TODO spreading only some properties of the object?

setObjPos :: Object -> Number -> Number -> Object
setObjPos o n_x n_y = o { position = { x: n_x, y: n_y } }

setObjVel :: Object -> Number -> Number -> Object
setObjVel o n_x n_y = o { velocity = { x: n_x, y: n_y } }

drawObjects :: GameEnvironment -> Time -> Drawing
drawObjects env time = foldMap makeCircle env.objects
  where
  makeCircle o = filledCircle o.hueVal o.position.x o.position.y 23.0

filledCircle :: Number -> Number -> Number -> Number -> Drawing
filledCircle hue_val pos_x pos_y radius = filled (fillColor col) (circle pos_x pos_y radius)
  where
  col = hsl hue_val 0.8 0.4 -- hue sth sth

-- TODO rod is still equal to bait, in future bait will follow rod
-- instantaneous move
rodMoves :: Model -> Model
rodMoves m = updateObjects m moveToPointer "bait"
  where
  moveToPointer obj =
    pure
      $ setObjPos obj m.pointerPos.x m.pointerPos.y

-- aplies momentum to rod
baitFollowsRod :: Model -> Model
baitFollowsRod = identity

-- applies momentum to fish
fishFollowsBait :: Model -> Model
fishFollowsBait m =
  let
    baitM = Map.lookup "bait" m.env.objects
  in
    updateObjects m (fishFollow baitM) "fish"
  where
  fishFollow bM f = fishFollowSimple <$> bM <*> pure f
    where
    fishFollowSimple :: Object -> Object -> Object
    fishFollowSimple bait fish = setObjVel fish (bait.position.x - fish.position.x) (bait.position.y - fish.position.y)

-- execute momentum of objects
objectsMove :: Model -> Model
objectsMove m = updateObjectsAll m momentum
  where
  momentum obj = pure $ setObjPos obj (obj.position.x + movementFactor * obj.velocity.x) (obj.position.y + movementFactor * obj.velocity.y)

-- TODO or else it catches its pray and stuff happens
fishStaysInWaterOrElse :: Model -> Model
fishStaysInWaterOrElse m = updateObjects m (\v -> pure $ setObjPos v v.position.x (waterLevel m)) "fish"

-- controller for controlling?
controller :: Model -> Effect Unit
controller model = do
  log $ "Tick " <> (show model.ticks) <> "; dTime " <> (show model.dTime)
  -- if (mod model.ticks 10 == 0) then log $ "dTime " <> (show model.dTime) else pure unit
  clearRect model.ctx { x: 0.0, y: 0.0, width: model.canvasSize.x, height: model.canvasSize.y }
  render model.ctx $ drawObjects model.env model.time

-- view updates
mouseMoved :: CoordinatePair -> Model -> Model
mouseMoved new_coord m =
  m
    { pointerPos =
      { x: toNumber new_coord.x - m.canvasPos.x
      , y: toNumber new_coord.y - m.canvasPos.y
      }
    }

timePassed :: Number -> Number -> Int -> Model -> Model
timePassed new_time old_time new_ticks m =
    m
      { time = new_time
      , dTime = (new_time - old_time)
      , ticks = new_ticks
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
    -- <*> intSlider_ 0 (floor model.width) (floor model.width)

    -- apply to current model

    <*> pure model
  where
  state time coord =
    identity -- state changes
      >>> timePassed time (model.time + 0.0) (model.ticks + 1)
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
      , canvasSize: { x: width, y: height }
      , canvasPos: { x: baseX, y: baseY }
      , pointerPos: { x: baseX, y: baseY }
      , env: { objects: Map.empty }
      , time: 0.0
      , dTime: 0.0
      , ticks: 0
      }
  objects <- startObjects model
  let modelPopulated = model { env { objects = objects } }
  runFlareWith "controls" controller (view modelPopulated)
