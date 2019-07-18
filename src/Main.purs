module Main where

import Prelude

import Color (hsl)
import Data.Foldable (foldMap)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Flare.Drawing (Drawing, circle, fillColor, filled)
import MahUtils.Vector (Vector)
import Signal (Signal, foldp, runSignal, sampleOn, (~>))
import Signal.DOM (CoordinatePair, mousePos)
import Signal.Time (Time, every)

type GameObject
  = { id :: String
    , css :: String -- some sprite definition
    , position :: Vector
    , velocity :: Vector
    }

foreign import renderObject :: GameObject -> Effect Unit

foreign import width :: Number

foreign import height :: Number

type ObjectMap
  = Map.Map String GameObject

type GameEnvironment
  = { objects :: ObjectMap }

type Model
  = { canvasSize :: Vector
    , canvasPos :: Vector
    , pointerPos :: Vector
    , env :: GameEnvironment
    , time :: Time
    , ticks :: Number
    }

updateObjects :: Model -> (GameObject -> Maybe GameObject) -> String -> Model
updateObjects model updateFunc key =
  model
    { env
      { objects = Map.update updateFunc key model.env.objects
      }
    }

updateObjectsAll :: Model -> (GameObject -> Maybe GameObject) -> Model
updateObjectsAll model updateFunc =
  model
    { env
      { objects = Map.mapMaybe updateFunc model.env.objects
      }
    }

frameRateConst :: Number
frameRateConst = 33.0

-- frameRateConst = 1000.0
frameRate :: Signal Number
frameRate = every frameRateConst

movementFactor :: Number
movementFactor = 0.5

waterLevel :: Number
waterLevel = height

initialGameEnv :: GameEnvironment
initialGameEnv =
  { objects:
    Map.fromFoldable
      [ Tuple "bait" { id: "bait", css: "", position: { x: 0.0, y: 0.0 }, velocity: { x: 0.0, y: 0.0 } }
      , Tuple "rod" { id: "rod", css: "", position: { x: 0.0, y: 0.0 }, velocity: { x: 0.0, y: 0.0 } }
      , Tuple "fish" { id: "fish", css: "", position: { x: (width / 2.0), y: (waterLevel) }, velocity: { x: 0.0, y: 0.0 } }
      ]
  }

initialModel :: Model
initialModel =
  { canvasSize: { x: width, y: height }
  , canvasPos: { x: 0.0, y: 0.0 }
  , pointerPos: { x: 0.0, y: 0.0 }
  , env: initialGameEnv
  , time: 0.0
  , ticks: 0.0
  }

moveObj :: GameObject -> Time -> GameObject
moveObj o dt = o -- TODO spreading only some properties of the object?

setObjPos :: GameObject -> Number -> Number -> GameObject
setObjPos o n_x n_y = o { position = { x: n_x, y: n_y } }

setObjVel :: GameObject -> Number -> Number -> GameObject
setObjVel o n_x n_y = o { velocity = { x: n_x, y: n_y } }

drawObjects :: GameEnvironment -> Effect Unit
drawObjects env = foldMap renderObject env.objects

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
    fishFollowSimple :: GameObject -> GameObject -> GameObject
    fishFollowSimple bait fish = setObjVel fish (bait.position.x - fish.position.x) (bait.position.y - fish.position.y)

-- execute momentum of objects
objectsMove :: Model -> Model
objectsMove m = updateObjectsAll m momentum
  where
  momentum obj = pure $ setObjPos obj (obj.position.x + movementFactor * obj.velocity.x) (obj.position.y + movementFactor * obj.velocity.y)

-- TODO or else it catches its pray and stuff happens
fishStaysInWaterOrElse :: Model -> Model
fishStaysInWaterOrElse m = updateObjects m (\v -> pure $ setObjPos v v.position.x (waterLevel)) "fish"

-- Inputs needed to calculate model changes
type Input
  -- = Tuple Time CoordinatePair
  = CoordinatePair

mouseMoved :: CoordinatePair -> Model -> Model
mouseMoved new_coord m =
  m
    { pointerPos =
      { x: toNumber new_coord.x - m.canvasPos.x
      , y: toNumber new_coord.y - m.canvasPos.y
      }
    }

timePassed :: Number -> Number -> Model -> Model
timePassed new_time new_ticks m =
  m
    { time = new_time
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

gameLogic :: Input -> Model -> Model
gameLogic input model = stateChanged input model
  where
  stateChanged :: Input -> (Model -> Model)
  stateChanged pPos =
    identity -- state changes
      -- >>> timePassed time (time / frameRateConst)

      >>> mouseMoved pPos
      >>> objectsInteract

-- controller taking inputs each frame
controller :: Signal Input -> Signal Model
controller input =
  foldp gameLogic initialModel
    (sampleOn frameRate input)

view :: Model -> Effect Unit
view model = drawObjects model.env

main :: Effect Unit
main = do
  pPos <- mousePos
  -- _ < runSignal frameRate
  log $ "width " <> (show width) <> " ;height " <> (show height)
  let
    scene = controller $ pPos
  runSignal $ scene ~> view
