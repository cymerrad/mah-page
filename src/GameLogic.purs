module GameLogic where

import Models
import Prelude

import Data.Int (toNumber)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Signal.DOM (CoordinatePair)
import Signal.Time (Time)


movementFactor :: Number
movementFactor = 0.5

fishingLineLength :: Number
fishingLineLength = 50.0

maxSpeed :: Number
maxSpeed = 4.0

waterLevel :: Model -> Number
waterLevel m = m.canvasSize.y

gravity :: Number
gravity = 0.15 -- px / frame^2

maxMoveSpeed :: Number
maxMoveSpeed = 2.5 -- px / frame

groundAccel :: Number
groundAccel = 0.06 -- px / frame^2

airAccel :: Number
airAccel = 0.04 -- px / frame^2

airFriction :: Number
airFriction = 0.02 -- px / frame^2

-- TODO rod is still equal to bait, in future bait will follow rod with a delay
-- instantaneous move
rodMoves :: Model -> Model
rodMoves m = updateObjects m moveToPointer _ROD_OBJ
  where
  moveToPointer obj =
    pure
      $ setObjPos obj m.pointerPos.x m.pointerPos.y

-- aplies momentum to rod
baitFollowsRod :: Model -> Model
baitFollowsRod m =
  let
    targetM = Map.lookup _ROD_OBJ m.env.objects
  in
    updateObjects m (thingFollows targetM) _BAIT_OBJ
  where
  thingFollows oM f = setAsTarget <$> oM <*> pure f

-- applies momentum to fish
fishFollowsBait :: Model -> Model
fishFollowsBait m =
  let
    targetM = Map.lookup _BAIT_OBJ m.env.objects
  in
    updateObjects m (thingFollows targetM) _FISH_OBJ
  where
  thingFollows oM f = setAsTarget <$> oM <*> pure f

setAsTarget :: GameObject -> GameObject -> GameObject
setAsTarget target source =
  setObjVel source (target.position.x - source.position.x) (target.position.y - source.position.y)

-- execute momentum of objects
objectsMove :: Model -> Model
objectsMove m = updateObjectsAll m momentum
  where
  momentum obj = pure $ setObjPos obj (obj.position.x + movementFactor * obj.velocity.x) (obj.position.y + movementFactor * obj.velocity.y)

baitHangs :: Model -> Model
baitHangs m =
  updateObjects m hangOnRod _BAIT_OBJ
  where
    hangOnRod bait = pure $
      setObjPos bait bait.position.x (bait.position.y + fishingLineLength)

-- TODO or else it catches its prey and stuff happens
fishStaysInWaterOrElse :: Model -> Model
fishStaysInWaterOrElse m = updateObjects m (\v -> pure $ setObjPos v v.position.x (waterLevel m)) "fish"

mouseMoved :: CoordinatePair -> Model -> Model
mouseMoved new_coord m =
  m
    { pointerPos =
      { x: toNumber new_coord.x - m.canvasPos.x
      , y: toNumber new_coord.y - m.canvasPos.y
      }
    }

timePassed :: Model -> Model
timePassed m =
  m
    { ticks = (m.ticks + 1)
    }

objectsInteract :: Model -> Model
objectsInteract =
  identity -- just to preserve indentation
    >>> rodMoves
    >>> baitFollowsRod
    >>> fishFollowsBait
    >>> objectsMove
    >>> baitHangs
    >>> fishStaysInWaterOrElse

-- Inputs needed to calculate model changes
type Input
  = Tuple Time CoordinatePair

gameLogic :: Input -> Model -> Model
gameLogic input model = stateChanged input model
  where
  stateChanged :: Input -> (Model -> Model)
  stateChanged (Tuple frame pPos) =
    identity -- state changes
      >>> timePassed
      >>> mouseMoved pPos
      >>> objectsInteract
