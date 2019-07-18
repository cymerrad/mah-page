module GameLogic where

import Prelude
import Data.Int (toNumber)
import Models
import Data.Tuple (Tuple(..))
import Signal.Time (Time)
import Data.Map as Map
import Signal.DOM (CoordinatePair)

waterLevel :: Model -> Number
waterLevel m = m.canvasSize.y

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
