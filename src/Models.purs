module Models where

import Data.Tuple (Tuple(..))
import Data.Map as Map
import Data.Maybe (Maybe)
import MahUtils.Vector (Vector)

type ObjectMap
  = Map.Map String GameObject

type GameObject
  = { id :: String
    , css :: String -- some sprite definition
    , position :: Vector
    , velocity :: Vector
    }

type GameEnvironment
  = { objects :: ObjectMap }

_BAIT_OBJ :: String
_BAIT_OBJ = "bait"
_ROD_OBJ :: String
_ROD_OBJ = "rod"
_FISH_OBJ :: String
_FISH_OBJ = "fish"

initialGameEnv :: GameEnvironment
initialGameEnv =
  { objects:
    Map.fromFoldable
      [ Tuple _BAIT_OBJ { id: _BAIT_OBJ, css: "", position: { x: 0.0, y: 0.0 }, velocity: { x: 0.0, y: 0.0 } }
      , Tuple _ROD_OBJ { id: _ROD_OBJ, css: "", position: { x: 0.0, y: 0.0 }, velocity: { x: 0.0, y: 0.0 } }
      , Tuple _FISH_OBJ { id: _FISH_OBJ, css: "", position: { x: 0.0, y: 0.0 }, velocity: { x: 0.0, y: 0.0 } }
      ]
  }

type Model
  = { canvasSize :: Vector
    , canvasPos :: Vector
    , pointerPos :: Vector
    , env :: GameEnvironment
    , ticks :: Int
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

initialModel :: Number -> Number -> Model
initialModel w h =
  { canvasSize: { x: w, y: h }
  , canvasPos: { x: 0.0, y: 0.0 }
  , pointerPos: { x: 0.0, y: 0.0 }
  , env: initialGameEnv
  , ticks: 0
  }

setObjPos :: GameObject -> Number -> Number -> GameObject
setObjPos o n_x n_y = o { position = { x: n_x, y: n_y } }

setObjVel :: GameObject -> Number -> Number -> GameObject
setObjVel o n_x n_y = o { velocity = { x: n_x, y: n_y } }
