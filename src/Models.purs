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

initialGameEnv :: GameEnvironment
initialGameEnv =
  { objects:
    Map.fromFoldable
      [ Tuple "bait" { id: "bait", css: "", position: { x: 0.0, y: 0.0 }, velocity: { x: 0.0, y: 0.0 } }
      , Tuple "rod" { id: "rod", css: "", position: { x: 0.0, y: 0.0 }, velocity: { x: 0.0, y: 0.0 } }
      , Tuple "fish" { id: "fish", css: "", position: { x: 0.0, y: 0.0 }, velocity: { x: 0.0, y: 0.0 } }
      ]
  }

movementFactor :: Number
movementFactor = 0.5

maxSpeed :: Number
maxSpeed = 4.0

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
