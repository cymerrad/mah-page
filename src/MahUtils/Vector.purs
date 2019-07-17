module MahUtils.Vector where

import Prelude

type Vector
  = { x :: Number
    , y :: Number
    }

addVec :: Vector -> Vector -> Vector
addVec v w = { x: v.x + w.x, y: v.y + w.y }

scaleVec :: Number -> Vector -> Vector
scaleVec a v = { x: a * v.x, y: a * v.y }

negVec :: Vector -> Vector
negVec v = { x: -v.x, y: -v.y }

 {-
newtype Vector2D
  = Vector2D
  { x :: Number
  , y :: Number
  }

-- I'm pretty sure that this is a type checker bug
-- https://github.com/purescript/purescript/issues/1957
-- this is similar and unfixed
vector2DtoRecord :: Vector2D -> { x :: Number, y :: Number }
vector2DtoRecord = unsafeCoerce

class VectorSpace v where
  translate :: v -> v -> v
  scale :: Number -> v -> v
  zero :: v
  one :: v

instance twoDimVector :: VectorSpace Vector2D where
  translate v w =
    let
      v_coerced = vector2DtoRecord v

      w_coerced = vector2DtoRecord w
    in
      Vector2D
        { x: v_coerced.x + w_coerced.x
        , y: v_coerced.y + w_coerced.y
        }
  scale a v =
    let
      v_coerced = vector2DtoRecord v
    in
      Vector2D
        { x: v_coerced.x * a
        , y: v_coerced.y * a
        }
  zero = Vector2D { x: 0.0, y: 0.0 }
  one = Vector2D { x: 1.0, y: 1.0 }
-}