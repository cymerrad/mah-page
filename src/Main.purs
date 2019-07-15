module Main where

import Prelude ((<$>), (<*>), Unit)

import Effect (Effect)
import Color (hsl)
import Flare (UI, numberSlider)
import Flare.Drawing (runFlareDrawing, Drawing, filled, fillColor, circle)

filledCircle :: Number -> Number -> Drawing
filledCircle hue radius =
  filled (fillColor col) (circle pos pos radius)
    where col = hsl hue 0.8 0.4
          pos = 50.0

ui :: UI Drawing
ui = filledCircle <$> numberSlider "Hue"    0.0 360.0 1.0  0.0
                  <*> numberSlider "Radius" 2.0  45.0 0.1 25.0

main :: Effect Unit
main = runFlareDrawing "controls" "output" ui