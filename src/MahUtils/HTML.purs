module MahUtils.HTML (getSaneElementbyId, getSaneCanvasElementById) where

import Prelude
import Data.Maybe (fromJust)
import Effect (Effect)
import Graphics.Canvas (CanvasElement, getCanvasElementById)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Internal.Types (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.HTMLElement (offsetTop, offsetLeft)
import Web.HTML.Window (document)

-- | purescript-web-html SUCKS ASS
toHTMLElement :: Element -> HTMLElement
toHTMLElement = unsafeCoerce

getSaneElementbyId :: String -> Effect HTMLElement
getSaneElementbyId id = do
  win <- window
  doc <- document win
  let
    docEl = toNonElementParentNode doc
  wut <- getElementById id docEl
  let
    kurwa = unsafePartial $ fromJust wut
  pure $ toHTMLElement kurwa

getSaneCanvasElementById :: String -> Effect CanvasElement
getSaneCanvasElementById id = do
  mcanvas <- getCanvasElementById "output"
  let
    pcanvas = unsafePartial $ fromJust mcanvas
  pure pcanvas
