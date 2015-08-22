module Input (cellClick_live) where

import Graphics.Element as GElem exposing (Element)
import Game
import Mouse
import Signal exposing ((<~))

(mouse_calib_x, mouse_calib_y) = (50, 765)

incomingMouseClick : Signal (Int, Int)
incomingMouseClick =
  Signal.sampleOn Mouse.clicks Mouse.position

incomingMouseClick_calibed : Signal (Int, Int)
incomingMouseClick_calibed =
  (\(x, y) -> (x - mouse_calib_x, mouse_calib_y - y)) <~ incomingMouseClick

cellClick_live : Signal Game.CellCoord
cellClick_live =
  Signal.filterMap Game.pixelToCellCoord (Game.CellCoord (0, 0)) incomingMouseClick_calibed

mouseInfo_live : Signal Element
mouseInfo_live =
  GElem.show <~ Mouse.position
