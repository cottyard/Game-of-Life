import Game
import Graphics.Element as GElem exposing (Element)
import Graphics.Collage exposing (collage, Form, move)
import Mouse
import Signal exposing ((<~))
import Timer

(canvas_width, canvas_height) = (1600, 800)
(mouse_calib_x, mouse_calib_y) = (50, 765)

main : Signal Element
main = layoutMany_live [ (GElem.show <~ Timer.rawTimer)
                       , mouseInfo_live
                       , (game_live |> draw_live |> scene_live)
                       ]

-- signals

incomingMouseClick : Signal (Int, Int)
incomingMouseClick =
  Signal.sampleOn Mouse.clicks Mouse.position

incomingMouseClick_calibed : Signal (Int, Int)
incomingMouseClick_calibed =
  (\(x, y) -> (x - mouse_calib_x, mouse_calib_y - y)) <~ incomingMouseClick


cellClick_live : Signal Game.CellCoord
cellClick_live =
  Signal.filterMap Game.pixelToCellCoord (Game.CellCoord (0, 0)) incomingMouseClick_calibed

game_live : Signal Game.Model
game_live =
  Signal.foldp Game.update Game.init incomingUpdate

mouseInfo_live : Signal Element
mouseInfo_live =
  GElem.show <~ Mouse.position

incomingUpdate : Signal Game.Update
incomingUpdate =
  Signal.merge (Game.CellClick <~ cellClick_live) ((\ _ -> Game.Clock) <~ Timer.timeUp)

scene_live : Signal (List Form) -> Signal Element
scene_live =
  Signal.map scene

draw_live : Signal (Game.Model) -> Signal (List Form)
draw_live =
  Signal.map Game.draw

layout_live : Signal Element -> Signal Element -> Signal Element
layout_live =
  Signal.map2 layout

layoutMany_live : List (Signal Element) -> Signal Element
layoutMany_live (head::tail) =
  List.foldl layout_live head tail

--

layout : Element -> Element -> Element
layout e1 e2 =
  GElem.flow GElem.right [e1, e2]

scene : List Form -> Element
scene forms =
  collage canvas_width canvas_height (
    calibrate (-canvas_width / 2 + 50, -canvas_height / 2 + 50) forms)

calibrate : (Float, Float) -> List Form -> List Form
calibrate offset forms =
  List.map (move offset) forms
