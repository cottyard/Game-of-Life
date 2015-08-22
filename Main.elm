import Game
import Graphics.Element as GElem exposing (Element)
import Graphics.Collage exposing (collage, Form, move)
import Signal exposing ((<~))
import Timer
import Input
import Text

(canvas_width, canvas_height) = (1600, 800)

main : Signal Element
main = layoutMany_live [ (GElem.show <~ Timer.indicator)
                       , Signal.constant <| GElem.leftAligned <| Text.fromString "Speed: "
                       , (Game.draw >> scene) <~ game_live
                       ]

-- signals

game_live : Signal Game.Model
game_live =
  Signal.foldp Game.update Game.init incomingUpdate

incomingUpdate : Signal Game.Update
incomingUpdate =
  Signal.merge (Game.CellClick <~ Input.cellClick_live) ((\ _ -> Game.Clock) <~ Timer.timeUp)

layoutMany_live : List (Signal Element) -> Signal Element
layoutMany_live (head::tail) =
  List.foldl (Signal.map2 layout) head tail

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
