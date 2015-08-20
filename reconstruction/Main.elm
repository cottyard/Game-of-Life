import Game
import Graphics.Element as GElem exposing (Element)
import Graphics.Collage exposing (collage, Form, move)
import Mouse
import Time exposing (Time)
import Signal exposing ((<~))

(canvas_width, canvas_height) = (1600, 800)

main : Signal Element
main = layout_live (game_live |> draw_live |> scene_live) 
                   info_live

-- signals

mouseClick_live : Signal (Int, Int)
mouseClick_live =
  Signal.sampleOn Mouse.clicks Mouse.position

clock : Signal Time
clock =
  Signal.constant Time.second
  -- Time.fpsWhen 1 timerStateChanged

cellClick_live : Signal Game.CellCoord
cellClick_live =
  Signal.filterMap Game.mousePosToCellCoord (Game.CellCoord (0, 0)) mouseClick_live

game_live : Signal Game.Model
game_live =
  Signal.foldp Game.update Game.init incomingUpdate

info_live : Signal Element
info_live =
  GElem.show <~ Mouse.position

incomingUpdate : Signal Game.Update
incomingUpdate =
  Signal.merge (Game.CellClick <~ cellClick_live) (Game.TimeUp <~ clock)

scene_live : Signal (List Form) -> Signal Element
scene_live =
  Signal.map scene

draw_live : Signal (Game.Model) -> Signal (List Form)
draw_live =
  Signal.map Game.draw

layout_live : Signal Element -> Signal Element -> Signal Element
layout_live =
  Signal.map2 layout

----

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