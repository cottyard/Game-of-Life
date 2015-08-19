import Game
import Graphics.Element exposing (Element)
import Graphics.Collage exposing (collage, Form, move)

(canvas_width, canvas_height) = (800, 800)

main : Element
main = collage canvas_width canvas_height (calibrate (0, -canvas_height / 3) Game.draw)

calibrate : (Float, Float) -> List Form -> List Form
calibrate offset forms =
  List.map (move offset) forms