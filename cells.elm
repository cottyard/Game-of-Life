import Mouse
import Signal exposing ((<~))
import Graphics.Element exposing (Element, show, flow, right)

import Update exposing (cellClicked)
import Model exposing (initial_game_state, revertCell)
import View exposing (scene)

main : Signal Element
main = 
  Signal.map3 (\e1 e2 e3 -> flow right [e1, e2, e3])
              (scene <~ Signal.foldp revertCell initial_game_state cellClicked)
              (show <~ Mouse.position)
              (show <~ cellClicked)
