import Mouse
import Signal exposing ((<~))
import Graphics.Element exposing (Element, show, flow, right)
import Time

import Update exposing (cellClicked, gameStateUpdated, timeUp)
import Model exposing (initial_game_state)
import View exposing (scene)

main : Signal Element
main = 
  Signal.map4 (\e1 e2 e3 e4 -> flow right [e1, e2, e3, e4])
              mainAreaUpdated
              mousePosUpdated
              mainAreaClicked
              showNeighbourCount

mainAreaUpdated : Signal Element
mainAreaUpdated =
  scene <~ gameStateUpdated

mousePosUpdated : Signal Element
mousePosUpdated =
  show <~ Mouse.position

mainAreaClicked : Signal Element
mainAreaClicked = 
  show <~ cellClicked

timeUpReceived : Signal Element
timeUpReceived =
  show <~ (Time.inSeconds <~ timeUp)  

showNeighbourCount : Signal Element
showNeighbourCount =
  show <~ (Signal.map2 Model.getNeighbourCount cellClicked gameStateUpdated)