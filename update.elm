module Update where

import Mouse
import Keyboard exposing (space)
import Time exposing (Time, fpsWhen)
import Signal exposing ((<~))

import View exposing (mousePosToCellCoord)
import Model exposing (GameState, CellCoord)

type Update = CellClick CellCoord | TimeUp Time

updates : Signal Update
updates =
  Signal.merge
    (CellClick <~ cellClicked)
    (TimeUp <~ timeUp)

updateGameState : Update -> GameState -> GameState
updateGameState update game_state =
  case update of
    CellClick (Model.CellCoord (x, y)) -> Model.revertCell (Model.CellCoord (x, y)) game_state
    TimeUp _ -> game_state

cellClicked : Signal CellCoord
cellClicked = 
  Signal.filterMap mousePosToCellCoord (Model.CellCoord (0, 0)) (Signal.sampleOn Mouse.clicks Mouse.position)

timeUp : Signal Time
timeUp = fpsWhen 1 timerState

changeTimerState : Bool -> Bool -> Bool
changeTimerState space_pressed timer_state =
  if space_pressed then not timer_state else timer_state

timerState : Signal Bool
timerState =
  Signal.foldp changeTimerState False Keyboard.space