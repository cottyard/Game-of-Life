module Update where

import Mouse
import Time exposing (Time, every, second)
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
timeUp = every second