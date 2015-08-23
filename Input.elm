module Input (Update (MouseDown, MouseUp, Hover),
  Model, init, update, updateGame, updated) where

import Graphics.Element as GElem exposing (Element)
import Game
import Cell
import Mouse
import Signal exposing ((<~))
import Util
import Keyboard

(mouse_calib_x, mouse_calib_y) = (50, 765)

type Update = MouseDown Game.CellCoord
            | MouseUp Game.CellCoord
            | Hover Game.CellCoord
            | NextStep

type Model = Reviving
           | Killing
           | Touching

type alias MousePos = (Int, Int)

init : Model
init =
  Touching

update : Update -> Model -> Game.Model -> Model
update update model game =
  case update of
    MouseDown coord -> case model of
      Touching -> case Game.getCell game coord of
        Cell.Dead -> Reviving
        Cell.Alive -> Killing
      _ -> model
    MouseUp _ -> Touching
    Hover _ -> model
    NextStep -> model

updateGame : Update -> Model -> Game.Model -> Maybe Game.Update
updateGame update model game =
  case update of
    MouseDown coord ->
      Just <| Game.Metabolism (Game.CircleOfLife coord)
    MouseUp coord ->
      Nothing
    Hover coord -> case model of
      Reviving ->
        Just <| Game.Metabolism (Game.Mitosis coord)
      Killing ->
        Just <| Game.Metabolism (Game.Apoptosis coord)
      Touching ->
        Nothing
    NextStep -> Just Game.Evolve

updated : Signal Update
updated =
  Signal.mergeMany [ 
    MouseDown <~ pressing,
    MouseUp <~ releasing,
    Hover <~ hovering,
    (\() -> NextStep) <~ manualStep
  ]

manualStep : Signal ()
manualStep =
  Util.whenPressed (Keyboard.isDown 80)

mouseDown : Signal ()
mouseDown =
  Util.whenPressed Mouse.isDown

mouseUp : Signal ()
mouseUp =
  Util.whenReleased Mouse.isDown

sampleMousePos : Signal () -> Signal MousePos
sampleMousePos sig =
  Signal.sampleOn sig mousePosCalibed

filterIllegalMousePos : Signal MousePos -> Signal Game.CellCoord
filterIllegalMousePos =
  Signal.filterMap Game.pixelToCellCoord (Game.CellCoord (-1, -1))

mousePosCalibed : Signal MousePos
mousePosCalibed =
  (\(x, y) -> (x - mouse_calib_x, mouse_calib_y - y)) <~ Mouse.position

pressing : Signal Game.CellCoord
pressing =
  filterIllegalMousePos <| sampleMousePos mouseDown

releasing : Signal Game.CellCoord
releasing =
  filterIllegalMousePos <| sampleMousePos mouseUp

hovering : Signal Game.CellCoord
hovering =
  Signal.dropRepeats <| filterIllegalMousePos mousePosCalibed

