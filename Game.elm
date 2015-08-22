module Game (init, draw, update, pixelToCellCoord, 
  Model, Update (CellClick, Clock), CellCoord (CellCoord)) where

import Graphics.Collage exposing (Form, move)
import Array exposing (Array)
import Cell
import Matrix exposing (Matrix)
import Util

type alias Model = Matrix Cell.Model
type CellCoord = CellCoord (Int, Int)
type Update = CellClick CellCoord | Clock

cell_count_w = 100
cell_count_h = 50

-- model

init : Model
init =
  Matrix.create (cell_count_w, cell_count_h) Cell.Dead

revertCell : CellCoord -> Model -> Model
revertCell (CellCoord coord) model =
  let cellModel = Matrix.get model coord 
  in Matrix.set model coord (Cell.reverse cellModel)

aliveNeighbours : CellCoord -> Model -> Int
aliveNeighbours (CellCoord coord) model =
  let countAlive cellModel count =
    case cellModel of
      Cell.Alive -> count + 1
      Cell.Dead -> count
  in List.foldl countAlive 0 (Matrix.neighbours model coord)

evolveCell : CellCoord -> Model -> Cell.Model
evolveCell (CellCoord coord) model =
  let n = aliveNeighbours (CellCoord coord) model
  in case Matrix.get model coord of
    Cell.Alive -> if
      | n < 2 -> Cell.Dead
      | n > 3 -> Cell.Dead
      | otherwise -> Cell.Alive
    Cell.Dead -> if
      | n == 3 -> Cell.Alive
      | otherwise -> Cell.Dead

evolve : Model -> Model
evolve model =
  Matrix.indexedMap (\index _ -> evolveCell (CellCoord index) model) model

-- view

draw : Model -> List Form
draw model = 
  Util.zipWith_iter (\f c -> f c)
                    (List.concat arrangementFuncs)
                    (List.concat <| drawCells model)

drawCells : Model -> List (List Form)
drawCells model =
  Array.map (Array.toList << Array.map Cell.draw) (Matrix.toArray model) |> Array.toList

move_Int : (Int, Int) -> Form -> Form
move_Int (x, y) = 
  move (toFloat x, toFloat y)

arrangementFuncs : List (List (Form -> Form))
arrangementFuncs =
  let getColumnFuncs xPos =
        List.map (move_Int << (,) xPos) (Util.rangeIntList 0 Cell.size cell_count_h)
  in List.map getColumnFuncs (Util.rangeIntList 0 Cell.size cell_count_w)

pixelToCellCoord : (Int, Int) -> Maybe CellCoord
pixelToCellCoord (x, y) =
  let x_coor = (x + Cell.size) // Cell.size - 1
      y_coor = (y + Cell.size) // Cell.size - 1
  in if | y_coor >= cell_count_h || y_coor < 0 ||
          x_coor >= cell_count_w || x_coor < 0 -> Nothing
        | otherwise -> Just (CellCoord (x_coor, y_coor))

-- update

update : Update -> Model -> Model
update update model =
  case update of
    CellClick (CellCoord (x, y)) -> revertCell (CellCoord (x, y)) model
    Clock -> evolve model
