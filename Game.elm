module Game (init, draw, update, pixelToCellCoord, 
  Model, Update (CellClick, Clock), CellCoord (CellCoord)) where

import Graphics.Collage exposing (Form, move)
import Array exposing (Array)
import Cell
import Trampoline exposing (Trampoline, trampoline)
import Matrix exposing (Matrix)

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
  zipWith_iter (\f c -> f c)
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
        List.map (move_Int << (,) xPos) (rangeIntList 0 Cell.size cell_count_h)
  in List.map getColumnFuncs (rangeIntList 0 Cell.size cell_count_w)

rangeIntList : Int -> Int -> Int -> List Int
rangeIntList begin step size =
  List.map ((*) step >> (+) begin) [0..size - 1]

-- Elm 0.15 doesn't support Tail Call Elimination; abandoning this function
zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith f l1 l2 =
  case l1 of
    []      -> []
    (x::xs) -> let (y::ys) = l2 in
      f x y :: zipWith f xs ys

zipWith_iter : (a -> b -> c) -> List a -> List b -> List c
zipWith_iter f l1 l2 = trampoline <| zipWith_iter' f l1 l2 []

zipWith_iter' : (a -> b -> c) -> List a -> List b -> List c -> Trampoline (List c)
zipWith_iter' f l1 l2 acc =
  case l1 of
    []      -> Trampoline.Done acc
    (x::xs) -> let (y::ys) = l2 in
      Trampoline.Continue (\() -> zipWith_iter' f xs ys (f x y::acc))

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
