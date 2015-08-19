module Game (draw) where

import Graphics.Collage exposing (Form, move)
import Array exposing (Array)
import Cell
import Trampoline exposing (Trampoline, trampoline)
import Time exposing (Time)

type alias Model = Array (Array Cell.Model)
type CellCoord = CellCoord (Int, Int)
type Update = CellClick CellCoord | TimeUp Time


cell_count_w = 100
cell_count_h = 50

-- model

init : Model
init =
  Array.repeat cell_count_w (Array.repeat cell_count_h Cell.Dead)

revertCell : CellCoord -> Model -> Model
revertCell (CellCoord (x, y)) model =
  case Array.get x model of
    Just column -> case Array.get y column of
      Just state ->
        Array.set x (Array.set y (Cell.reverse state) column) model
      Nothing -> model
    Nothing -> model

-- view

draw : List Form
draw = 
  zipWith_Iter (\f c -> f c)
               (List.concat arrangementFuncs)
               (List.concat <| drawCells init)

drawCells : Model -> List (List Form)
drawCells model =
  Array.map (Array.toList << Array.map Cell.draw) model |> Array.toList


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

zipWith_Iter : (a -> b -> c) -> List a -> List b -> List c
zipWith_Iter f l1 l2 = trampoline <| zipWith_Iter' f l1 l2 []

zipWith_Iter' : (a -> b -> c) -> List a -> List b -> List c -> Trampoline (List c)
zipWith_Iter' f l1 l2 acc =
  case l1 of
    []      -> Trampoline.Done acc
    (x::xs) -> let (y::ys) = l2 in
      Trampoline.Continue (\() -> zipWith_Iter' f xs ys (f x y::acc))

-- update

update : Update -> Model -> Model
update update model =
  case update of
    CellClick (CellCoord (x, y)) -> revertCell (CellCoord (x, y)) model
    TimeUp _ -> model


--getCell : CellCoord -> Model -> Cell.Model
--getCell (CellCoord (x, y)) game =
--  case get x game of
--    Just line -> case get y line of
--      Just state -> state
--    Nothing -> Dead
