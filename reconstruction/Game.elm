module Game (draw) where

import Graphics.Collage exposing (Form, move)
--import Array exposing (Array, get, set)

import Cell

cell_count_w = 30
cell_count_h = 30

draw : List Form
draw = 
  zipWith (\f c -> f c)
          (List.concat arrangementFuncs)
          (List.concat (List.repeat cell_count_w (List.repeat cell_count_h (Cell.draw Cell.Dead))))

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

zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith f l1 l2 =
  case l1 of
    []      -> []
    (x::xs) -> let (y::ys) = l2 in
      (f x y) :: (zipWith f xs ys)

--type alias Model = Array (Array Cell.Model)
--type alias CellCoord = (Int, Int)
--type Update = CellClick CellCoord | TimeUp Time



--update : Update -> Model -> Model
--update update model =
--  case update of
--    CellClick (CellCoord (x, y)) -> revertCell (CellCoord (x, y)) model
--    TimeUp _ -> model

--revertCell : CellCoord -> Model -> Model
--revertCell (CellCoord (x, y)) game =
--  case get x game of
--    Just line -> case get y line of
--      Just state -> case state of
--        Alive -> set x (set y Dead line) game
--        Dead -> set x (set y Alive line) game
--      Nothing -> game
--    Nothing -> game

--getCell : CellCoord -> Model -> Cell.Model
--getCell (CellCoord (x, y)) game =
--  case get x game of
--    Just line -> case get y line of
--      Just state -> state
--    Nothing -> Dead
