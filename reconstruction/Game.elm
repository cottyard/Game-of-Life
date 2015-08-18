module Game (draw) where

import Graphics.Collage exposing (Form, move)
--import Array exposing (Array, get, set)

import Cell

draw : List Form
draw = mapWith [move (50, 50), move (50, 70)] [Cell.draw Cell.Dead, Cell.draw Cell.Alive]

mapWith : List (a -> a) -> List a -> List a
mapWith fss xss =
  case xss of
    []     -> []
    (x::xs) -> let (f::fs) = fss in
      (f x) :: mapWith fs xs


--type alias Model = Array (Array Cell.Model)
--type alias CellCoord = (Int, Int)
--type Update = CellClick CellCoord | TimeUp Time

--cell_count_w = 30
--cell_count_h = 30


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
