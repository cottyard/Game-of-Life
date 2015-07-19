module View where

import Color
import Graphics.Element exposing (Element)
import Graphics.Collage exposing (Shape, collage, square, filled, Form, defaultLine, outlined, group, move)

import Model exposing (GameState, ImmutableGameState, CellState, CellCoord, kCELL_COUNT_H, kCELL_COUNT_W)

kCANVAS_WIDTH : Int
kCANVAS_WIDTH = 650
kCANVAS_HEIGHT : Int
kCANVAS_HEIGHT = 650

kCELL_SIZE : Int
kCELL_SIZE = 20

scene : GameState -> Element
scene game_state =
  collage kCANVAS_WIDTH kCANVAS_HEIGHT (draw_cells (Model.make_immutable game_state) -300 -300)

draw_cells : ImmutableGameState -> Int -> Int -> List Form
draw_cells game_state w_begin h_begin =
    let iter game_state line_count =
        case game_state of
            [] -> []
            (line_state :: rest_lines_state) ->
                let line_offset = h_begin + line_count * kCELL_SIZE
                in draw_cells_in_line line_state line_offset w_begin
                   ++ iter rest_lines_state (line_count + 1)
    in iter game_state 0

create_cell : CellState -> Form
create_cell state = 
    case state of
        Model.Alive -> living_cell
        Model.Dead  -> dead_cell

draw_cells_in_line : List CellState -> Int -> Int -> List Form
draw_cells_in_line line_state line_offset first_cell_offset =
    let iter line_state cell_count =
        case line_state of
            [] -> []
            (cell_state :: rest_cells_state) ->
                let cell_offset = first_cell_offset + cell_count * kCELL_SIZE
                in move ( cell_offset |> toFloat, 
                          line_offset |> toFloat )
                        ( create_cell cell_state )
                   :: iter rest_cells_state (cell_count + 1)
    in iter line_state 0

mousePosToCellCoord : (Int, Int) -> Maybe CellCoord
mousePosToCellCoord (x_raw, y_raw) =
    let x_calib = 22
        y_calib = 645
    in let x_coor = (x_raw - x_calib + kCELL_SIZE) // kCELL_SIZE - 1
           y_coor = (y_calib - y_raw + kCELL_SIZE) // kCELL_SIZE - 1
       in if 
            | y_coor >= kCELL_COUNT_H || y_coor < 0 ||
              x_coor >= kCELL_COUNT_W || x_coor < 0 -> Nothing
            | otherwise -> Just (Model.CellCoord (y_coor, x_coor))

cell : Shape
cell = square (toFloat kCELL_SIZE)

living_cell : Form
living_cell = cell |> filled Color.black 

dead_cell : Form
dead_cell = cell |> outlined defaultLine

