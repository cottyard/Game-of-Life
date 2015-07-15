import Color
import Signal exposing ((<~))
import Graphics.Element exposing (Element, show)
import Graphics.Collage exposing (Shape, collage, square, filled, Form, defaultLine, outlined, group, move)
import Mouse
import Window

kCANVAS_WIDTH = 650
kCANVAS_HEIGHT = 650

kCELL_SIZE : Int
kCELL_SIZE = 20

kCELL_COUNT_W : Int
kCELL_COUNT_W = 30

kCELL_COUNT_H : Int
kCELL_COUNT_H = 30

main : Element
main = scene

type CellState = Alive | Dead
type alias GameState = List (List CellState)

initial_game_state : GameState
initial_game_state = List.repeat kCELL_COUNT_H (List.repeat kCELL_COUNT_W Dead)


scene : Element
scene = collage kCANVAS_WIDTH kCANVAS_HEIGHT (draw_cells initial_game_state -300 -300 kCELL_COUNT_W kCELL_COUNT_H)

draw_cells : GameState -> Int -> Int -> Int -> Int -> List Form
draw_cells game_state w_begin h_begin w_cells h_cells =
    let iter game_state line_count =
        case game_state of
            [] -> []
            (line_state :: rest_lines_state) ->
                let line_offset = h_begin + line_count * kCELL_SIZE
                in draw_cells_in_line line_state line_offset w_begin w_cells
                   ++ iter rest_lines_state (line_count + 1)
    in iter game_state 0


create_cell : CellState -> Form
create_cell state = 
    case state of
        Alive -> living_cell
        Dead  -> dead_cell


draw_cells_in_line : List CellState -> Int -> Int -> Int -> List Form
draw_cells_in_line line_state line_offset first_cell_offset cells =
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

cell : Shape
cell = square (toFloat kCELL_SIZE)

living_cell = cell |> filled Color.black 

dead_cell = cell |> outlined defaultLine