import Color
import Signal exposing ((<~))
import Graphics.Element exposing (Element, show)
import Graphics.Collage exposing (Shape, collage, square, filled, Form, defaultLine, outlined, group, move)
import Mouse
import Window
import Array exposing (Array, get, set)

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
type alias GameState = Array (Array CellState)

type alias ImmutableGameState = List (List CellState)

initial_game_state : GameState
initial_game_state = 
    let blank_state = (Array.repeat kCELL_COUNT_H (Array.repeat kCELL_COUNT_W Dead))
    in case get 3 blank_state of
        Just line ->
            set 3 (set 4 Alive line) blank_state
        Nothing ->
            blank_state

make_immutable : GameState -> ImmutableGameState
make_immutable game_state = Array.map Array.toList game_state |> Array.toList

scene : Element
scene = collage kCANVAS_WIDTH kCANVAS_HEIGHT (draw_cells (make_immutable initial_game_state) -300 -300)

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
        Alive -> living_cell
        Dead  -> dead_cell


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

cell : Shape
cell = square (toFloat kCELL_SIZE)

living_cell = cell |> filled Color.black 

dead_cell = cell |> outlined defaultLine