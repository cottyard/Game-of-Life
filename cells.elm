import Color
import Signal exposing ((<~))
import Graphics.Element exposing (Element, show, flow, right)
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

main : Signal Element
main = scene <~ Signal.foldp revertCell initial_game_state cellClicked

combine : Element -> Element -> Element
combine e1 e2 = flow right [e1, e2]

type CellState = Alive | Dead
type alias CellCoord = (Int, Int)

type alias GameState = Array (Array CellState)
type alias ImmutableGameState = List (List CellState)

revertCell : CellCoord -> GameState -> GameState
revertCell (x, y) game =
    case get x game of
        Just line -> case get y line of
            Just state -> case state of
                Alive -> set x (set y Dead line) game
                Dead -> set x (set y Alive line) game
            Nothing -> game
        Nothing -> game

cellClicked : Signal CellCoord
cellClicked = 
    Signal.filterMap mousePosToCellCoord (0, 0) (Signal.sampleOn Mouse.clicks Mouse.position)

mousePosToCellCoord : (Int, Int) -> Maybe CellCoord
mousePosToCellCoord (x_raw, y_raw) =
    let x_calib = 22
        y_calib = 643
    in let x_coor = (x_raw - x_calib) // kCELL_SIZE
           y_coor = (y_calib - y_raw) // kCELL_SIZE
       in if 
            | y_coor >= kCELL_COUNT_H || x_coor >= kCELL_COUNT_W -> Nothing
            | otherwise -> Just (y_coor, x_coor)


initial_game_state : GameState
initial_game_state = Array.repeat kCELL_COUNT_H (Array.repeat kCELL_COUNT_W Dead)


make_immutable : GameState -> ImmutableGameState
make_immutable game_state = Array.map Array.toList game_state |> Array.toList

scene : GameState -> Element
scene game_state = collage kCANVAS_WIDTH kCANVAS_HEIGHT (draw_cells (make_immutable game_state) -300 -300)

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