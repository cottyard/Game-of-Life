import Color
import Signal exposing ((<~))
import Graphics.Element exposing (Element, show)
import Graphics.Collage exposing (Shape, collage, square, filled, Form, defaultLine, outlined, group, move)
import Mouse
import Window

canvas_width = 650
canvas_height = 650

cell_size : Int
cell_size = 20

main : Element
main = scene

scene : Element
scene = collage canvas_width canvas_height (draw_cells -300 -300 30 30)

draw_cells : Int -> Int -> Int -> Int -> List Form
draw_cells w_begin h_begin w_cells h_cells =
    let iter line_count =
        if | line_count <= h_cells ->
                draw_cells_in_line (h_begin + line_count * cell_size) w_begin w_cells
                ++ iter (line_count + 1)
           | otherwise             -> []
    in iter 0

draw_cells_in_line : Int -> Int -> Int -> List Form
draw_cells_in_line line_offset first_cell_offset cells =
    let iter cell_count = 
            if | cell_count <= cells -> 
                    move ( first_cell_offset + cell_count * cell_size |> toFloat, 
                           line_offset |> toFloat ) 
                         living_cell
                    :: iter (cell_count + 1)
               | otherwise           -> []
    in iter 0

cell : Shape
cell = square (toFloat cell_size)

living_cell = cell |> filled Color.black 

dead_cell = cell |> outlined defaultLine