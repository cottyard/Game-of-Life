module Cell (reverse, draw, Model (Alive, Dead), size) where

import Color
import Graphics.Collage exposing (Shape, Form, square, filled, defaultLine, outlined)

-- model

type Model = Alive | Dead

-- update

reverse : Model -> Model
reverse model =
  case model of
    Alive -> Dead
    Dead -> Alive

-- view

size = 15

draw : Model -> Form
draw state = 
  case state of
    Alive -> empty |> filled Color.black 
    Dead  -> empty |> outlined defaultLine

empty : Shape
empty = 
  square (toFloat size)
