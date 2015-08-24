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

empty : Shape
empty = 
  square (toFloat size)

cellFormAlive : Form
cellFormAlive =
  filled Color.black empty

cellFormDead : Form
cellFormDead =
  outlined defaultLine empty

draw : Model -> Form
draw state = 
  case state of
    Alive -> cellFormAlive
    Dead  -> cellFormDead
