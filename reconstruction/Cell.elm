module Cell (reverseCell, drawCell, Model (Alive, Dead)) where

import Color
import Graphics.Collage exposing (Shape, Form, square, filled, defaultLine, outlined, group, move)

-- model

type Model = Alive | Dead

-- update

reverseCell : Model -> Model
reverseCell model =
  case model of
    Alive -> Dead
    Dead -> Alive

-- view

kCELL_SIZE = 20

drawCell : Model -> Form
drawCell state = 
  case state of
    Alive -> emptyCell |> filled Color.black 
    Dead  -> emptyCell |> outlined defaultLine

emptyCell : Shape
emptyCell = 
  square (toFloat kCELL_SIZE)
