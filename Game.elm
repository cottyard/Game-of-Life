module Game (init, view, update, pixelToCellCoord, Model, 
  Update (Metabolism, Evolve), 
  Metabolism (Mitosis, Apoptosis, CircleOfLife), 
  CellCoord (CellCoord),
  getCell) where --eliminate getCell!!

import Graphics.Element as GElem exposing (Element)
import Graphics.Collage as GColl exposing (Form)
import Array exposing (Array)
import Cell
import Matrix exposing (Matrix)
import Util

type alias Model = Matrix Cell.Model

type CellCoord = CellCoord (Int, Int)

(canvas_width, canvas_height) = (1600, 800)
(cell_count_w, cell_count_h) = (100, 50)

-- model

init : Model
init =
  Matrix.create (cell_count_w, cell_count_h) Cell.Dead

revertCell : Model -> CellCoord -> Model
revertCell model coord =
  let cellModel = getCell model coord 
  in setCell model coord (Cell.reverse cellModel)

getCell : Model -> CellCoord -> Cell.Model
getCell model (CellCoord coord) =
  Matrix.get model coord

setCell : Model -> CellCoord -> Cell.Model -> Matrix Cell.Model
setCell model (CellCoord coord) cell =
  Matrix.set model coord cell

getCellNeighbours : Model -> CellCoord -> List Cell.Model
getCellNeighbours model (CellCoord coord) =
  Matrix.neighbours model coord

aliveNeighbours : CellCoord -> Model -> Int
aliveNeighbours coord model =
  let countAlive cellModel count =
    case cellModel of
      Cell.Alive -> count + 1
      Cell.Dead -> count
  in List.foldl countAlive 0 (getCellNeighbours model coord)

evolveCell : CellCoord -> Model -> Cell.Model
evolveCell coord model =
  let n = aliveNeighbours coord model
  in case getCell model coord of
    Cell.Alive -> if
      | n < 2 -> Cell.Dead
      | n > 3 -> Cell.Dead
      | otherwise -> Cell.Alive
    Cell.Dead -> if
      | n == 3 -> Cell.Alive
      | otherwise -> Cell.Dead

evolve : Model -> Model
evolve model =
  Matrix.indexedMap (\index _ -> evolveCell (CellCoord index) model) model

-- view

view : Model -> Element
view =
  scene << draw

scene : List Form -> Element
scene forms =
  GColl.collage canvas_width canvas_height (
    calibrate (-canvas_width / 2 + 50, -canvas_height / 2 + 50) forms)

calibrate : (Float, Float) -> List Form -> List Form
calibrate offset forms =
  List.map (GColl.move offset) forms

draw : Model -> List Form
draw model = 
  Util.zipWith_iter (\f c -> f c)
                    (List.concat arrangementFuncs)
                    (List.concat <| drawCells model)

drawCells : Model -> List (List Form)
drawCells model =
  Array.map (Array.toList << Array.map Cell.draw) (Matrix.toArray model) |> Array.toList

move_Int : (Int, Int) -> Form -> Form
move_Int (x, y) = 
  GColl.move (toFloat x, toFloat y)

arrangementFuncs : List (List (Form -> Form))
arrangementFuncs =
  let getColumnFuncs xPos =
        List.map (move_Int << (,) xPos) (Util.rangeIntList 0 Cell.size cell_count_h)
  in List.map getColumnFuncs (Util.rangeIntList 0 Cell.size cell_count_w)

pixelToCellCoord : (Int, Int) -> Maybe CellCoord
pixelToCellCoord (x, y) =
  let x_coor = (x + Cell.size) // Cell.size - 1
      y_coor = (y + Cell.size) // Cell.size - 1
  in if | y_coor >= cell_count_h || y_coor < 0 ||
          x_coor >= cell_count_w || x_coor < 0 -> Nothing
        | otherwise -> Just (CellCoord (x_coor, y_coor))

-- update

type Update = Metabolism Metabolism 
            | Evolve

type Metabolism = Mitosis CellCoord 
                | Apoptosis CellCoord
                | CircleOfLife CellCoord

update : Update -> Model -> Model
update update model =
  case update of
    Metabolism m -> case m of
      Mitosis coord -> setCell model coord Cell.Alive
      Apoptosis coord -> setCell model coord Cell.Dead
      CircleOfLife coord -> revertCell model coord
    Evolve -> evolve model
