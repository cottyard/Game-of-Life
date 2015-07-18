module Update where

import Mouse

import View exposing (mousePosToCellCoord)
import Model exposing (CellCoord)

cellClicked : Signal CellCoord
cellClicked = 
    Signal.filterMap mousePosToCellCoord (0, 0) (Signal.sampleOn Mouse.clicks Mouse.position)

