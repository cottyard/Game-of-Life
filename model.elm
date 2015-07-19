module Model where

import Array exposing (Array, get, set)

kCELL_COUNT_W : Int
kCELL_COUNT_W = 30
kCELL_COUNT_H : Int
kCELL_COUNT_H = 30


type CellState = Alive | Dead
type CellCoord = CellCoord (Int, Int)

type alias GameState = Array (Array CellState)
type alias ImmutableGameState = List (List CellState)

initial_game_state : GameState
initial_game_state = Array.repeat kCELL_COUNT_H (Array.repeat kCELL_COUNT_W Dead)

make_immutable : GameState -> ImmutableGameState
make_immutable game_state = Array.map Array.toList game_state |> Array.toList

getCellState : CellCoord -> GameState -> CellState
getCellState (CellCoord (x, y)) game =
  case get x game of
    Just line -> case get y line of
      Just state -> state
    Nothing -> Dead

getNeighbourCount : CellCoord -> GameState -> Int
getNeighbourCount (CellCoord (x, y)) game =
  let addCoord (dx, dy) = (x + dx, y + dy)
      makeToroidal (x, y) = (x % kCELL_COUNT_W, y % kCELL_COUNT_H)
      neighbour_cell_states = List.map 
        ((flip getCellState game) << CellCoord << makeToroidal << addCoord)
        [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
      countAlive cell_state alive_count =
        case cell_state of
          Alive -> alive_count + 1
          Dead -> alive_count
  in List.foldl countAlive 0 neighbour_cell_states

--evolveStep : GameState -> GameState
--evolveStep game =

combinations : List a -> List a -> List (a, a)
combinations list_1 list_2 =
  case list_1 of
    (h::t) -> (List.map ((,) h) list_2) ++ (combinations t list_2)
    _ -> []

revertCell : CellCoord -> GameState -> GameState
revertCell (CellCoord (x, y)) game =
    case get x game of
        Just line -> case get y line of
            Just state -> case state of
                Alive -> set x (set y Dead line) game
                Dead -> set x (set y Alive line) game
            Nothing -> game
        Nothing -> game
