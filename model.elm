module Model where


import Array exposing (Array, get, set)


kCELL_COUNT_W : Int
kCELL_COUNT_W = 30
kCELL_COUNT_H : Int
kCELL_COUNT_H = 30


type CellState = Alive | Dead
type alias CellCoord = (Int, Int)

type alias GameState = Array (Array CellState)
type alias ImmutableGameState = List (List CellState)

initial_game_state : GameState
initial_game_state = Array.repeat kCELL_COUNT_H (Array.repeat kCELL_COUNT_W Dead)

make_immutable : GameState -> ImmutableGameState
make_immutable game_state = Array.map Array.toList game_state |> Array.toList

getCellState : CellCoord -> GameState -> CellState
getCellState (x, y) game =
  case get x game of
    Just line -> case get y line of
      Just state -> state
    Nothing -> Dead

revertCell : CellCoord -> GameState -> GameState
revertCell (x, y) game =
    case get x game of
        Just line -> case get y line of
            Just state -> case state of
                Alive -> set x (set y Dead line) game
                Dead -> set x (set y Alive line) game
            Nothing -> game
        Nothing -> game
