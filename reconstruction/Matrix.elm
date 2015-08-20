module Matrix (Matrix, get, set, create, toArray) where

import Array exposing (Array)
import Maybe exposing (andThen)

type Matrix a = Matrix (Array (Array a)) a

create : (Int, Int) -> a -> Matrix a
create (xcount, ycount) defaultValue =
  Matrix (Array.repeat xcount (Array.repeat ycount defaultValue)) defaultValue

toArray : Matrix a -> (Array (Array a))
toArray (Matrix m _) =
  m

get : Matrix a -> (Int, Int) -> a
get (Matrix m defaultValue) (x, y) =
  let result =
    Array.get x m `andThen` \arr ->
    Array.get y arr
  in case result of
    Nothing    -> defaultValue
    Just value -> value

set : Matrix a -> (Int, Int) -> a -> Matrix a
set (Matrix m defaultValue) (x, y) value =
  let result = 
    Array.get x m `andThen` \arr ->
    let new_arr = Array.set y value arr
    in Just (Matrix (Array.set x new_arr m) defaultValue)
  in case result of
    Nothing    -> (Matrix m defaultValue)
    Just value -> value