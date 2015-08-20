module Matrix (Matrix, get, set, create, size, toArray, neighbours, indexedMap) where

import Array exposing (Array)
import Maybe exposing (andThen)

--import Graphics.Element exposing (Element, show)
--mapper : Matrix Int -> (Int, Int) -> Int -> Int
--mapper m coord elem =
--  List.foldl (+) 0 (neighbours m coord)

--main : Element
--main = 
--  let m = (create (3, 3) -1)
--  in show (indexedMap (mapper m) m)

type Matrix a = Matrix (Array (Array a)) a

create : (Int, Int) -> a -> Matrix a
create (xcount, ycount) defaultValue =
  Matrix (Array.repeat xcount (Array.repeat ycount defaultValue)) defaultValue

toArray : Matrix a -> (Array (Array a))
toArray (Matrix arrOfArr _) =
  arrOfArr

size : Matrix a -> (Int, Int)
size (Matrix arrOfArr _) =
  let xsize = Array.length arrOfArr
  in case Array.get 0 arrOfArr of
    Nothing    -> (xsize, 0)
    Just arr -> (xsize, Array.length arr)

get : Matrix a -> (Int, Int) -> a
get (Matrix arrOfArr defaultValue) (x, y) =
  let result =
    Array.get x arrOfArr `andThen` \arr ->
    Array.get y arr
  in case result of
    Nothing    -> defaultValue
    Just value -> value

set : Matrix a -> (Int, Int) -> a -> Matrix a
set (Matrix arrOfArr defaultValue) (x, y) value =
  let result = 
    Array.get x arrOfArr `andThen` \arr ->
    let new_arr = Array.set y value arr
    in Just (Matrix (Array.set x new_arr arrOfArr) defaultValue)
  in case result of
    Nothing    -> (Matrix arrOfArr defaultValue)
    Just value -> value

neighbours : Matrix a -> (Int, Int) -> List a
neighbours m (x, y) =
  let (size_x, size_y) = size m
      indexes = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
      actualIndex (dx, dy) = (x + dx, y + dy)
      makeToroidal (x, y) = (x % size_x, y % size_y)
  in List.map (get m << makeToroidal << actualIndex) indexes

indexedMap : ((Int, Int) -> a -> a) -> Matrix a -> Matrix a
indexedMap f (Matrix arrOfArr defaultValue) =
  let perElem x y elem = f (x, y) elem
      perArr x arr = Array.indexedMap (perElem x) arr
  in (Matrix (Array.indexedMap perArr arrOfArr) defaultValue)