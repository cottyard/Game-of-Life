module Util where

import Trampoline exposing (Trampoline, trampoline)

rangeIntList : Int -> Int -> Int -> List Int
rangeIntList begin step size =
  List.map ((*) step >> (+) begin) [0..size - 1]

-- Elm 0.15 doesn't support Tail Call Elimination; abandoning this function
zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith f l1 l2 =
  case l1 of
    []      -> []
    (x::xs) -> let (y::ys) = l2 in
      f x y :: zipWith f xs ys

zipWith_iter : (a -> b -> c) -> List a -> List b -> List c
zipWith_iter f l1 l2 = trampoline <| zipWith_iter' f l1 l2 []

zipWith_iter' : (a -> b -> c) -> List a -> List b -> List c -> Trampoline (List c)
zipWith_iter' f l1 l2 acc =
  case l1 of
    []      -> Trampoline.Done acc
    (x::xs) -> let (y::ys) = l2 in
      Trampoline.Continue (\() -> zipWith_iter' f xs ys (f x y::acc))
