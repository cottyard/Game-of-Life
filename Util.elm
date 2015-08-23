module Util where

--import Trampoline exposing (Trampoline, trampoline)
import Signal exposing (Signal)

rangeIntList : Int -> Int -> Int -> List Int
rangeIntList begin step size =
  List.map ((*) step >> (+) begin) [0..size - 1]

---- Elm 0.15 doesn't support Tail Call Elimination; abandoning this function
--zipWith : (a -> b -> c) -> List a -> List b -> List c
--zipWith f l1 l2 =
--  case l1 of
--    []      -> []
--    (x::xs) -> let (y::ys) = l2 in
--      f x y :: zipWith f xs ys

--zipWith_iter : (a -> b -> c) -> List a -> List b -> List c
--zipWith_iter f l1 l2 = trampoline <| zipWith_iter' f l1 l2 []

--zipWith_iter' : (a -> b -> c) -> List a -> List b -> List c -> Trampoline (List c)
--zipWith_iter' f l1 l2 acc =
--  case l1 of
--    []      -> Trampoline.Done acc
--    (x::xs) -> let (y::ys) = l2 in
--      Trampoline.Continue (\() -> zipWith_iter' f xs ys (f x y::acc))

whenPressed : Signal Bool -> Signal ()
whenPressed = 
  Signal.filterMap (\down -> if down then (Just ()) else Nothing) ()

whenReleased : Signal Bool -> Signal ()
whenReleased = 
  Signal.filterMap (\down -> if not down then (Just ()) else Nothing) ()
