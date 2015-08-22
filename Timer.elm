module Timer (timeUp, indicator) where

import Signal exposing ((<~))
import Time exposing (Time)
import Keyboard

timeUp : Signal ()
timeUp =
  timer

type Update = Acc | Dec | Clock
type alias Model = 
  { threshold : Int
  , count : Int
  }

(thres_lower, thres_upper) = (0, 16)

init : Model
init = 
  { threshold = 14
  , count = 0 
  }

-- frequency

timer : Signal ()
timer =
  Signal.filterMap trigger () rawTimer

indicator : Signal Int
indicator =
  let modelToSpeed model =
    9 - model.threshold // 2
  in modelToSpeed <~ rawTimer

rawTimer : Signal Model
rawTimer = 
  Signal.foldp update init incomingUpdate

trigger : Model -> Maybe ()
trigger { threshold, count } =
  if threshold <= count then (Just ()) else Nothing

update : Update -> Model -> Model
update update model =
  case update of
    Acc -> if model.threshold > thres_lower
           then { model | threshold <- model.threshold - 2 }
           else model
    Dec -> if model.threshold < thres_upper
           then { model | threshold <- model.threshold + 2 }
           else model
    Clock -> if
      | model.count >= model.threshold -> { model | count <- 0 }
      | otherwise -> { model | count <- model.count + 1 }

incomingUpdate : Signal Update
incomingUpdate =
  Signal.mergeMany [ (\() -> Acc) <~ clockAcc
                   , (\() -> Dec) <~ clockDec
                   , (\_ -> Clock) <~ clock
                   ]

clockAcc : Signal ()
clockAcc =
  whenPressed (Keyboard.isDown 187)

clockDec : Signal ()
clockDec =
  whenPressed (Keyboard.isDown 189)

whenPressed : Signal Bool -> Signal ()
whenPressed = 
  Signal.filterMap (\down -> if down then (Just ()) else Nothing) ()
  

-- toggle

toggleTimer : Bool -> Bool -> Bool
toggleTimer spacePressed timerState =
  if spacePressed then not timerState else timerState

incomingToggleTimer : Signal Bool
incomingToggleTimer =
  Signal.foldp toggleTimer False Keyboard.space

-- signal source

clock : Signal Time
clock =
  Time.fpsWhen 10 incomingToggleTimer