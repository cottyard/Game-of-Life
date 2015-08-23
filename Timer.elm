module Timer (timeUp, indicator, incomingToggleTimer) where

import Signal exposing ((<~))
import Time exposing (Time)
import Keyboard
import Util

timeUp : Signal ()
timeUp =
  timer

type Update = Acc | Dec | Clock

type alias Model = 
  { threshold : Int
  , count : Int
  , clockTriggered : Bool
  }

(thres_lower, thres_upper) = (0, 16)

init : Model
init = 
  { threshold = 4
  , count = 0
  , clockTriggered = False
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
trigger { threshold, count, clockTriggered } =
  if clockTriggered
    then if threshold <= count then (Just ()) else Nothing
    else Nothing

update : Update -> Model -> Model
update update mdl =
  let model = { mdl | clockTriggered <- False }
  in case update of
    Acc -> if model.threshold > thres_lower
           then { model | threshold <- model.threshold - 2 }
           else model
    Dec -> if model.threshold < thres_upper
           then { model | threshold <- model.threshold + 2 }
           else model
    Clock -> if model.count >= model.threshold
             then { model | count <- 0, clockTriggered <- True }
             else { model | count <- model.count + 1, clockTriggered <- True }

incomingUpdate : Signal Update
incomingUpdate =
  Signal.mergeMany [ (\() -> Acc) <~ clockAcc
                   , (\() -> Dec) <~ clockDec
                   , (\_ -> Clock) <~ clock
                   ]

-- toggle

toggleTimer : Bool -> Bool -> Bool
toggleTimer spacePressed timerState =
  if spacePressed then not timerState else timerState

incomingToggleTimer : Signal Bool
incomingToggleTimer =
  Signal.foldp toggleTimer False Keyboard.space

-- signal source

clockAcc : Signal ()
clockAcc =
  Util.whenPressed (Keyboard.isDown 187)

clockDec : Signal ()
clockDec =
  Util.whenPressed (Keyboard.isDown 189)

clock : Signal Time
clock =
  Time.fpsWhen 10 incomingToggleTimer