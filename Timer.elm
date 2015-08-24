module Timer (timeUp, indicator, incomingToggleTimer) where

import Signal exposing ((<~))
import Time exposing (Time)
import Keyboard
import Util
import Array

timeUp : Signal ()
timeUp =
  timer

type Update = Acc | Dec | Clock

type alias Speed = Int

type alias Model = 
  { speed : Speed
  , count : Int
  , clockTriggered : Bool
  }

init : Model
init = 
  { speed = 4
  , count = 0
  , clockTriggered = False
  }

-- frequency

timer : Signal ()
timer =
  Signal.filterMap trigger () rawTimer

indicator : Signal Int
indicator =
  .speed <~ rawTimer

rawTimer : Signal Model
rawTimer = 
  Signal.foldp update init incomingUpdate

trigger : Model -> Maybe ()
trigger { speed, count, clockTriggered } =
  if clockTriggered
    then if getThreshold speed <= count then (Just ()) else Nothing
    else Nothing

threshold = Array.fromList [16, 10, 5, 2, 1, 0]
(speedMin, speedMax) = (0, Array.length threshold - 1)

getThreshold : Speed -> Int
getThreshold spd =
  let (Just thres) = Array.get spd threshold
  in thres

update : Update -> Model -> Model
update update mdl =
  let model = { mdl | clockTriggered <- False }
  in case update of
    Dec -> if model.speed > speedMin
           then { model | speed <- model.speed - 1 }
           else model
    Acc -> if model.speed < speedMax
           then { model | speed <- model.speed + 1 }
           else model
    Clock -> if model.count >= getThreshold model.speed
             then { model | count <- 0, clockTriggered <- True }
             else { model | count <- model.count + 1, clockTriggered <- True }

incomingUpdate : Signal Update
incomingUpdate =
  Signal.mergeMany [ always Acc <~ clockAcc
                   , always Dec <~ clockDec
                   , always Clock <~ clock
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