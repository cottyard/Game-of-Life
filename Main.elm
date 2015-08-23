import Game
import Graphics.Element as GElem exposing (Element)
import Signal exposing ((<~))
import Timer
import Text
import Input

main : Signal Element
main =
  layoutMany_live [
    GElem.show <~ Timer.incomingToggleTimer,
    Signal.constant <| GElem.leftAligned <| Text.fromString "    Evolving: ",
    GElem.show <~ Timer.indicator,
    Signal.constant <| GElem.leftAligned <| Text.fromString "Speed: ",
    view <~ state
  ]

type Model = GlobalState Input.Model Game.Model

type Update = UserInput Input.Update
            | Clock

update : Update -> Model -> Model
update update (GlobalState inputM gameM) =
  case update of
    Clock -> 
      GlobalState inputM (Game.update Game.Evolve gameM)
    UserInput inputU -> 
      let nextInputM = Input.update inputU inputM gameM
          gameUpdate = Input.updateGame inputU inputM gameM
          nextGameM = case gameUpdate of
            Just upd -> Game.update upd gameM
            Nothing -> gameM
      in GlobalState nextInputM nextGameM

updated : Signal Update
updated =
  Signal.merge ((\() -> Clock) <~ Timer.timeUp)
               (UserInput <~ Input.updated)
               
view : Model -> Element
view (GlobalState inputM gameM) =
  Game.view gameM

-- todo: add filter for this signal(necessary?)
state : Signal Model
state =
  Signal.foldp update (GlobalState Input.init Game.init) updated

layoutMany_live : List (Signal Element) -> Signal Element
layoutMany_live (head::tail) =
  List.foldl (Signal.map2 layout) head tail

layout : Element -> Element -> Element
layout e1 e2 =
  GElem.flow GElem.right [e1, e2]

