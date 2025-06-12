module Style exposing (..)

import Model exposing (..)
import Html exposing (Attribute)
import Html.Attributes exposing (style)
import Debug exposing (log)



-- STYLE


appStyle : List (Attribute Msg)
appStyle =
  []


topicStyle : Model -> Id -> Point -> Color -> List (Attribute Msg)
topicStyle model id pos color =
  let
    dragging = case model.dragState of
      DragTopic id_ _ _ -> id_ == id
      _ -> False
    targeted = case model.dragState of
      DragTopic _ _ (Just id_) -> id_ == id
      _ -> False
  in
  [ style "position" "absolute"
  , style "left" <| String.fromInt pos.x ++ "px"
  , style "top" <| String.fromInt pos.y ++ "px"
  , style "width" "30px"
  , style "height" "30px"
  , style "border-radius" "17px"
  , style "border-width" "2px"
  , style "border-style" "solid"
  , style "border-color" <| if targeted then "blue" else "transparent"
  , style "z-index" <| if dragging then "0" else "1"
  , style "background-color" <| "hsl(" ++ String.fromInt color ++ ", 70%, 60%)"
  ]



-- DEBUG


logError : String -> String -> a -> a
logError func text val =
  log ("### ERROR @" ++ func ++ ": " ++ text) val
