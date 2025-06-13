module Style exposing (..)

import Model exposing (..)
import Html exposing (Attribute)
import Html.Attributes exposing (style)
import String exposing (String, fromInt)
import Svg.Attributes exposing (x1, y1, x2, y2, stroke, strokeWidth)
import Debug exposing (log)



-- CONFIG

topicSize = 24
topicBorder = 5
topicRadius = topicSize // 2 + topicBorder
assocWith = 5



-- STYLE


appStyle : List (Attribute Msg)
appStyle =
  []


topicStyle : Model -> TopicInfo -> List (Attribute Msg)
topicStyle model { id, pos, color } =
  let
    dragging = case model.dragState of
      DragTopic id_ _ _ -> id_ == id
      _ -> False
    targeted = case model.dragState of
      DragTopic _ _ (Just id_) -> id_ == id
      _ -> False
  in
  [ style "position" "absolute"
  , style "left" <| fromInt (pos.x - topicRadius) ++ "px"
  , style "top" <| fromInt (pos.y - topicRadius) ++ "px"
  , style "width" <| fromInt topicSize ++ "px"
  , style "height" <| fromInt topicSize ++ "px"
  , style "border-radius" <| fromInt topicRadius ++ "px"
  , style "border-width" <| fromInt topicBorder ++ "px"
  , style "border-style" "solid"
  , style "border-color" <| if targeted then "blue" else "transparent"
  , style "z-index" <| if dragging then "0" else "1"
  , style "background-color" <| "hsl(" ++ fromInt color ++ ", 70%, 60%)"
  ]


svgStyle : List (Attribute Msg)
svgStyle =
  [ style "position" "absolute"
  , style "top" "0"
  , style "left" "0"
  , style "z-index" "-1" -- behind the nodes
  ]


lineStyle : Point -> Point -> List (Attribute Msg)
lineStyle pos1 pos2 =
  [ x1 <| fromInt pos1.x
  , y1 <| fromInt pos1.y
  , x2 <| fromInt pos2.x
  , y2 <| fromInt pos2.y
  , stroke "gray"
  , strokeWidth <| fromInt assocWith ++ "px"
  ]



-- DEBUG


logError : String -> String -> a -> a
logError func text val =
  log ("### ERROR @" ++ func ++ ": " ++ text) val
