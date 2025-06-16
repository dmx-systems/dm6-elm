module Style exposing (..)

import Model exposing (..)
import Html exposing (Attribute)
import Html.Attributes exposing (style)
import String exposing (String, fromInt)
import Svg.Attributes exposing (x1, y1, x2, y2, stroke, strokeWidth)
import Debug exposing (log)



-- CONFIG


borderWidth = 5

topicSize = 24
topicRadius = topicSize // 2 + borderWidth

containerSize = 30
containerOffset = containerSize // 2 + borderWidth
containerRadius = 10

whitebox = { width = 350, height = 200}
whiteboxOffset =
  { x = whitebox.width // 2 + borderWidth
  , y = whitebox.height // 2 + borderWidth
  }
whiteboxRadius = 20

assocWith = 5
selectionColor = "#007AFF" -- Firefox focus color



-- STYLE


appStyle : List (Attribute Msg)
appStyle =
  [ style "font-family" "sans-serif"
  , style "user-select" "none"
  , style "-webkit-user-select" "none" -- Safari still needs vendor prefix
  ]


topicStyle : TopicInfo -> Model -> List (Attribute Msg)
topicStyle { id } model =
  let
    selected = model.selection |> List.member id
    dragging = case model.dragState of
      Drag DragTopic id_ _ _ -> id_ == id
      _ -> False
    targeted = case model.dragState of
      Drag _ _ _ (Just id_) -> id_ == id
      _ -> False
    borderColor =
      if targeted then
        "blue"
      else
        if selected then
          selectionColor
        else
          "transparent"
  in
  [ style "position" "absolute"
  , style "border-width" <| fromInt borderWidth ++ "px"
  , style "border-style" "solid"
  , style "border-color" borderColor
  , style "z-index" <| if dragging then "0" else "1"
  ]


normalStyle : TopicInfo -> TopicProps -> List (Attribute Msg)
normalStyle { color } { pos } =
  [ style "left" <| fromInt (pos.x - topicRadius) ++ "px"
  , style "top" <| fromInt (pos.y - topicRadius) ++ "px"
  , style "width" <| fromInt topicSize ++ "px"
  , style "height" <| fromInt topicSize ++ "px"
  , style "border-radius" <| fromInt topicRadius ++ "px"
  , style "background-color" <| "hsl(" ++ fromInt color ++ ", 70%, 60%)"
  ]


containerStyle : TopicInfo -> TopicProps -> List (Attribute Msg)
containerStyle { color } { pos } =
  [ style "left" <| fromInt (pos.x - containerOffset) ++ "px"
  , style "top" <| fromInt (pos.y - containerOffset) ++ "px"
  , style "width" <| fromInt containerSize ++ "px"
  , style "height" <| fromInt containerSize ++ "px"
  , style "border-radius" <| fromInt containerRadius ++ "px"
  , style "background-color" <| "hsl(" ++ fromInt color ++ ", 70%, 60%)"
  ]


whiteboxStyle : TopicInfo -> TopicProps -> List (Attribute Msg)
whiteboxStyle { color } { pos } =
  [ style "left" <| fromInt (pos.x - whiteboxOffset.x) ++ "px"
  , style "top" <| fromInt (pos.y - whiteboxOffset.y) ++ "px"
  , style "width" <| fromInt whitebox.width ++ "px"
  , style "height" <| fromInt whitebox.height ++ "px"
  , style "border-radius" <| fromInt whiteboxRadius ++ "px"
  , style "background-color" <| "hsl(" ++ fromInt color ++ ", 100%, 95%)"
  ]


itemCountStyle : List (Attribute Msg)
itemCountStyle =
  [ style "position" "relative"
  , style "top" "-1.3em"
  , style "text-align" "center"
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


buttonStyle : List (Attribute Msg)
buttonStyle =
  [ style "display" "block"
  , style "margin-top" "20px"
  ]



-- DEBUG


logError : String -> String -> a -> a
logError func text val =
  log ("### ERROR @" ++ func ++ ": " ++ text) val
