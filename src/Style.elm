module Style exposing (..)

import Model exposing (..)
import Html exposing (Attribute)
import Html.Attributes exposing (style)
import String exposing (String, fromInt)
import Svg.Attributes exposing (x1, y1, x2, y2, stroke, strokeWidth)



-- CONFIG


mainFontSize = "14px"
selectionColor = "#007AFF" -- Firefox focus color

borderWidth = 5

topicSize = 34
topicRadius = topicSize // 2
topicRect = Rectangle -topicRadius -topicRadius topicRadius topicRadius

blackBoxSize = 42
blackBoxOffset = blackBoxSize // 2
blackBoxRadius = 10
blackBoxRect = Rectangle -blackBoxOffset -blackBoxOffset blackBoxOffset blackBoxOffset

whiteboxRange = { width = 250, height = 150 }
whiteboxRadius = 20

assocWith = 5



-- STYLE


appStyle : List (Attribute Msg)
appStyle =
  [ style "font-family" "sans-serif"
  , style "font-size" mainFontSize
  , style "user-select" "none"
  , style "-webkit-user-select" "none" -- Safari still needs vendor prefix
  ]


toolbarStyle : List (Attribute Msg)
toolbarStyle =
  [ style "display" "inline-flex"
  , style "flex-direction" "column"
  , style "align-items" "flex-start"
  , style "gap" "28px"
  , style "margin-top" "20px"
  ]


displayModeStyle : Bool -> List (Attribute Msg)
displayModeStyle disabled =
  let
    (color, pointerEvents) =
      if disabled then
        ("gray", "none")
      else
        ("unset", "unset")
  in
  [ style "display" "flex"
  , style "flex-direction" "column"
  , style "gap" "6px"
  , style "color" color
  , style "pointer-events" pointerEvents
  ]


buttonStyle : List (Attribute Msg)
buttonStyle =
  [ style "font-family" "sans-serif"
  , style "font-size" mainFontSize
  ]


topicStyle : TopicInfo -> MapId -> Model -> List (Attribute Msg)
topicStyle { id } mapId model =
  let
    selected = model.selection |> List.member (id, mapId)
    dragging = case model.dragState of
      Drag DragTopic id_ _ _ _ -> id_ == id -- TODO: mapId?
      _ -> False
    targeted = case model.dragState of
      Drag _ _ _ _ (Just target) -> target == (id, mapId)
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
  , style "box-sizing" "border-box"
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


blackBoxStyle : TopicInfo -> TopicProps -> List (Attribute Msg)
blackBoxStyle { color } { pos } =
  [ style "left" <| fromInt (pos.x - blackBoxOffset) ++ "px"
  , style "top" <| fromInt (pos.y - blackBoxOffset) ++ "px"
  , style "width" <| fromInt blackBoxSize ++ "px"
  , style "height" <| fromInt blackBoxSize ++ "px"
  , style "border-radius" <| fromInt blackBoxRadius ++ "px"
  , style "background-color" <| "hsl(" ++ fromInt color ++ ", 70%, 60%)"
  ]


whiteboxStyle : TopicInfo -> TopicProps -> Rectangle -> Offset -> List (Attribute Msg)
whiteboxStyle { color } { pos } rect offset =
  [ style "left" <| fromInt (pos.x + rect.x1 + offset.x) ++ "px"
  , style "top" <| fromInt (pos.y + rect.y1 + offset.y) ++ "px"
  , style "width" <| fromInt (rect.x2 - rect.x1) ++ "px"
  , style "height" <| fromInt (rect.y2 - rect.y1) ++ "px"
  , style "border-radius" <| fromInt whiteboxRadius ++ "px"
  , style "background-color" <| "hsl(" ++ fromInt color ++ ", 100%, 95%)"
  ]


itemCountStyle : List (Attribute Msg)
itemCountStyle =
  [ style "position" "relative"
  , style "top" "-1.3em"
  , style "text-align" "center"
  ]


nestedMapStyle : List (Attribute Msg)
nestedMapStyle =
  [ style "position" "absolute"
  , style "width" "100%"
  , style "height" "100%"
  ]


topicLayerStyle : Rectangle -> List (Attribute Msg)
topicLayerStyle mapRect =
  [ style "position" "absolute"
  , style "left" <| fromInt (-mapRect.x1 - borderWidth) ++ "px"
  , style "top" <| fromInt (-mapRect.y1 - borderWidth) ++ "px"
  ]


svgStyle : List (Attribute Msg)
svgStyle =
  [ style "position" "absolute"
  , style "top" <| fromInt -borderWidth ++ "px"
  , style "left" <| fromInt -borderWidth ++ "px"
  , style "z-index" "-1" -- behind the topic layer
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
