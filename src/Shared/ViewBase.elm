module Shared.ViewBase exposing (boxStyle, topicBorderStyle, selectionStyle, hGap, vGap)

import Box
import Config as C
import Feature.Mouse as Mouse
import Feature.Sel as Sel
import Model exposing (Model, Msg)
import ModelBase exposing (..)

import Html exposing (Html, div, span)
import Html.Attributes exposing (style)
import String exposing (fromInt)



boxStyle : BoxId -> BoxPath -> Size -> Model -> Attrs Msg
boxStyle boxId boxPath size model =
  if Box.isFullscreen boxId model then
    []
  else
    nestedBoxStyle boxId boxPath size model


nestedBoxStyle : BoxId -> BoxPath -> Size -> Model -> Attrs Msg
nestedBoxStyle boxId boxPath size model =
  let
    r = fromInt C.whiteBoxRadius ++ "px"
  in
  [ style "position" "absolute"
  , style "left" <| fromInt -C.topicBorderWidth ++ "px"
  , style "top" <| fromInt (C.topicSize.h - 2 * C.topicBorderWidth) ++ "px"
  , style "width" <| fromInt size.w ++ "px"
  , style "height" <| fromInt size.h ++ "px"
  , style "border-radius" <| "0 " ++ r ++ " " ++ r ++ " " ++ r
  ]
  ++ topicBorderStyle (fromBoxId boxId) boxPath model
  ++ selectionStyle (fromBoxId boxId) boxPath model


topicBorderStyle : TopicId -> BoxPath -> Model -> Attrs Msg
topicBorderStyle topicId boxPath model =
  let
    isDropTarget = Mouse.isDropTarget topicId boxPath model
  in
  [ style "border-width" <| fromInt C.topicBorderWidth ++ "px"
  , style "border-style" <| if isDropTarget then "dashed" else "solid"
  , style "box-sizing" "border-box"
  , style "background-color" "white"
  ]


selectionStyle : TopicId -> BoxPath -> Model -> Attrs Msg
selectionStyle topicId boxPath model =
  case Sel.isSelected (T topicId) boxPath model of
    True -> [ style "box-shadow" C.topicBoxShadow ]
    False -> []


--

hGap : Int -> Html Msg
hGap gap =
  span
    [ style "display" "inline-block"
    , style "width" <| fromInt gap ++ "px"
    ]
    []


vGap : Int -> Html Msg
vGap gap =
  div
    [ style "height" <| fromInt gap ++ "px" ]
    []
