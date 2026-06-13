module Shared.ViewBase exposing (boxStyle, topicBorderStyle, selectionStyle)

import Box
import Config as C
import Feature.Sel as Sel
import Model exposing (Model, Msg)
import ModelBase exposing (..)

import Html.Attributes exposing (style)
import String exposing (fromInt)



boxStyle : BoxId -> BoxPath -> Size -> Bool -> Model -> Attrs Msg
boxStyle boxId boxPath size isTarget model =
  if Box.isFullscreen boxId model then
    []
  else
    nestedBoxStyle boxId boxPath size isTarget model


nestedBoxStyle : BoxId -> BoxPath -> Size -> Bool -> Model -> Attrs Msg
nestedBoxStyle boxId boxPath size isTarget model =
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
  ++ topicBorderStyle (fromBoxId boxId) boxPath isTarget model
  ++ selectionStyle (fromBoxId boxId) boxPath model


topicBorderStyle : TopicId -> BoxPath -> Bool -> Model -> Attrs Msg
topicBorderStyle topicId boxPath isTarget model =
  [ style "border-width" <| fromInt C.topicBorderWidth ++ "px"
  , style "border-style" <| if isTarget then "dashed" else "solid"
  , style "box-sizing" "border-box"
  , style "background-color" "white"
  ]


selectionStyle : TopicId -> BoxPath -> Model -> Attrs Msg
selectionStyle topicId boxPath model =
  case Sel.isSelected (T topicId) boxPath model of
    True -> [ style "box-shadow" C.topicBoxShadow ]
    False -> []
