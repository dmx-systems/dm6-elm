module Shared.ViewBase exposing (boxStyle, topicBorderStyle, selectionStyle)

import Box
import Config as C
import Feature.Sel as Sel
import Model exposing (Model, Msg)
import ModelBase exposing (..)
import TopicMap.TopicMapDef exposing (MouseState(..), DragMode(..)) -- TODO: remove dependency?

import Html.Attributes exposing (style)
import String exposing (fromInt)



boxStyle : BoxId -> Size -> BoxPath -> Model -> Attrs Msg
boxStyle boxId size boxPath model =
  if Box.isFullscreen boxId model then
    []
  else
    nestedBoxStyle boxId size boxPath model


nestedBoxStyle : BoxId -> Size -> BoxPath -> Model -> Attrs Msg
nestedBoxStyle boxId size boxPath model =
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


-- TODO: factor out TopicMap specifics?
topicBorderStyle : TopicId -> BoxPath -> Model -> Attrs Msg
topicBorderStyle id boxPath model =
  let
    isTarget_ = isTarget id boxPath
    targeted =
      case (model.mouse.dragState, model.topicMap.mouseState) of
        -- can't move a topic to a box where it is already, can happen if mouse moves very quick
        -- can't create assoc when both topics are in different box
        (Just dragState, Drag DragTopic {dropTarget}) ->
          isTarget_ dropTarget && fromBoxId (Box.firstId dragState.boxPath) /= id
        (Just dragState, Drag DraftAssoc {dropTarget}) ->
          isTarget_ dropTarget && dragState.boxPath == boxPath
        _ -> False
  in
  [ style "border-width" <| fromInt C.topicBorderWidth ++ "px"
  , style "border-style" <| if targeted then "dashed" else "solid"
  , style "box-sizing" "border-box"
  , style "background-color" "white"
  ]


isTarget : TopicId -> BoxPath -> Maybe Target -> Bool
isTarget topicId boxPath maybeTarget =
  case maybeTarget of
    Just target -> target == (T topicId, boxPath)
    Nothing -> False


selectionStyle : TopicId -> BoxPath -> Model -> Attrs Msg
selectionStyle topicId boxPath model =
  case Sel.isSelected (T topicId) boxPath model of
    True -> [ style "box-shadow" C.topicBoxShadow ]
    False -> []
