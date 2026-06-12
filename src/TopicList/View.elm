module TopicList.View exposing (view)

import Box
import Config as C
import Env exposing (Env2)
import Feature.Mouse as Mouse
import Feature.Text as Text
import Feature.Tool as Tool exposing (ToolbarPos)
import Model exposing (Model, Msg)
import ModelBase exposing (..)
import Shared.Events as Events
import Shared.ViewBase as VB
import Topic
import TopicList.TopicList as TopicList
import TopicList.TopicListDef exposing (DropTarget(..), Targets)
import Utils as U

import Array
import Html exposing (Html, div, ul, li, text)
import Html.Attributes exposing (style)
import String exposing (fromInt)



type alias HtList = List (Html Msg)



-- ExtManager.ExtBoxView
-- For the fullscreen box boxPath is empty
view : BoxId -> BoxPath -> Env2 -> Html Msg
view boxId boxPath ({model} as env) =
  let
    boxPath_ = boxId :: boxPath
  in
  div
    ( listStyle boxId boxPath model )
    ( (Box.traverseWith
        boxPath_
        TopicList.listOrder
        []
        (viewListItem boxPath_)
        (viewList boxPath_ env)
        model
      )
      ++
      viewDraggingItem boxPath_ model
    )


listStyle : BoxId -> BoxPath -> Model -> Attrs Msg
listStyle boxId boxPath model =
  let
    size = TopicList.getSize boxId model
  in
  VB.boxStyle boxId size boxPath model
    ++ listFontStyle


listFontStyle : Attrs Msg
listFontStyle =
  [ style "font-family" C.mainFont
  , style "font-size" <| fromInt C.contentFontSize ++ "px"
  , style "line-height" <| fromInt C.listItemHeight ++ "px"
  ]


-- Box.LevelDone (ixBoxPath and env are applied already)
viewList : BoxPath -> Env2 -> BoxPath -> HtList -> HtList
viewList ixBoxPath ({model} as env) boxPath topics =
  [ ul
      []
      topics
  ]
  ++ Tool.viewToolbar boxPath (toolbarPos boxPath ixBoxPath model) env


-- Box.Acc (ixBoxPath is applied already)
viewListItem : BoxPath -> Topic -> Level -> BoxPath -> HtList -> Maybe HtList -> Model -> HtList
viewListItem ixBoxPath topic level boxPath acc childrenAcc model =
  acc ++
    [ li
        ( Events.draggable topic.id boxPath ixBoxPath
          ++ VB.selectionStyle topic.id boxPath model
          ++ listItemStyle topic.id boxPath model
          ++ topicBorderStyle topic.id boxPath model
          -- ++ hoverStyle topic.id boxPath model -- debug
        )
        ( viewTopic topic boxPath model
          ++
          case childrenAcc of
            Just children -> children
            Nothing -> []
        )
    ]


listItemStyle : TopicId -> BoxPath -> Model -> Attrs Msg
listItemStyle topicId boxPath model =
  if Mouse.isTopicDragging topicId boxPath model then
    [ style "background-color" "white"
    , style "filter" C.topicLimboFilter
    ]
  else
    []


topicBorderStyle : TopicId -> BoxPath -> Model -> Attrs Msg
topicBorderStyle topicId boxPath model =
  let
    isTarget_ = isTarget (Drop (T topicId, boxPath)) model
  in
  [ style "border-width" <| fromInt C.topicBorderWidth ++ "px"
  , style "border-color" <| if isTarget_ then "black" else "transparent"
  , style "border-style" "dashed"
  ]


-- for hit-test debugging
hoverStyle : TopicId -> BoxPath -> Model -> Attrs Msg
hoverStyle topicId boxPath model =
  if Mouse.isHovered topicId boxPath model then
    [ style "background-color" "orange" ]
  else
    []


viewTopic : Topic -> BoxPath -> Model -> HtList
viewTopic topic boxPath model =
  [ div
      (insertionPointStyle topic.id boxPath model)
      []
  , div
      []
      [ if Text.isEdit topic.id boxPath model then
          Text.viewInput topic boxPath inputStyle
        else
          text (Topic.label topic)
      ]
  ]


insertionPointStyle : TopicId -> BoxPath -> Model -> Attrs Msg
insertionPointStyle topicId boxPath model =
  let
    isTarget_ = isTarget (InsertBefore (T topicId, boxPath)) model
  in
  [ style "height" "2px" ]
  ++
  if isTarget_ then
    [ style "background-color" "black" ]
  else
    []


isTarget : DropTarget -> Model -> Bool
isTarget dropTarget_ model =
  case model.topicList.dropTarget of
    Just dropTarget -> dropTarget == dropTarget_
    _ -> False


viewDraggingItem : BoxPath -> Model -> HtList
viewDraggingItem viewBoxPath model =
  case (model.mouse.dragSource, model.topicList.dragPos) of
    (Just {topicId, ixBoxPath}, Just dragPos) ->
      case (viewBoxPath == ixBoxPath, Topic.fromId topicId model) of
        (True, Just topic) ->
          [ div
              (draggingTopicStyle dragPos)
              [text (Topic.label topic)]
          ]
        _ -> []
    _ -> []


draggingTopicStyle : Point -> Attrs Msg
draggingTopicStyle pos =
  [ style "position" "absolute"
  , style "left" <| fromInt pos.x ++ "px"
  , style "top" <| fromInt pos.y ++ "px"
  ]


inputStyle : Attrs Msg
inputStyle =
  [ style "font-family" C.mainFont -- Default for <input> is "-apple-system" (on Mac)
  , style "font-size" <| fromInt C.contentFontSize ++ "px"
  , style "width" "100%"
  , style "position" "relative"
  , style "left" "-4px"
  ]


-- Toolbar

toolbarPos : BoxPath -> BoxPath -> Model -> ToolbarPos
toolbarPos boxPath ixBoxPath model =
  ToolbarPos
    (\topic ->
      case TopicList.targets ixBoxPath model |> findIndexOf (T topic.id, boxPath) of
        Just (index, level) ->
          Point
            (40 + 40 * level)
            (index * (C.listItemHeight + 4) - 16)
        Nothing -> Point 0 0
    )
    (\assoc -> Point 0 0)


findIndexOf : Target -> Targets -> Maybe (Int, Level)
findIndexOf target targets =
  let
    found =
      targets
        |> Array.toIndexedList
        |> List.filter (\(_, (_, t)) -> t == target)
  in
  case found of
    [(index, (level, _))] -> Just (index, level)
    [] -> U.logError "TopicList.View.findIndexOf"
      ("Target " ++ U.toString target ++ " not found")
      Nothing
    _ -> U.logError "TopicList.View.findIndexOf"
      ("Target " ++ U.toString target ++ " found " ++ fromInt (List.length found) ++ " times")
      Nothing
