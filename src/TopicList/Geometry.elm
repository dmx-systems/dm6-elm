module TopicList.Geometry exposing (hitTest, autoSize, toolbarPos)

import Array exposing (Array)
import Box
import Config as C
import Dict
import Env exposing (Env2)
import Model exposing (Model)
import ModelBase exposing (..)
import Topic
import TopicList.TopicList as TL
import TopicList.TopicListDef exposing (TopicList)
import Utils as U



-- HIT TEST


hitTest : BoxId -> BoxPath -> Point -> Maybe TopicId -> Env2 -> Maybe Target
hitTest (BoxId topicId as boxId) boxPath pos excludeTopicId {model} =
  if isListHovered boxId pos model then
    let
      t = targets boxId boxPath model
      i = round (toFloat (pos.y - 30) / (C.topicLineHeight * C.contentFontSize))
      target = Array.get i t
      _ = U.info "TopicList.Geometry.hitTest" target
    in
    case target of
      Just _ -> target
      Nothing -> Just (T topicId, boxPath)
  else
    Nothing


targets : BoxId -> BoxPath -> Model -> Array Target
targets boxId boxPath model =
  let
    path = boxId :: boxPath
  in
  Box.topicIds boxId model |> List.foldl
    (\topicId acc ->
      let
        t = Array.push (T topicId, path) acc
      in
      if Topic.isBox topicId model then
        Array.append t (targets (BoxId topicId) path model) -- recursion
      else
        t
    )
    Array.empty


isListHovered : BoxId -> Point -> Model -> Bool
isListHovered boxId pos model =
  let
    size = TL.listSize boxId model
  in
  pos.x > 0 &&
  pos.x < size.w &&
  pos.y > 0 &&
  pos.y < size.h



-- AUTO-SIZE


autoSize : BoxPath -> Env2 -> (Rectangle, Model)
autoSize boxPath {model} =
  let
    boxId = Box.firstId boxPath
    count = topicCount boxId model
    width = 260 -- ### TODO
    height = round (toFloat count * C.topicLineHeight * C.contentFontSize) + 34
    rect = Rectangle 0 0 width height
    newModel = setSize boxId (Size width height) model
  in
  (rect, newModel)


topicCount : BoxId -> Model -> Int
topicCount boxId model =
  Box.topicIds boxId model |> List.foldr
    (\topicId acc ->
      if Topic.isBox topicId model then
        acc + 1 + topicCount (BoxId topicId) model -- recursion
      else
        acc + 1
    )
    0


setSize : BoxId -> Size -> Model -> Model
setSize boxId size ({topicList} as model) =
  { model | topicList =
    { topicList | topicLists = topicList.topicLists
        |> Dict.insert (toBoxId boxId) (TopicList boxId size)
    }
  }



-- TOOLBAR


toolbarPos : BoxId -> Model -> ToolbarPos
toolbarPos mapId model =
  -- TODO
  (ToolbarPos (\_ -> Point 0 0) (\_ -> Point 0 0))
