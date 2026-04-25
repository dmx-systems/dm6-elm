module TopicList.Geometry exposing (hitTest, autoSize, toolbarPos)

import Box
import Config as C
import Dict
import Env exposing (ExtManager, Env2)
import Item
import Model exposing (Model)
import ModelBase exposing (..)
import TopicList.TopicList as TL
import TopicList.TopicListDef exposing (TopicList)
import Utils as U



-- HIT TEST


hitTest : BoxId -> BoxPath -> Point -> Maybe Id -> Env2 -> Maybe Target
hitTest boxId boxPath pos excludeTopicId {model} =
  -- TODO
  let
    maybeThisItem : Bool -> Maybe Target
    maybeThisItem found = if found then Just (boxId, boxPath) else Nothing
  in
  if Box.isFullscreen boxId model then
    Nothing
  else
    isListHovered boxId pos model |> maybeThisItem


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
    (\(TopicId id) acc ->
      if Item.isBox id model then
        acc + 1 + topicCount id model
      else
        acc + 1
    )
    0


setSize : BoxId -> Size -> Model -> Model
setSize boxId size ({topicList} as model) =
  { model | topicList = topicList |> Dict.insert boxId (TopicList boxId size) }



-- TOOLBAR


toolbarPos : BoxId -> Model -> ToolbarPos
toolbarPos mapId model =
  -- TODO
  (ToolbarPos (\_ -> Point 0 0) (\_ -> Point 0 0))
