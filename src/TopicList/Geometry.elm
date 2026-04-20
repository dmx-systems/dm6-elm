module TopicList.Geometry exposing (hitTest, autoSize)

import Box
import Config as C
import Dict
import Env exposing (ExtManager)
import Item
import Model exposing (Model)
import ModelBase exposing (Id, BoxId, BoxPath, Target, Point, Rectangle, Size)
import TopicList.TopicList as TL
import TopicList.TopicListDef exposing (TopicList)
import Utils as U

import String exposing (fromInt)



-- Hit Test

hitTest : BoxId -> BoxPath -> Point -> Maybe Id -> ExtManager -> Model -> Maybe Target
hitTest boxId boxPath pos excludeTopicId ext model =
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


-- Auto-Size

autoSize : BoxPath -> ExtManager -> Model -> (Rectangle, Model)
autoSize boxPath ext model =
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
  Box.topics boxId model |> List.foldr
    (\topic acc ->
      if Item.isBox topic.id model then
        acc + 1 + topicCount topic.id model
      else
        acc + 1
    )
    0


setSize : BoxId -> Size -> Model -> Model
setSize boxId size ({topicList} as model) =
  { model | topicList = topicList |> Dict.insert boxId (TopicList boxId size) }
