module TopicList.Geometry exposing (hitTest, autoSize)

import Array exposing (Array)
import Box
import Config as C
import Dict
import Env exposing (Env2)
import Model exposing (Model)
import ModelBase exposing (..)
import String exposing (fromInt)
import TopicList.TopicList as TL
import TopicList.TopicListDef exposing (BoxProps, Targets)
import Utils as U



-- HIT TEST


-- ExtManager.NestingHitTest
-- Point is in box-local coordinates
hitTest : BoxId -> BoxPath -> Point -> Maybe TopicId -> Env2 -> Maybe Target
hitTest (BoxId topicId as boxId) boxPath pos excludeTopicId {model} =
  if isListHovered boxId pos model then
    let
      t = TL.targets (boxId :: boxPath) model
      index = (pos.y - 13) // (C.listItemHeight + 4) -- TODO: no magic numbers
      -- _ = U.info "TopicList.Geometry.hitTest" index
    in
    case Array.get index t of
      Just (_, target) -> Just target
      Nothing -> Just (T topicId, boxPath)
  else
    Nothing


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


-- ExtManager.NestingAutoSize
autoSize : BoxPath -> Env2 -> (Rectangle, Model)
autoSize boxPath {model} =
  let
    boxId = Box.firstId boxPath
    count = Box.topicCount boxId model
    width = 260 -- ### TODO
    height = count * (C.listItemHeight + 4) + 30
    rect = Rectangle 0 0 width height
    newModel = setSize boxId (Size width height) model
  in
  (rect, newModel)


setSize : BoxId -> Size -> Model -> Model
setSize boxId size ({topicList} as model) =
  model
    |> updateTopicList boxId
      (\list -> {list | size = size})


{-| Canonical BoxProps transformation.
Logs an error if BoxProps do not exist.
-}
updateTopicList : BoxId -> (BoxProps -> BoxProps) -> Model -> Model
updateTopicList boxId transform ({topicList} as model) =
  { model | topicList =
    { topicList | boxProps = topicList.boxProps |> Dict.update (toBoxId boxId)
      (\maybeList ->
        case maybeList of
          Just list -> Just (transform list)
          Nothing -> U.boxNotFound "TopicList.Geometry.updateTopicList" boxId Nothing
      )
    }
  }
