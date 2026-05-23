module TopicList.Geometry exposing (hitTest, autoSize, toolbarPos)

import Array exposing (Array)
import Box
import Config as C
import Dict
import Env exposing (Env2)
import Model exposing (Model)
import ModelBase exposing (..)
import String exposing (fromInt)
import Topic
import TopicList.TopicList as TL
import TopicList.TopicListDef exposing (BoxProps)
import Utils as U



-- HIT TEST


-- ExtManager.NestingHitTest
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



-- TOOLBAR


-- ExtManager.NestingToolbar
toolbarPos : BoxPath -> Model -> ToolbarPos
toolbarPos boxPath model =
  (ToolbarPos
    (\topic ->
      case TL.targets boxPath model |> findIndexOf (T topic.id, boxPath) of
        Just index ->
          Point
            40
            (index * (C.listItemHeight + 4) - 16)
        Nothing -> Point 0 0
    )
    (\assoc -> Point 0 0)
  )


findIndexOf : Target -> Array (Int, Target) -> Maybe Int
findIndexOf target ts =
  let
    found = ts
      |> Array.toIndexedList
      |> List.filter (\(_, (_, t)) -> t == target)
  in
  case found of
    [(i, _)] -> Just i
    [] -> U.logError "TopicList.Geometry.findIndexOf"
      ("Target " ++ U.toString target ++ " not found")
      Nothing
    _ -> U.logError "TopicList.Geometry.findIndexOf"
      ("Target " ++ U.toString target ++ " found " ++ fromInt (List.length found) ++ " times")
      Nothing
