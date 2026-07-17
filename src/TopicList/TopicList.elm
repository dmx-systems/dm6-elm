module TopicList.TopicList exposing (init, targets, listOrder, getSize, reorderTopic,
  isContentHovered, hitTest, autoSize)

import Box
import Config as C
import Dict
import Env exposing (Env)
import Model exposing (Model)
import ModelBase exposing (..)
import TopicList.TopicListDef exposing (TopicList, Targets)
import Utils as U

import Array
import String exposing (fromInt)



-- Dispatch.ExtInit
init : BoxId -> Model -> Model
init boxId model =
  let
    _ = U.info "TopicList.TopicList.init" boxId
  in
  model
    |> initTopicList boxId
    |> initOrder boxId


initTopicList : BoxId -> Model -> Model
initTopicList boxId ({topicList} as model) =
  let
    id = toBoxId boxId
  in
  if Dict.member id topicList.view then
    let
      _ = U.info "TopicList.TopicList.initTopicList"
        ("Box (" ++ U.toString boxId ++ ") has TopicList entry already")
    in
    model
  else
    let
      _ = U.info "TopicList.TopicList.initTopicList"
        ("Creating TopicList entry for box (" ++ U.toString boxId ++ ")")
      topicList_ = TopicList boxId [] (Size 0 0)
    in
    { model | topicList =
      { topicList | view = topicList.view
          |> Dict.insert id topicList_
      }
    }


initOrder : BoxId -> Model -> Model
initOrder boxId ({topicList} as model) =
  model
    |> updateOrder boxId
      (\orderList ->
        let
          missing =
            List.filterMap
              (missingTopicIds orderList)
              (Box.topicIds boxId model)
          _ = U.info "TopicList.TopicList.initOrder"
            ("Add missing TopicList " ++ U.toString missing ++ " to " ++ U.toString orderList)
        in
        missing ++ orderList
      )


missingTopicIds : List TopicId -> TopicId -> Maybe TopicId
missingTopicIds orderList topicId =
  if List.member topicId orderList then
    Nothing
  else
    Just topicId


--

{- Box content as a flat array of targets -}
targets : BoxPath -> Model -> Targets
targets boxPath model =
  Box.traverse
    boxPath
    listOrder
    Array.empty
    targetAcc
    (\_ levelResult -> levelResult)
    model


-- Box.Transform
{- Applies list order to box content. Iterates over list's order list and filters those TopicIds
contained in the box. Note: order list might be longer than box's content list (maintaining
stability).
-}
listOrder : BoxId -> Model -> List TopicId -> List TopicId
listOrder boxId model topicIds =
  let
    isBoxContent : TopicId -> Maybe TopicId
    isBoxContent topicId =
      if List.member topicId topicIds then
        Just topicId
      else
        Nothing
  in
  case fromId boxId model of
    Just {order} -> order
      |> List.filterMap isBoxContent
    Nothing -> U.fail "TopicList.TopicList.listOrder" boxId topicIds


-- Box.Acc
targetAcc : Topic -> Level -> BoxPath -> Targets -> Maybe Targets -> Model -> Targets
targetAcc topic level boxPath acc childrenAcc model =
  acc
    |> Array.push (level, (T topic.id, boxPath))
    |> \t -> Array.append t (childrenAcc |> Maybe.withDefault Array.empty)


{- Inserts a topic ID into the order list at the given insertion point.
-}
reorderTopic : TopicId -> BoxId -> TopicId -> Model -> Model
reorderTopic topicId boxId beforeTopicId model =
  let
    insertion : TopicId -> (List TopicId, Bool) -> (List TopicId, Bool)
    insertion id (list, found) =
      case (id == beforeTopicId, found) of -- at insertion point? insertion point found already?
        (False, _) -> (id :: list, found)
        (True, False) -> ([topicId, id] ++ list, True)
        (True, True) -> U.logError "TopicList.TopicList.reorderTopic"
          "Found more than one insertion point" (list, found)
  in
  model
    |> updateOrder boxId
      (\orderList -> orderList
        |> List.filter (\id -> id /= topicId) -- remove before inserting
        |> List.foldr insertion ([], False)
        |> \(list, found) ->
            case found of
              True -> list
              False -> U.logError "TopicList.TopicList.reorderTopic"
                "Insertion point not found" orderList
      )


{- Canonical order list transformation.
Logs an error if the TopicList entry is missing.
-}
updateOrder : BoxId -> (List TopicId -> List TopicId) -> Model -> Model
updateOrder boxId transform ({topicList} as model) =
  { model | topicList =
    { topicList | view = topicList.view |> Dict.update (toBoxId boxId)
        (\maybeTopicList ->
          case maybeTopicList of
            Just topicList_ -> Just { topicList_ | order = transform topicList_.order }
            Nothing -> U.logError "TopicList.TopicList.updateOrder"
              (U.toString {boxId = boxId}) Nothing
        )
    }
  }



-- HIT TEST


-- Dispatch.ExtHitTest
-- Point is in box-local coordinates
hitTest : BoxId -> BoxPath -> Point -> Maybe TopicId -> Env -> Maybe BoxTarget
hitTest (BoxId topicId as boxId) boxPath localPos maybeFilter {model} =
  if isBoxHovered boxId localPos model then
    let
      fullPath = boxId :: boxPath
    in
    if isContentHovered boxId localPos model then
      let
        index = (localPos.y - 13) // (C.listItemHeight + 4) -- TODO: no magic numbers
        -- _ = U.info "TopicList.TopicList.hitTest" (boxId, index, localPos)
      in
      case Array.get index (targets fullPath model) of
        Just (_, target) -> Just (BoxTarget fullPath target)
        Nothing ->
          U.logError "TopicList.TopicList.hitTest"
            ("Array lookup failed for index " ++ fromInt index)
            Nothing
    else
      Just (BoxTarget fullPath (T topicId, boxPath))
  else
    Nothing


isBoxHovered : BoxId -> Point -> Model -> Bool
isBoxHovered boxId localPos model =
  let
    size = getSize boxId model
  in
  localPos.x > 0 &&
  localPos.x < size.w &&
  localPos.y > 0 &&
  localPos.y < size.h


isContentHovered : BoxId -> Point -> Model -> Bool
isContentHovered boxId localPos model =
  let
    size = getSize boxId model
  in
  localPos.x > 20 &&          -- 40px is <ul> padding-inline-start browser style, TODO
  localPos.x < size.w - 13 &&
  localPos.y > 13 &&          -- 1em is <ul> margin-block-start browser style, TODO
  localPos.y < size.h - 17    -- 1em is <ul> margin-block-end browser style, TODO



-- AUTO-SIZE


-- Dispatch.ExtAutoSize
autoSize : BoxPath -> Env -> (Rectangle, Model)
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
setSize boxId size model =
  model
    |> updateTopicList boxId
      (\topicList -> {topicList | size = size})


{-| Canonical TopicList transformation.
Logs an error if TopicList do not exist.
-}
updateTopicList : BoxId -> (TopicList -> TopicList) -> Model -> Model
updateTopicList boxId transform ({topicList} as model) =
  { model | topicList =
    { topicList | view = topicList.view
        |> Dict.update (toBoxId boxId)
          (\maybeTopicList ->
            case maybeTopicList of
              Just topicList_ -> Just (transform topicList_)
              Nothing -> U.logError "TopicList.TopicList.updateTopicList"
                ("Missing TopicList entry for (" ++ U.toString boxId ++ ")") Nothing
          )
    }
  }


getSize : BoxId -> Model -> Size
getSize boxId model =
  case fromId boxId model of
    Just topicList -> topicList.size
    Nothing -> U.fail "TopicList.TopicList.getSize" boxId (Size 0 0)


{-| Logs an error if the TopicList entry is missing.
-}
fromId : BoxId -> Model -> Maybe TopicList
fromId boxId model =
  case model.topicList.view |> Dict.get (toBoxId boxId) of
    Just topicList -> Just topicList
    Nothing -> U.logError "TopicList.TopicList.fromId"
      ("Missing TopicList entry for (" ++ U.toString boxId ++ ")") Nothing
