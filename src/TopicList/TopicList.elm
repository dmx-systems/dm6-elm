module TopicList.TopicList exposing (init, targets, listOrder, getSize, reorderTopic,
  isContentHovered, hitTest, autoSize)

import Box
import Config as C
import Console
import Env exposing (Env)
import Feature.Crdt
import Feature.CrdtDef as CrdtDef
import Model exposing (Model)
import ModelBase exposing (..)
import SyncModel exposing (SyncModel)
import TopicList.TopicListDef as TopicListDef exposing (TopicList, Targets)

import Crdt
import Crdt.Edit as Edit

import Array
import Dict
import String exposing (fromInt)



{-| Looks up a TopicList in cache.
Logs an error if TopicList is missing.
-}
lookup : BoxId -> Model -> Maybe TopicList
lookup (BoxId (TopicId id)) model =
  case Dict.get id model.cache.topicList of
    Just topicList ->
      Just topicList
    Nothing ->
      Console.logError "TopicList.TopicList.lookup" ("Missing TopicList " ++ id) Nothing


-- Dispatch.InitHook
init : BoxId -> Model -> Model
init boxId model =
  let
    _ = Console.info "TopicList.TopicList.init" boxId
  in
  model
    |> initTopicList boxId
    |> initOrder boxId


initTopicList : BoxId -> Model -> Model
initTopicList (BoxId (TopicId id) as boxId) model =
  if Dict.member id model.cache.topicList then
    let
      _ = Console.info "TopicList.TopicList.initTopicList"
        ("Box " ++ id ++ " has TopicList entry already")
    in
    model
  else
    let
      _ = Console.info "TopicList.TopicList.initTopicList"
        ("Creating TopicList entry for box " ++ id)
    in
    model
      |> create (TopicList boxId [] (Size 0 0))


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
          _ = Console.info "TopicList.TopicList.initOrder" ("Add missing TopicList "
            ++ Console.toString missing ++ " to " ++ Console.toString orderList)
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
  case lookup boxId model of
    Just {order} -> order
      |> List.filterMap isBoxContent
    Nothing -> Console.fail "TopicList.TopicList.listOrder" boxId topicIds


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
        (True, True) -> Console.logError "TopicList.TopicList.reorderTopic"
          "Found more than one insertion point" (list, found)
  in
  model
    |> updateOrder boxId
      (\orderList ->
        orderList
          |> List.filter (\id -> id /= topicId) -- remove before inserting
          |> List.foldr insertion ([], False)
          |> \(list, found) ->
              case found of
                True -> list
                False -> Console.logError "TopicList.TopicList.reorderTopic"
                  "Insertion point not found" orderList
      )


{- Canonical order list transformation.
Logs an error if TopicList is missing.
-}
updateOrder : BoxId -> (List TopicId -> List TopicId) -> Model -> Model
updateOrder boxId transform model =
  case lookup boxId model of
    Just topicList ->
      model
        |> setOrder boxId (transform topicList.order)
    Nothing ->
      let
        _ = Console.fail "TopicList.TopicList.updateOrder" "Can't update order" Nothing
      in
      model



-- HIT TEST


-- Dispatch.HitTestHook
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
        -- _ = Console.info "TopicList.TopicList.hitTest" (boxId, index, localPos)
      in
      case Array.get index (targets fullPath model) of
        Just (_, target) -> Just (BoxTarget fullPath target)
        Nothing ->
          Console.logError "TopicList.TopicList.hitTest"
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


-- Dispatch.AutoSizeHook
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


getSize : BoxId -> Model -> Size
getSize boxId model =
  case lookup boxId model of
    Just topicList -> topicList.size
    Nothing -> Console.fail "TopicList.TopicList.getSize" boxId (Size 0 0)



-- CRDT


create : TopicList -> Model -> Model
create topicList model =
  model.crdt.doc
    |> Edit.setKey CrdtDef.refs.topicList (toBoxId topicList.id) topicList
    |> Feature.Crdt.editResult model


setOrder : BoxId -> List TopicId -> Model -> Model
setOrder (BoxId (TopicId boxId)) order model =
  model.crdt.doc
    |> Edit.set (listOrderRef boxId) order
    |> Feature.Crdt.editResult model


setSize : BoxId -> Size -> Model -> Model
setSize (BoxId (TopicId boxId)) size model =
  model.crdt.doc
    |> Edit.set (listSizeRef boxId) size
    |> Feature.Crdt.editResult model


-- Ref composition

listRef : Id -> Crdt.Ref SyncModel Crdt.Nested TopicList
listRef id =
  CrdtDef.refs.topicList
    |> TopicListDef.topicListsDict.key id


listOrderRef : Id -> Crdt.Ref SyncModel (Crdt.ListK Crdt.Fixed (Crdt.Variants TopicId) TopicId)
                                                                                  (List TopicId)
listOrderRef id =
  listRef id
    |> Crdt.at TopicListDef.topicListDoc.order


listSizeRef : Id -> Crdt.Ref SyncModel Crdt.Nested Size
listSizeRef id =
  listRef id
    |> Crdt.at TopicListDef.topicListDoc.size
