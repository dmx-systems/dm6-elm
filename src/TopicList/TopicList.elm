module TopicList.TopicList exposing (init, targets, listOrder, getSize, addTopic,
  insertIntoOrder, isListHovered, hitTest, autoSize)

import Box
import Config as C
import Dict
import Env exposing (Env2)
import Model exposing (Model, Msg)
import ModelBase exposing (..)
import Topic
import TopicList.TopicListDef exposing (BoxProps, Targets)
import Utils as U

import Array



-- ExtManager.ExtInit
init : BoxId -> Model -> Model
init boxId model =
  model
    |> Box.topicIds boxId
    |> List.foldl
      (\topicId acc ->
        if Topic.isBox topicId acc then
          init (BoxId topicId) acc -- recursion
        else
          acc
      )
      model
    |> createBoxProps boxId
    |> initOrder boxId


createBoxProps : BoxId -> Model -> Model
createBoxProps boxId ({topicList} as model) =
  let
    id = toBoxId boxId
  in
  if Dict.member id model.topicList.boxProps then
    let
      _ = U.info "TopicList.TopicList.createBoxProps"
        ("Box (" ++ U.toString boxId ++ ") has BoxProps entry already")
    in
    model
  else
    let
      _ = U.info "TopicList.TopicList.createBoxProps"
        ("Creating BoxProps entry for box (" ++ U.toString boxId ++ ")")
    in
    { model | topicList =
      { topicList | boxProps = topicList.boxProps
          |> Dict.insert id (BoxProps boxId [] (Size 0 0))
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
            ("Add missing BoxProps " ++ U.toString missing ++ " to " ++ U.toString orderList)
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
  Box.traverseWith
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
  case boxPropsFromId boxId model of
    Just {order} -> order
      |> List.filterMap isBoxContent
    Nothing -> U.fail "TopicList.TopicList.listOrder" boxId topicIds


-- Box.Acc
targetAcc : Topic -> Level -> BoxPath -> Targets -> Maybe Targets -> Model -> Targets
targetAcc topic level boxPath acc childrenAcc model =
  acc
    |> Array.push (level, (T topic.id, boxPath))
    |> \t -> Array.append t (childrenAcc |> Maybe.withDefault Array.empty)


-- ExtManager.AddTopic
addTopic : TopicId -> BoxId -> Env2 -> Model
addTopic _ boxId ({model} as env) =
  -- Note: added topic might be nested. Needs to init BoxProps recursively.
  -- We just init entire box.
  model
    |> init boxId
    |> Env.autoSize2 env


{- Canonical order list transformation.
Logs an error if the BoxProps entry is missing.
-}
updateOrder : BoxId -> (List TopicId -> List TopicId) -> Model -> Model
updateOrder boxId transform ({topicList} as model) =
  { model | topicList =
    { topicList | boxProps = topicList.boxProps |> Dict.update (toBoxId boxId)
        (\maybeBoxProps ->
          case maybeBoxProps of
            Just boxProps -> Just { boxProps | order = transform boxProps.order }
            Nothing -> U.logError "TopicList.TopicList.updateOrder"
              (U.toString {boxId = boxId}) Nothing
        )
    }
  }


{- Inserts a topic ID into the order list at the given insertion point.
-}
insertIntoOrder : TopicId -> BoxId -> TopicId -> Model -> Model
insertIntoOrder topicId boxId beforeTopicId model =
  let
    insertion : TopicId -> (List TopicId, Bool) -> (List TopicId, Bool)
    insertion id (list, found) =
      case (id == beforeTopicId, found) of -- at insertion point? insertion point found already?
        (False, _) -> (id :: list, found)
        (True, False) -> ([topicId, id] ++ list, True)
        (True, True) -> U.logError "TopicList.TopicList.insertIntoOrder"
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
              False -> U.logError "TopicList.TopicList.insertIntoOrder"
                "Insertion point not found" orderList
      )



-- HIT TEST


-- ExtManager.ExtHitTest
-- Point is in box-local coordinates
hitTest : BoxId -> BoxPath -> Point -> Maybe TopicId -> Env2 -> Maybe BoxTarget
hitTest (BoxId topicId as boxId) boxPath localPos maybeFilter {model} =
  if isListHovered boxId localPos model then
    let
      fullPath = boxId :: boxPath
      t = targets fullPath model
      index = (localPos.y - 13) // (C.listItemHeight + 4) -- TODO: no magic numbers
      -- _ = U.info "TopicList.TopicList.hitTest" (boxId, index)
    in
    case Array.get index t of
      Just (_, target) -> Just (BoxTarget fullPath target)
      Nothing -> Just (BoxTarget fullPath (T topicId, boxPath))
  else
    Nothing


isListHovered : BoxId -> Point -> Model -> Bool
isListHovered boxId localPos model =
  let
    size = getSize boxId model
  in
  localPos.x > 0 &&
  localPos.x < size.w &&
  localPos.y > 0 &&
  localPos.y < size.h



-- AUTO-SIZE


-- ExtManager.ExtAutoSize
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
setSize boxId size model =
  model
    |> updateBoxProps boxId
      (\boxProps -> {boxProps | size = size})


{-| Canonical BoxProps transformation.
Logs an error if BoxProps do not exist.
-}
updateBoxProps : BoxId -> (BoxProps -> BoxProps) -> Model -> Model
updateBoxProps boxId transform ({topicList} as model) =
  { model | topicList =
    { topicList | boxProps = topicList.boxProps
        |> Dict.update (toBoxId boxId)
          (\maybeBoxProps ->
            case maybeBoxProps of
              Just boxProps -> Just (transform boxProps)
              Nothing -> U.logError "TopicList.TopicList.updateBoxProps"
                ("Missing BoxProps entry for (" ++ U.toString boxId ++ ")") Nothing
          )
    }
  }


getSize : BoxId -> Model -> Size
getSize boxId model =
  case boxPropsFromId boxId model of
    Just boxProps -> boxProps.size
    Nothing -> U.fail "TopicList.TopicList.getSize" boxId (Size 0 0)


{-| Logs an error if the BoxProps entry is missing.
-}
boxPropsFromId : BoxId -> Model -> Maybe BoxProps
boxPropsFromId boxId model =
  case model.topicList.boxProps |> Dict.get (toBoxId boxId) of
    Just boxProps -> Just boxProps
    Nothing -> U.logError "TopicList.TopicList.boxPropsFromId"
      ("Missing BoxProps entry for (" ++ U.toString boxId ++ ")") Nothing
