module TopicList.Mouse exposing (dragStart, drag, updateDropTarget, resetDropTarget, dragStop)

import Box
import Config as C
import Env exposing (Env2)
import Feature.Sel as Sel
import Feature.Tool as Tool
import Model exposing (Model, Msg)
import ModelBase exposing (..)
import Outcome exposing (..)
import TopicList.TopicList as TopicList
import TopicList.TopicListDef exposing (DropMode(..))
import TopicMap.ViewModel exposing (toLocalPos)
import Utils as U

import Array



-- ExtManager.ExtDragStart
dragStart : Env2 -> (Model, Cmd Msg)
dragStart {model} =
  ( case model.mouse.dragSource of
      Just {ixBoxPath, startPos} ->
        model
          |> setDragPos (Just (toElemPos startPos ixBoxPath model))
      Nothing ->
        let
          _ = U.logError "TopicList.Mouse.dragStart" "Unexpected drag state"
            model.mouse.dragSource
        in
        model
  , Cmd.none
  )


toElemPos : Point -> BoxPath -> Model -> Point
toElemPos clientPos ixBoxPath model =
  let
    index = toIndex (toLocalPos clientPos ixBoxPath model)
    level =
      case Array.get index (TopicList.targets ixBoxPath model) of
        Just (level_, _) -> level_
        Nothing -> 0
    x = 41 + 41 * level -- 41 = 40 + 1px item border
    y = index * (C.listItemHeight + 4) - level + 16 -- 4 = 2px border (top/bottom) + 2px browser
    _ = U.info "TopicList.Mouse.toElemPos" index
  in
  Point x y


{-| Transforms a box-local position into an index (for "targets" array).
-}
toIndex : Point -> Int
toIndex localPos =
  (localPos.y - 1) // (C.listItemHeight + 4)


-- ExtManager.ExtDrag
drag : Point -> Env2 -> (Model, Cmd Msg)
drag clientPos {model} =
  ( case (model.mouse.dragSource, model.topicList.dragPos) of
      (Just {lastPointerPos}, Just dragPos) ->
        model
          |> setDragPos
            (Just
              { dragPos
              | x = dragPos.x + clientPos.x - lastPointerPos.x
              , y = dragPos.y + clientPos.y - lastPointerPos.y
              }
            )
      _ ->
        let
          _ = U.logError "TopicList.Mouse.drag" "Unexpected drag state"
            (model.mouse.dragSource, model.topicList.dragPos)
        in
        model
  , Cmd.none
  )


-- ExtManager.ExtDropTargeting
updateDropTarget : Point -> Env2 -> (Model, Maybe Target)
updateDropTarget clientPos {model} =
  case model.mouse.dragSource of
    Just {topicId, ixBoxPath} ->
      let
        localPos = toLocalPos clientPos ixBoxPath model
      in
      case dropTargetAt localPos topicId model of
        Just (dropTarget, dropMode) ->
          ( model
              |> setDropMode (Just dropMode)
          , Just dropTarget
          )
        Nothing -> (model, Nothing)
    _ ->
      let
        _ = U.logError "TopicList.Mouse.updateDropTarget" "Unexpected drag state"
          model.mouse.dragSource
      in
      (model, Nothing)


{-| Projects Feature.Mouse's general "hover" state to TopicList specific accepted DropTarget.
-}
dropTargetAt : Point -> TopicId -> Model -> Maybe (Target, DropMode)
dropTargetAt localPos sourceTopicId model =
  case model.mouse.hover of
    Just {target} ->
      case target of
        (T dropTopicId, dropBoxId :: _) ->
          let
            lowerHalf =
              modBy (C.listItemHeight + 4) (localPos.y - 1) > (C.listItemHeight + 4) // 2
            (dropMode, targetBoxId) =
              case lowerHalf of
                True -> (Drop, dropTopicId)
                False -> (InsertBefore, fromBoxId dropBoxId)
            isCyclic = Box.hadDeepTopic targetBoxId sourceTopicId model
          in
          if not isCyclic then
            Just (target, dropMode)
          else
            Nothing
        _ -> Nothing -- TODO: error?
    Nothing -> Nothing


-- ExtManager.ExtDropTargetReset
resetDropTarget : Env2 -> Model
resetDropTarget ({model} as env2) =
  model
    |> setDropMode Nothing -- TODO: needed? Hook still needed in general?


-- ExtManager.ExtDragStop
dragStop : Env2 -> Outcome
dragStop ({model} as env2) =
  let
    outcome =
      case model.mouse.dragSource of
        Just {topicId, boxPath} ->
          case model.mouse.dropTarget of
            Just (T targetTopicId, targetBoxId :: _) ->
              env2
                |> processDrop topicId (Box.firstId boxPath) targetTopicId targetBoxId
            Nothing ->
              let
                _ = U.info "TopicList.Mouse.dragStop" "no drop target -> select topic"
              in
              Outcome.with Cmd.none <| Sel.select (T topicId) boxPath model
            _ ->
              U.logError "TopicList.Mouse.dragStop" (U.toString model.mouse.dropTarget)
                (Outcome.with Cmd.none model)
        _ ->
          U.logError "TopicList.Mouse.dragStop" (U.toString model.mouse.dragSource)
            (Outcome.with Cmd.none model)
  in
  outcome
    |> Outcome.map (setDragPos Nothing)
    |> Outcome.map (setDropMode Nothing)


processDrop : TopicId -> BoxId -> TopicId -> BoxId -> Env2 -> Outcome
processDrop sourceTopicId sourceBoxid targetTopicId targetBoxId ({model} as env) =
  let
    _ = U.info "TopicList.Mouse.processDrop"
      { sourceTopicId = sourceTopicId
      , sourceBoxid = sourceBoxid
      , targetTopicId = targetTopicId
      , targetBoxId = targetBoxId
      , dropMode = model.topicList.dropMode
      }
  in
  model
    |> Box.removeTopic sourceTopicId sourceBoxid
    |> \model_ ->
      (case model.topicList.dropMode of
        Just Drop ->
          let
            boxId = BoxId targetTopicId
          in
          model_
            |> Tool.createBoxOnDemand targetTopicId
            |> Box.addTopic (BoxTopic sourceTopicId Expanded) boxId
            |> TopicList.init boxId
            |> Env.autoSize2 env
        Just InsertBefore ->
          model_
            |> Box.addTopic (BoxTopic sourceTopicId Expanded) targetBoxId
            |> TopicList.insertIntoOrder sourceTopicId targetBoxId targetTopicId
            |> Env.autoSize2 env
        Nothing ->
          U.logError "TopicList.Mouse.processDrop" (U.toString model.topicList.dropMode) model_
      )
    |> Outcome (Directives Store Push) Cmd.none


setDragPos : Maybe Point -> Model -> Model
setDragPos dragPos ({topicList} as model) =
  { model | topicList = { topicList | dragPos = dragPos }}


setDropMode : Maybe DropMode -> Model -> Model
setDropMode dropMode ({topicList} as model) =
  -- let
  --   _ = U.info "TopicList.Mouse.setDropMode" dropMode
  -- in
  { model | topicList = { topicList | dropMode = dropMode }}
