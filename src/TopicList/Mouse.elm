module TopicList.Mouse exposing (dragStart, drag, dragStop)

import Box
import Config as C
import Env exposing (Env2)
import Feature.Sel as Sel
import Feature.Tool as Tool
import Model exposing (Model, Msg)
import ModelBase exposing (..)
import Outcome exposing (..)
import TopicList.Model as TopicList
import TopicList.TopicListDef exposing (DragState, DropTarget(..))
import TopicMap.ViewModel exposing (toLocalPos)
import Utils as U

import Array



-- ExtManager.NestingDragStart
dragStart : Env2 -> (Model, Cmd Msg)
dragStart {model} =
  ( case model.mouse.dragState of
      Just {ixBoxPath, startPos} ->
        let
          elemPos = toElemPos startPos ixBoxPath model
        in
        model
          |> setDragState (Just (DragState elemPos startPos Nothing))
      Nothing ->
        let
          _ = U.logError "TopicList.Mouse.dragStart" "Unexpected drag state"
            model.mouse.dragState
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


-- ExtManager.NestingDrag
drag : Point -> Env2 -> (Model, Cmd Msg)
drag clientPos {model} =
  ( case (model.mouse.dragState, model.topicList.dragState) of
      (Just {topicId, ixBoxPath}, Just ({elemPos, lastPos} as dragState)) ->
        let
          localPos = toLocalPos clientPos ixBoxPath model
        in
        model
          |> setDragState
            (Just
              { dragState |
                elemPos = { elemPos | y = elemPos.y + clientPos.y - lastPos.y }
              , lastPos = clientPos
              , dropTarget = dropTargetAt localPos topicId model
              }
            )
      _ ->
        let
          _ = U.logError "TopicList.Mouse.drag" "Unexpected drag state"
            (model.mouse.dragState, model.topicList.dragState)
        in
        model
  , Cmd.none
  )


dropTargetAt : Point -> TopicId -> Model -> Maybe DropTarget
dropTargetAt localPos dragTopicId model =
  case model.mouse.hover of
    Just ((T dropTopicId, dropBoxId :: _) as target) ->
      let
        lowerHalf = modBy (C.listItemHeight + 4) (localPos.y - 1) > (C.listItemHeight + 4) // 2
        (dropMode, targetBoxId) =
          case lowerHalf of
            True -> (Drop, dropTopicId)
            False -> (InsertBefore, fromBoxId dropBoxId)
        isCyclic = Box.hadDeepTopic targetBoxId dragTopicId model
      in
      if not isCyclic then
        Just (dropMode target)
      else
        Nothing
    _ -> Nothing


-- ExtManager.NestingDragStop
dragStop : Env2 -> Outcome
dragStop ({model} as env2) =
  let
    outcome =
      case (model.mouse.dragState, model.topicList.dragState) of
        (Just {topicId, boxPath}, Just dragState) ->
          case dragState.dropTarget of
            Just dropTarget ->
              env2
                |> processDrop topicId (Box.firstId boxPath) dropTarget
            Nothing ->
              let
                _ = U.info "TopicList.Mouse.dragStop" "no drop target -> select topic"
              in
              Outcome.with Cmd.none <| Sel.select (T topicId) boxPath model
        _ -> U.logError "TopicList.Mouse.dragStop" (U.toString model.mouse.dragState)
          (Outcome.with Cmd.none model)
  in
  outcome
    |> Outcome.map (setDragState Nothing)


processDrop : TopicId -> BoxId -> DropTarget -> Env2 -> Outcome
processDrop sourceTopicId sourceBoxid dropTarget ({model} as env) =
  let
    _ = U.info "TopicList.Mouse.processDrop"
      { sourceTopicId = sourceTopicId
      , sourceBoxid = sourceBoxid
      , dropTarget = dropTarget
      }
  in
  model
    |> Box.removeTopic sourceTopicId sourceBoxid
    |> \model_ ->
      (case dropTarget of
        Drop (T targetTopicId, _) ->
          let
            boxId = BoxId targetTopicId
          in
          model_
            |> Tool.createBoxOnDemand targetTopicId
            |> Box.addTopic (BoxTopic sourceTopicId Expanded) boxId
            |> TopicList.init boxId
            |> Env.autoSize2 env
        InsertBefore (T targetTopicId, targetBoxId :: _) ->
          model_
            |> Box.addTopic (BoxTopic sourceTopicId Expanded) targetBoxId
            |> TopicList.insertIntoOrder sourceTopicId targetBoxId targetTopicId
            |> Env.autoSize2 env
        _ -> U.logError "TopicList.Mouse.processDrop" (U.toString dropTarget) model_
      )
    |> Outcome (Directives Store Push) Cmd.none


setDragState : Maybe DragState -> Model -> Model
setDragState dragState ({topicList} as model) =
  { model | topicList = { topicList | dragState = dragState }}
