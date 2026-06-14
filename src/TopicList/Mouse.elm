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
import TopicList.TopicListDef exposing (DropTarget(..))
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
updateDropTarget : Point -> Env2 -> Model
updateDropTarget clientPos {model} =
  case model.mouse.dragSource of
    Just {topicId, ixBoxPath} ->
      model
        |> setDropTarget
          (dropTargetAt
            (toLocalPos clientPos ixBoxPath model)
            topicId model
          )
    _ ->
      let
        _ = U.logError "TopicList.Mouse.updateDropTarget" "Unexpected drag state"
          model.mouse.dragSource
      in
      model


{-| Projects Feature.Mouse's general "hover" state to TopicList specific accepted DropTarget.
-}
dropTargetAt : Point -> TopicId -> Model -> Maybe DropTarget
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
            Just (dropMode target)
          else
            Nothing
        _ -> Nothing -- TODO: error?
    Nothing -> Nothing


-- ExtManager.ExtDropTargetReset
resetDropTarget : Env2 -> Model
resetDropTarget ({model} as env2) =
  model
    |> setDropTarget Nothing


-- ExtManager.ExtDragStop
dragStop : Env2 -> Outcome
dragStop ({model} as env2) =
  let
    outcome =
      case model.mouse.dragSource of
        Just {topicId, boxPath} ->
          case model.topicList.dropTarget of
            Just dropTarget ->
              env2
                |> processDrop topicId (Box.firstId boxPath) dropTarget
            Nothing ->
              let
                _ = U.info "TopicList.Mouse.dragStop" "no drop target -> select topic"
              in
              Outcome.with Cmd.none <| Sel.select (T topicId) boxPath model
        _ -> U.logError "TopicList.Mouse.dragStop" (U.toString model.mouse.dragSource)
          (Outcome.with Cmd.none model)
  in
  outcome
    |> Outcome.map (setDragPos Nothing)
    |> Outcome.map (setDropTarget Nothing)


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


setDragPos : Maybe Point -> Model -> Model
setDragPos dragPos ({topicList} as model) =
  { model | topicList = { topicList | dragPos = dragPos }}


setDropTarget : Maybe DropTarget -> Model -> Model
setDropTarget dropTarget ({topicList} as model) =
  { model | topicList = { topicList | dropTarget = dropTarget }}
