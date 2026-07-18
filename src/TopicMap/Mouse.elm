module TopicMap.Mouse exposing (dragStart, drag, updateDropTarget, dragStop, timeArrived,
  isDraftAssoc)

import Assoc
import Box
import Config as C
import Console
import Env exposing (Env)
import Extension
import Feature.Sel as Sel
import Model exposing (Model, Msg(..))
import ModelBase exposing (..)
import Outcome exposing (..)
import TopicMap.TopicMap as TopicMap
import TopicMap.TopicMapDef as TopicMapDef exposing (DragState(..), DragMode(..))

import String exposing (fromInt)
import Task
import Time exposing (Posix, posixToMillis)



-- UPDATE


-- Dispatch.ExtDragStart
dragStart : Env -> (Model, Cmd Msg)
dragStart {model} =
  case model.mouse.dragSource of
    Just {topicId, boxPath} ->
      ( model
          |> setDragState (Just WaitForStartTime)
      , Cmd.batch
          [ Outcome.command (Cancel (Just (T topicId, boxPath)))
          , Task.perform (TopicMap << TopicMapDef.GotTime) Time.now
          ]
      )
    Nothing ->
      let
        _ = Console.logError "TopicMap.Mouse.dragStart" "Unexpected drag source"
          model.mouse.dragSource
      in
      (model, Cmd.none)


timeArrived : Posix -> Model -> Outcome
timeArrived time model =
  case (model.mouse.dragSource, model.topicMap.dragState) of
    (_, Just WaitForStartTime) ->
      model
        |> setDragState (Just <| DragEngaged time)
        |> Outcome.default
    (Just {topicId, boxPath}, Just (WaitForEndTime startTime)) ->
      let
        delay = posixToMillis time - posixToMillis startTime
        (dragState, historyOp) =
          if delay > C.assocDelayMillis then
            (Just <| Drag DraftAssoc, Swap)
          else
            case TopicMap.topicPos topicId (Box.firstId boxPath) model of
              Just origPos -> (Just <| Drag <| DragTopic origPos, Push)
              Nothing -> (Nothing, Swap) -- error is already logged
      in
      model
        |> setDragState dragState
        |> Outcome.from (Directives NoStore historyOp)
    _ ->
      let
        _ = Console.logError "TopicMap.Mouse.timeArrived" "Unexpected drag state"
          (model.mouse.dragSource, model.topicMap.dragState)
      in
      model
        |> Outcome.default


-- Dispatch.ExtDrag
drag : Point -> Env -> (Model, Cmd Msg)
drag clientPos ({model} as env) =
  case model.topicMap.dragState of
    Just (DragEngaged time) ->
      ( model
          |> setDragState (Just <| WaitForEndTime time)
      , Task.perform (TopicMap << TopicMapDef.GotTime) Time.now
      )
    Just (Drag _) ->
      ( env
          |> Env.map (updateTopicPos clientPos)
          |> Env.autoSize
          |> .model
      , Cmd.none
      )
    _ ->
      (model, Cmd.none)


updateTopicPos : Point -> Model -> Model
updateTopicPos clientPos model =
  case (model.mouse.dragSource, model.topicMap.dragState) of
    (Just {topicId, boxPath, lastPointerPos}, Just (Drag dragMode)) ->
      case dragMode of
        DragTopic _ ->
          model
            |> TopicMap.updateTopicPos topicId (Box.firstId boxPath)
              (\pos ->
                Point
                  (pos.x + clientPos.x - lastPointerPos.x)
                  (pos.y + clientPos.y - lastPointerPos.y)
              )
        DraftAssoc ->
          model
    _ ->
      let
        _ = Console.logError "TopicMap.Mouse.updateTopicPos" "Unexpected mouse state"
          (model.mouse.dragSource, model.topicMap.dragState)
      in
      model


-- Dispatch.ExtDropTargeting
updateDropTarget : Point -> Env -> (Model, Maybe Target)
updateDropTarget _ {model} =
  (model, dropTarget model)


{-| Projects Feature.Mouse's general "hover" state to TopicMap specific accepted "dropTarget".
-}
dropTarget : Model -> Maybe Target
dropTarget model =
  case (model.mouse.hover, model.mouse.dragSource) of
    (Just {target}, Just {topicId, boxPath}) ->
      case target of
        (T targetTopicId, targetBoxPath) ->
          let
            -- geometrically hovered topic (targetTopicId) is accepted as a drop target if
            -- it is not contained in item/box being dragged (topicId), would create a cycle
            isCyclic = Box.hadDeepTopic targetTopicId topicId -- partially applied
          in
          case model.topicMap.dragState of
            Just (Drag (DragTopic _)) ->
              let
                -- When dragging a topic inside a nested box that box will be the target (this
                -- is since target is determined by map geometry, not by enter/leave events
                -- anymore). We distinguish a topic-moved-to-box from a topic-dragged-inside-box
                -- by comparing the dragged topic's direct parent box.
                isParent = fromBoxId (Box.firstId boxPath) == targetTopicId
              in
              if not isParent && not (isCyclic model) then
                Just target
              else
                Nothing
            Just (Drag DraftAssoc) ->
              -- only create assoc if both topics are in same box
              if boxPath == targetBoxPath then
                Just target
              else
                Nothing
            Just _ -> Nothing -- TODO: error?
            Nothing ->
              -- foreign drop (from non-TopicMap renderer)
              if not (isCyclic model) then
                Just target
              else
                Nothing
        _ -> Nothing -- TODO: error?
    _ -> Nothing -- TODO: error?


-- Dispatch.ExtDragStop
dragStop : Env -> Outcome
dragStop ({model} as env) =
  let
    noOp = Outcome.with Cmd.none model
    outcome =
      case model.mouse.dragSource of
        Just {topicId, boxPath} ->
          case model.topicMap.dragState of
            Just (Drag (DragTopic origTopicPos)) ->
              topicDragEnd topicId boxPath origTopicPos env
            Just (Drag DraftAssoc) ->
              assocDragEnd topicId boxPath model
            Just (DragEngaged _) ->
              let
                _ = Console.info "TopicMap.Mouse.dragStop" "topic not moved -> select topic"
              in
              noOp
                |> Outcome.map (Sel.select (T topicId) boxPath)
            Nothing ->
              foreignTopicDrop topicId boxPath env
            _ ->
              let
                _ = Console.logError "TopicMap.Mouse.dragStop" "Unexpected drag state"
                  model.mouse.dragSource
              in
              noOp
        Nothing ->
          let
            _ = Console.logError "TopicMap.Mouse.dragStop" "Unexpected drag source"
              model.mouse.dragSource
          in
          noOp
  in
  outcome
    |> Outcome.map (setDragState Nothing)


topicDragEnd : TopicId -> BoxPath -> Point -> Env -> Outcome
topicDragEnd sourceTopicId sourceBoxPath origTopicPos ({model} as env) =
  case model.mouse.dropTarget of
    Just (T targetId, targetPath) ->
      let
        _ = Console.info "TopicMap.Mouse.topicDragEnd" ("dropped "
          ++ fromInt (toTopicId sourceTopicId) ++ " (box " ++ Box.fromPath sourceBoxPath
          ++ ") on " ++ fromInt (toTopicId targetId) ++ " (box " ++ Box.fromPath targetPath
          ++ ") --> move topic to box")
        boxId = Box.firstId sourceBoxPath
      in
      env
        |> moveTopicToBox sourceTopicId boxId targetId targetPath
        |> Model.map (TopicMap.setTopicPos sourceTopicId boxId origTopicPos)
        |> \(model_, cmd) -> Outcome (Directives Store Push) cmd model_
    Nothing ->
      let
        _ = Console.info "TopicMap.Mouse.topicDragEnd"
          "topic drag ended w/o target -> store position"
      in
      Outcome (Directives Store Swap) Cmd.none model
    _ ->
      Outcome.with Cmd.none model


foreignTopicDrop : TopicId -> BoxPath -> Env -> Outcome
foreignTopicDrop sourceTopicId sourceBoxPath ({model} as env) =
  let
    noOp = Outcome.with Cmd.none model
  in
  case model.mouse.dropTarget of
    Just (T targetId, targetPath) ->
      let
        _ = Console.info "TopicMap.Mouse.foreignTopicDrop" ("dropped "
          ++ fromInt (toTopicId sourceTopicId) ++ " (box " ++ Box.fromPath sourceBoxPath
          ++ ") on " ++ fromInt (toTopicId targetId) ++ " (box " ++ Box.fromPath targetPath
          ++ ") --> foreign topic drop")
        boxId = Box.firstId sourceBoxPath
      in
      env
        |> moveTopicToBox sourceTopicId boxId targetId targetPath
        |> \(model_, cmd) -> Outcome (Directives Store Push) cmd model_
    Nothing ->
      let
        _ = Console.info "TopicMap.Mouse.foreignTopicDrop"
          "foreign topic drag ended w/o target -> do nothing"
      in
      noOp
    _ ->
      let
        _ = Console.logError "TopicMap.Mouse.foreignTopicDrop" "Unexpected drop target"
          model.mouse.dropTarget
      in
      noOp


assocDragEnd : TopicId -> BoxPath -> Model -> Outcome
assocDragEnd sourceTopicId sourceBoxPath model =
  case model.mouse.dropTarget of
    Just (T targetId, targetPath) ->
      let
        _ = Console.info "TopicMap.Mouse.assocDragEnd" ("assoc drawn from "
          ++ fromInt (toTopicId sourceTopicId) ++ " (box " ++ Box.fromPath sourceBoxPath
          ++ ") to " ++ fromInt (toTopicId targetId) ++ " (box " ++ Box.fromPath targetPath
          ++ ") --> create assoc")
        boxId = Box.firstId sourceBoxPath
      in
      Outcome
        (Directives Store Push)
        Cmd.none
        (model |> createAssoc sourceTopicId targetId boxId)
    Nothing ->
      let
        _ = Console.info "TopicMap.Mouse.assocDragEnd" "drawn assoc ended w/o target"
      in
      Outcome.with Cmd.none model
    _ ->
      Outcome.with Cmd.none model


moveTopicToBox : TopicId -> BoxId -> TopicId -> BoxPath -> Env -> (Model, Cmd Msg)
moveTopicToBox topicId boxId targetTopicId targetPath ({model, dispatch} as env) =
  let
    targetBoxId = BoxId targetTopicId -- after turnTopicIntoBox target topic is a box for sure
    expansion = Box.expansionOf topicId boxId model
    maybeRenderer = Extension.fromString "TopicMap"
  in
  case maybeRenderer of
    Just renderer ->
      env
        |> Box.turnTopicIntoBox targetTopicId renderer
        |> Box.addTopic (BoxTopic topicId expansion) targetBoxId
        |> .model
        |> Box.removeTopic topicId boxId
        |> Sel.select (T targetTopicId) targetPath
        |> TopicMap.randomPos topicId targetBoxId
    Nothing ->
      (model, Cmd.none)


-- Presumption: both topics exist in same box
createAssoc : TopicId -> TopicId -> BoxId -> Model -> Model
createAssoc topicId1 topicId2 boxId model =
  model
    |> Assoc.create Association topicId1 topicId2
    |> \(model_, assocId) ->
      model_
        |> Box.addAssoc assocId boxId


setDragState : Maybe DragState -> Model -> Model
setDragState dragState ({topicMap} as model) =
  { model | topicMap = { topicMap | dragState = dragState }}


isDraftAssoc : Model -> Bool
isDraftAssoc model =
  case model.topicMap.dragState of
    Just (Drag DraftAssoc) -> True
    _ -> False
