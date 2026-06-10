module TopicMap.Mouse exposing (dragStart, drag, dragTargeting, resetDropTarget, dragStop,
  timeArrived)

import Assoc
import Box
import Config as C
import Env exposing (Env2)
import Feature.Sel as Sel
import Feature.Tool as Tool
import Model exposing (Model, Msg(..))
import ModelBase exposing (..)
import Outcome exposing (..)
import TopicMap.BoxProps as TM
import TopicMap.TopicMapDef as TopicMapDef exposing (MouseState(..), DragState, DragMode(..))
import Undo exposing (UndoModel)
import Utils as U

import String exposing (fromInt)
import Task
import Time exposing (Posix, posixToMillis)



-- UPDATE


-- ExtManager.NestingDragStart
dragStart : Env2 -> (Model, Cmd Msg)
dragStart {model} =
  case model.mouse.dragState of
    Just {topicId, boxPath} ->
      ( model
          |> setMouseState WaitForStartTime
      , Cmd.batch
          [ U.command (Cancel (Just (T topicId, boxPath)))
          , Task.perform (TopicMap << TopicMapDef.GotTime) Time.now
          ]
      )
    Nothing ->
      let
        _ = U.logError "TopicMap.Mouse.dragStart" "Unexpected drag state" model.mouse.dragState
      in
      (model, Cmd.none)


timeArrived : Posix -> UndoModel -> (UndoModel, Cmd Msg)
timeArrived time ({present} as undoModel) =
  case (present.topicMap.mouseState, present.mouse.dragState) of
    (WaitForStartTime, _) ->
      let
        mouseState = DragEngaged time
      in
      (setMouseState mouseState present, Cmd.none)
        |> Undo.swap undoModel
    (WaitForEndTime startTime, Just {topicId, boxPath}) ->
      let
        delay = posixToMillis time - posixToMillis startTime
        (dragMode, undo) =
          case delay > C.assocDelayMillis of
            True -> (DraftAssoc, Undo.swap)
            False -> (DragTopic, Undo.push)
        maybeOrigPos = TM.topicPos topicId (Box.firstId boxPath) present
        mouseState =
          case maybeOrigPos of
            Just origPos -> Drag dragMode (DragState origPos Nothing)
            Nothing -> NoDrag -- error is already logged
      in
      (setMouseState mouseState present, Cmd.none)
        |> undo undoModel
    _ ->
      U.logError "TopicMap.Mouse.timeArrived"
        "Received Time when mouseState is not WaitFor..Time" (undoModel, Cmd.none)


-- ExtManager.NestingDrag
drag : Point -> Env2 -> (Model, Cmd Msg)
drag clientPos ({model} as env) =
  case model.topicMap.mouseState of
    DragEngaged time ->
      ( setMouseState (WaitForEndTime time) model
      , Task.perform (TopicMap << TopicMapDef.GotTime) Time.now
      )
    Drag _ _ ->
      (updateTopicPos clientPos env, Cmd.none)
    _ ->
      (model, Cmd.none)


-- ExtManager.DragTargeting
dragTargeting : Point -> Env2 -> Model
dragTargeting _ {model} =
  case model.topicMap.mouseState of
    Drag dragMode dragState ->
      let
        target = dropTargetFor dragMode model
      in
      model
        |> setMouseState (Drag dragMode {dragState | dropTarget = target})
    _ -> model -- TODO: error?


updateTopicPos : Point -> Env2 -> Model
updateTopicPos pos ({model} as env) =
  case (model.topicMap.mouseState, model.mouse.dragState) of
    (Drag dragMode _, Just {topicId, boxPath, lastPointerPos}) ->
      let
        newModel =
          case dragMode of
            DragTopic -> TM.updateTopicPos topicId (Box.firstId boxPath)
              (\oldPos ->
                Point
                  (oldPos.x + pos.x - lastPointerPos.x)
                  (oldPos.y + pos.y - lastPointerPos.y)
              )
              model
            DraftAssoc -> model
      in
      newModel
        |> Env.autoSize2 env
    _ -> U.logError "TopicMap.Mouse.updateTopicPos"
      ("Received \"Drag\" when mouseState is " ++ U.toString model.topicMap.mouseState) model


{-| If Drag is in progress updates its accepted drop target, based on geometrically hovered
topic (Feature.Mouse module's "hover" state). ### FIXDOC
-}
dropTargetFor : DragMode -> Model -> Maybe Target
dropTargetFor dragMode model =
  case (model.mouse.hover, model.mouse.dragState) of
    (Just {target}, Just {topicId}) ->
      case target of
        (T dropTopicId, _) ->
          let
            isCyclic = Box.hadDeepTopic dropTopicId topicId model
          in
          -- geometrically hovered topic (dropTopicId) is accepted as a drop target if it is
          -- 1. not contained in item/box being dragged (topicId), would create a cycle
          -- 2. OR draft assoc is in progress
          if not isCyclic || dragMode == DraftAssoc then
            Just target
          else
            Nothing
        _ -> Nothing -- TODO: error?
    _ -> Nothing -- TODO: error?


-- ExtManager.DropTargetReset
resetDropTarget : Env2 -> Model
resetDropTarget ({model} as env2) =
  model -- TODO


-- ExtManager.NestingDragStop
dragStop : Env2 -> Outcome
dragStop ({model} as env) =
  let
    out =
      case (model.topicMap.mouseState, model.mouse.dragState) of
        (Drag DragTopic {origTopicPos, dropTarget}, Just {topicId, boxPath}) ->
          case dropTarget of
            Just (T targetId, targetPath) ->
              let
                _ = U.info "TopicMap.Mouse.dragStop" ("dropped " ++ fromInt (toTopicId topicId)
                  ++ " (box " ++ Box.fromPath boxPath ++ ") on " ++ fromInt (toTopicId targetId)
                  ++ " (box " ++ Box.fromPath targetPath ++ ") --> "
                  ++ if shouldMoveToBox then "move topic to box" else "abort, store position")
                -- When dragging a topic inside a nested box that box will be the target (this
                -- is since target is determined by map geometry, not by enter/leave events
                -- anymore). We distinguish a topic-moved-to-box from a topic-dragged-inside-box
                -- by comparing the dragged topic's parent box.
                boxId = Box.firstId boxPath
                shouldMoveToBox = fromBoxId boxId /= targetId
              in
              if shouldMoveToBox then
                env
                  |> moveTopicToBox topicId boxId origTopicPos targetId targetPath
                  |> \(model_, cmd) -> Outcome (Directives Store Push) cmd model_
              else
                Outcome (Directives Store Swap) Cmd.none model -- store topic pos
            Nothing ->
              let
                _ = U.info "TopicMap.Mouse.dragStop"
                  "topic drag ended w/o target -> store position"
              in
              Outcome (Directives Store Swap) Cmd.none model
            _ ->
              Outcome.with Cmd.none model
        (Drag DraftAssoc {dropTarget}, Just {topicId, boxPath}) ->
          case dropTarget of
            Just (T targetId, targetPath) ->
              let
                _ = U.info "TopicMap.Mouse.dragStop" ("assoc drawn from "
                  ++ fromInt (toTopicId topicId) ++ " (box " ++ Box.fromPath boxPath ++ ") to "
                  ++ fromInt (toTopicId targetId) ++ " (box " ++ Box.fromPath targetPath
                  ++ ") --> " ++ if isSameBox then "create assoc" else "abort")
                boxId = Box.firstId boxPath
                isSameBox = boxId == Box.firstId targetPath
              in
              case isSameBox of
                True ->
                  Outcome
                    (Directives Store Push)
                    Cmd.none
                    (model |> createAssoc topicId targetId boxId)
                False -> Outcome.with Cmd.none model
            Nothing ->
              let
                _ = U.info "TopicMap.Mouse.dragStop" "assoc ended w/o target"
              in
              Outcome.with Cmd.none model
            _ ->
              Outcome.with Cmd.none model
        (DragEngaged _, Just {topicId, boxPath}) ->
          let
            _ = U.info "TopicMap.Mouse.dragStop" "topic not moved -> select topic"
          in
          Outcome.with Cmd.none <| Sel.select (T topicId) boxPath model
        _ ->
          Outcome.with Cmd.none model
  in
  out
    |> Outcome.map (setMouseState NoDrag)


moveTopicToBox : TopicId -> BoxId -> Point -> TopicId -> BoxPath -> Env2 -> (Model, Cmd Msg)
moveTopicToBox topicId boxId origPos targetTopicId targetPath {model, ext} =
  let
    targetBoxId = BoxId targetTopicId -- after createBoxOnDemand target topic is a box for sure
    expansion = Box.expansionOf topicId boxId model
  in
  model
    |> Tool.createBoxOnDemand targetTopicId
    |> Box.addTopic (BoxTopic topicId expansion) targetBoxId
    |> Box.removeTopic topicId boxId
    |> Sel.select (T targetTopicId) targetPath
    |> TM.setTopicPos topicId boxId origPos
    |> ext.addTopic topicId targetBoxId Random -- TODO: revise extension point -> use init?
    -- Calling Env.autoSize is the responsibility of the extension's addTopic implementation.
    -- Particular extensions might add the topic asynchronously (TopicMap extension does) so
    -- their BoxProps might not yet be initialized but are needed for auto-sizing. 


-- Presumption: both topics exist in same box
createAssoc : TopicId -> TopicId -> BoxId -> Model -> Model
createAssoc topicId1 topicId2 boxId model =
  model
    |> Assoc.create Association topicId1 topicId2
    |> \(model_, assocId) ->
      model_
        |> Box.addAssoc assocId boxId


setMouseState : MouseState -> Model -> Model
setMouseState mouseState ({topicMap} as model) =
  { model | topicMap = { topicMap | mouseState = mouseState }}
