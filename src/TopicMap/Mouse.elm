module TopicMap.Mouse exposing (dragStart, drag, dragStop, timeArrived)

import Box
import Config as C
import Env exposing (Env2)
import Feature.MouseDef as MouseDef
import Model exposing (Model, Msg(..))
import ModelBase exposing (..)
import Outcome exposing (Outcome)
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
    MouseDef.DragStarted topicId boxPath _ _ ->
      ( model
          |> setMouseState WaitForStartTime
      , Cmd.batch
          [ U.command (Cancel (Just (T topicId, boxPath)))
          , Task.perform (TopicMap << TopicMapDef.Time) Time.now
          ]
      )
    _ ->
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
    (WaitForEndTime startTime, MouseDef.DragStarted id (boxId :: _) _ pos) ->
      let
        delay = posixToMillis time - posixToMillis startTime
        (dragMode, undo) =
          case delay > C.assocDelayMillis of
            True -> (DraftAssoc, Undo.swap)
            False -> (DragTopic, Undo.push)
        maybeOrigPos = TM.topicPos id boxId present
        mouseState =
          case maybeOrigPos of
            Just origPos -> Drag dragMode (DragState origPos pos Nothing)
            Nothing -> NoDrag -- error is already logged
      in
      (setMouseState mouseState present, Cmd.none)
        |> undo undoModel
    _ ->
      U.logError "TopicMap.Mouse.timeArrived"
        "Received Time when mouseState is not WaitFor..Time" (undoModel, Cmd.none)


-- ExtManager.NestingDrag
drag : Point -> Env2 -> (Model, Cmd Msg)
drag pos ({model} as env) =
  let
    newModel = updateTarget model -- TODO: pipe model, having dropTargetAt, like TopicList
    newEnv = Env.withModel2 env newModel
  in
  case newModel.topicMap.mouseState of
    DragEngaged time ->
      ( setMouseState (WaitForEndTime time) newModel
      , Task.perform (TopicMap << TopicMapDef.Time) Time.now
      )
    Drag _ _ ->
      ( performDrag pos newEnv, Cmd.none )
    _ ->
      ( newModel, Cmd.none )


{-| If Drag is in progress updates its accepted drop target, based on geometrically hovered
topic (Feature.Mouse module's "hover" state).
-}
updateTarget : Model -> Model
updateTarget model =
  case (model.mouse.dragState, model.topicMap.mouseState) of
    (MouseDef.DragStarted dragTopicId _ _ _, Drag dragMode dragState) ->
      case model.mouse.hover of
        Just ((T dropTopicId, _) as target) ->
          let
            isCyclic = Box.hadDeepTopic dropTopicId dragTopicId model
            newTarget =
              -- geometrically hovered topic (dropTopicId) is accepted as a drop target if it is
              -- 1. not contained in item/box being dragged (dragTopicId), would create a cycle
              -- 2. OR draft assoc is in progress
              if not isCyclic || dragMode == DraftAssoc then
                Just target
              else
                Nothing
          in
          model -- update accepted drop target
            |> setMouseState (Drag dragMode {dragState | dropTarget = newTarget})
        Nothing ->
          model -- reset accepted drop target
            |> setMouseState (Drag dragMode {dragState | dropTarget = Nothing})
        _ -> model
    _ -> model


performDrag : Point -> Env2 -> Model
performDrag pos ({model} as env) =
  case (model.mouse.dragState, model.topicMap.mouseState) of
    (MouseDef.DragStarted topicId (boxId :: _) _ _, Drag dragMode ({lastPointerPos} as dragState)) ->
      let
        newModel =
          case dragMode of
            DragTopic -> TM.updateTopicPos topicId boxId
              (\oldPos ->
                Point
                  (oldPos.x + pos.x - lastPointerPos.x)
                  (oldPos.y + pos.y - lastPointerPos.y)
              )
              model
            DraftAssoc -> model
      in
      newModel
        |> setMouseState (Drag dragMode {dragState | lastPointerPos = pos})
        |> Env.autoSize2 env
    _ -> U.logError "TopicMap.Mouse.performDrag"
      ("Received \"Drag\" when mouseState is " ++ U.toString model.topicMap.mouseState) model


-- ExtManager.NestingDragStop
dragStop : Env2 -> Outcome
dragStop {model} =
  let
    cmd =
      case (model.topicMap.mouseState, model.mouse.dragState) of
        (Drag DragTopic {origTopicPos, dropTarget}, MouseDef.DragStarted id boxPath _ _) ->
          case dropTarget of
            Just (T targetId, targetPath) ->
              let
                _ = U.info "TopicMap.Mouse.dragStop" ("dropped " ++ fromInt (toTopicId id)
                  ++ " (box " ++ Box.fromPath boxPath ++ ") on " ++ fromInt (toTopicId targetId)
                  ++ " (box " ++ Box.fromPath targetPath ++ ") --> "
                  ++ if shouldMoveToBox then "move topic to box" else "abort")
                (BoxId topicId as boxId) = Box.firstId boxPath
                -- When dragging a topic inside a nested box that box will be the target (this
                -- is since target is determined by map geometry, not by enter/leave events
                -- anymore). We distinguish a topic-moved-to-box from a topic-dragged-inside-box
                -- by comparing the dragged topic's parent box.
                shouldMoveToBox = topicId /= targetId
              in
              case shouldMoveToBox of
                True -> U.command (TopicDropped id boxId origTopicPos targetId targetPath)
                False -> U.command TopicDragged -- store topic pos
            Nothing ->
              let
                _ = U.info "TopicMap.Mouse.dragStop" "topic drag ended w/o target"
              in
              U.command TopicDragged
            _ ->
              Cmd.none
        (Drag DraftAssoc {dropTarget}, MouseDef.DragStarted id boxPath _ _) ->
          case dropTarget of
            Just (T targetId, targetPath) ->
              let
                _ = U.info "TopicMap.Mouse.dragStop" ("assoc drawn from "
                  ++ fromInt (toTopicId id) ++ " (box " ++ Box.fromPath boxPath ++ ") to "
                  ++ fromInt (toTopicId targetId) ++ " (box " ++ Box.fromPath targetPath
                  ++ ") --> " ++ if isSameBox then "create assoc" else "abort")
                boxId = Box.firstId boxPath
                isSameBox = boxId == Box.firstId targetPath
              in
              case isSameBox of
                True -> U.command <| CreateAssoc id targetId boxId
                False -> Cmd.none
            Nothing ->
              let
                _ = U.info "TopicMap.Mouse.dragStop" "assoc ended w/o target"
              in
              Cmd.none
            _ ->
              Cmd.none
        (DragEngaged _, MouseDef.DragStarted id boxPath _ _) ->
          let
            _ = U.info "TopicMap.Mouse.dragStop" "topic not moved -> ItemClicked"
          in
          U.command <| ItemClicked (T id) boxPath
        _ ->
          Cmd.none
  in
  Outcome.with cmd model
    |> Outcome.map (setMouseState NoDrag)


setMouseState : MouseState -> Model -> Model
setMouseState mouseState ({topicMap} as model) =
  { model | topicMap = { topicMap | mouseState = mouseState }}
