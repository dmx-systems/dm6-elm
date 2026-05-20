module TopicMap.Mouse exposing (dragStart, drag, dragStop, timeArrived)

import Box
import Config as C
import Env exposing (Env2)
import Feature.Mouse as Mouse
import Feature.MouseDef as MouseDef
import Model exposing (Model, Msg(..))
import ModelBase exposing (..)
import TopicMap.BoxProps as TM
import TopicMap.TopicMapDef as TopicMapDef exposing (DragState(..), DragMode(..))
import Undo exposing (UndoModel)
import Utils as U

import String exposing (fromInt)
import Task
import Time exposing (Posix, posixToMillis)



-- UPDATE


-- ExtManager.NestingDragStart
dragStart : TopicId -> BoxPath -> BoxPath -> Point -> PointerType -> Env2 -> (Model, Cmd Msg)
dragStart topicId boxPath ixBoxPath pos pointerType {model} =
  ( model
      |> setDragState WaitForStartTime
  , Cmd.batch
      [ U.command (Cancel (Just (T topicId, boxPath)))
      , Task.perform (TopicMap << TopicMapDef.Time) Time.now
      ]
  )


timeArrived : Posix -> UndoModel -> (UndoModel, Cmd Msg)
timeArrived time ({present} as undoModel) =
  case (present.topicMap.dragState, present.mouse.dragState) of
    (WaitForStartTime, _) ->
      let
        dragState = DragEngaged time
      in
      (setDragState dragState present, Cmd.none)
        |> Undo.swap undoModel
    (WaitForEndTime startTime, MouseDef.DragStarted id (boxId :: _) _ pos) ->
      let
        delay = posixToMillis time - posixToMillis startTime
        (dragMode, undo) =
          case delay > C.assocDelayMillis of
            True -> (DraftAssoc, Undo.swap)
            False -> (DragTopic, Undo.push)
        maybeOrigPos = TM.topicPos id boxId present
        dragState =
          case maybeOrigPos of
            Just origPos -> Drag dragMode origPos pos Nothing
            Nothing -> NoDrag -- error is already logged
      in
      (setDragState dragState present, Cmd.none)
        |> undo undoModel
    _ ->
      U.logError "TopicMap.Mouse.timeArrived"
        "Received Time when dragState is not WaitFor..Time" (undoModel, Cmd.none)


-- ExtManager.NestingDrag
drag : Point -> Env2 -> (Model, Cmd Msg)
drag pos env =
  let
    model = updateTarget pos env
    newEnv = Env.withModel2 env model
  in
  case model.topicMap.dragState of
    DragEngaged time ->
      ( setDragState (WaitForEndTime time) model
      , Task.perform (TopicMap << TopicMapDef.Time) Time.now
      )
    Drag _ _ _ _ ->
      ( performDrag pos newEnv, Cmd.none )
    _ ->
      ( model, Cmd.none )


{-| If Drag is in progress updates its accepted drop target, based on geometrically hovered
topic (Feature.Mouse module's "hover" state).
-}
updateTarget : Point -> Env2 -> Model
updateTarget pos {model, ext} =
  case (model.mouse.dragState, model.topicMap.dragState) of
    (MouseDef.DragStarted id _ _ _, Drag dragMode origPos lastPos _) ->
      let
        dragState = Drag dragMode origPos lastPos
      in
      case model.mouse.hover of
        Just ((T topicId, _) as target) ->
          let
            isCyclic = Box.hadDeepTopic topicId id model
            newTarget =
              -- the geometrically hovered topic (topicId) is accepted as a drop target if it is
              -- 1. not contained in item/box being dragged (id), this would create a cycle
              -- 2. OR draft assoc is in progress
              if not isCyclic || dragMode == DraftAssoc then
                Just target
              else
                Nothing
          in
          model -- update accepted drop target
            |> setDragState (dragState newTarget)
        Nothing ->
          model -- reset accepted drop target
            |> setDragState (dragState Nothing)
        _ -> model
    _ -> model


performDrag : Point -> Env2 -> Model
performDrag pos ({model} as env) =
  case (model.mouse.dragState, model.topicMap.dragState) of
    (MouseDef.DragStarted topicId (boxId :: _) _ _, Drag dragMode origPos lastPos target) ->
      let
        newModel =
          case dragMode of
            DragTopic -> TM.updateTopicPos topicId boxId
              (\oldPos ->
                Point
                  (oldPos.x + pos.x - lastPos.x)
                  (oldPos.y + pos.y - lastPos.y)
              )
              model
            DraftAssoc -> model
      in
      -- update lastPos
      newModel
        |> setDragState (Drag dragMode origPos pos target)
        |> Env.autoSize2 env
    _ -> U.logError "TopicMap.Mouse.performDrag"
      ("Received \"Drag\" when dragState is " ++ U.toString model.topicMap.dragState) model


-- ExtManager.NestingDragStop
dragStop : Env2 -> (Model, Cmd Msg)
dragStop {model} =
  let
    cmd =
      case (model.topicMap.dragState, model.mouse.dragState) of
        ( Drag DragTopic origPos _ (Just (T targetId, targetPath))
          , MouseDef.DragStarted id boxPath _ _) ->
          let
            _ = U.info "TopicMap.Mouse.dragStop" ("dropped " ++ fromInt (toTopicId id)
              ++ " (box " ++ Box.fromPath boxPath ++ ") on " ++ fromInt (toTopicId targetId)
              ++ " (box " ++ Box.fromPath targetPath ++ ") --> "
              ++ if shouldMoveToBox then "move topic to box" else "abort")
            (BoxId topicId as boxId) = Box.firstId boxPath
            -- When dragging a topic inside a nested box that box will be the target (this is
            -- since target is determined by map geometry, not by enter/leave events anymore).
            -- We distinguish a topic-moved-to-box from a topic-dragged-inside-box by comparing
            -- the dragged topic's parent box.
            shouldMoveToBox = topicId /= targetId
          in
          case shouldMoveToBox of
            True -> U.command <| TopicDropped id boxId origPos targetId targetPath
            False -> U.command TopicDragged -- store topic pos
        (Drag DragTopic _ _ _, _) ->
          let
            _ = U.info "TopicMap.Mouse.dragStop" "topic drag ended w/o target"
          in
          U.command TopicDragged
        (Drag DraftAssoc _ _ (Just (T targetId, targetPath))
          , MouseDef.DragStarted id boxPath _ _) ->
          let
            _ = U.info "TopicMap.Mouse.dragStop" ("assoc drawn from " ++ fromInt (toTopicId id)
              ++ " (box " ++ Box.fromPath boxPath ++ ") to " ++ fromInt (toTopicId targetId)
              ++ " (box " ++ Box.fromPath targetPath ++ ") --> "
              ++ if isSameBox then "create assoc" else "abort")
            boxId = Box.firstId boxPath
            isSameBox = boxId == Box.firstId targetPath
          in
          case isSameBox of
            True -> U.command <| CreateAssoc id targetId boxId
            False -> Cmd.none
        (Drag DraftAssoc _ _ _, _) ->
          let
            _ = U.info "TopicMap.Mouse.dragStop" "assoc ended w/o target"
          in
          Cmd.none
        (DragEngaged _, MouseDef.DragStarted id boxPath _ _) ->
          let
            _ = U.info "TopicMap.Mouse.dragStop" "topic not moved -> ItemClicked"
          in
          U.command <| ItemClicked (T id) boxPath
        _ ->
          Cmd.none
  in
  (setDragState NoDrag model, cmd)


setDragState : DragState -> Model -> Model
setDragState dragState ({topicMap} as model) =
  { model | topicMap = { topicMap | dragState = dragState }}
