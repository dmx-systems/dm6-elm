module TopicMap.Mouse exposing (dragStart, drag, dragStop, timeArrived,
  isDragInProgress, clearHover)

import Box
import Config as C
import Env exposing (Env2)
import Feature.Mouse as Mouse
import Model exposing (Model, Msg(..))
import ModelBase exposing (..)
import TopicMap.TopicMapDef as TopicMapDef exposing (DragState(..), DragMode(..))
import TopicMap.BoxProps as TM
import Undo exposing (UndoModel)
import Utils as U

import String exposing (fromInt)
import Task
import Time exposing (Posix, posixToMillis)



-- UPDATE


dragStart : TopicId -> BoxPath -> Point -> PointerType -> Env2 -> (Model, Cmd Msg)
dragStart topicId boxPath pos pointerType {model} =
  ( model
      |> emulateHover topicId boxPath pointerType
      |> setDragState (WaitForStartTime topicId boxPath pos)
  , Cmd.batch
      [ U.command (Cancel (Just (T topicId, boxPath)))
      , Task.perform (TopicMap << TopicMapDef.Time) Time.now
      ]
  )


emulateHover : TopicId -> BoxPath -> PointerType -> Model -> Model
emulateHover topicId boxPath pointerType model =
  if pointerType == "touch" then
    model
      |> Mouse.setHover (Just (T topicId, boxPath))
  else
    model


timeArrived : Posix -> UndoModel -> (UndoModel, Cmd Msg)
timeArrived time ({present} as undoModel) =
  case present.topicMap.dragState of
    WaitForStartTime id boxPath pos ->
      let
        dragState = DragEngaged time id boxPath pos
      in
      (setDragState dragState present, Cmd.none) |> Undo.swap undoModel
    WaitForEndTime startTime id boxPath pos ->
      let
        delay = posixToMillis time - posixToMillis startTime
        (dragMode, undo) =
          case delay > C.assocDelayMillis of
            True -> (DraftAssoc, Undo.swap)
            False -> (DragTopic, Undo.push)
        maybeOrigPos = TM.topicPos id (Box.firstId boxPath) present
        dragState =
          case maybeOrigPos of
            Just origPos -> Drag dragMode id boxPath origPos pos Nothing
            Nothing -> NoDrag -- error is already logged
      in
      (setDragState dragState present, Cmd.none) |> undo undoModel
    _ ->
      U.logError "TopicMap.Mouse.timeArrived"
        "Received Time when dragState is not WaitFor..Time" (undoModel, Cmd.none)


drag : Point -> Env2 -> (Model, Cmd Msg)
drag pos env =
  let
    model = updateTarget pos env
    newEnv = Env.withModel2 env model
  in
  case model.topicMap.dragState of
    DragEngaged time id boxPath pos_ ->
      ( setDragState (WaitForEndTime time id boxPath pos_) model
      , Task.perform (TopicMap << TopicMapDef.Time) Time.now
      )
    Drag _ _ _ _ _ _ ->
      ( performDrag pos newEnv, Cmd.none )
    _ ->
      ( model, Cmd.none )


{-| If Drag is in progress updates its "target" based on Mouse "hover" state.
-}
updateTarget : Point -> Env2 -> Model
updateTarget pos {model, ext} =
  case model.topicMap.dragState of
    Drag dragMode id boxPath origPos lastPos _ ->
      let
        dragState = Drag dragMode id boxPath origPos lastPos
      in
      case model.mouse.hover of
        Just ((T topicId, _) as target) ->
          let
            isCyclic = Box.hadDeepTopic topicId id model
            newTarget =
              -- the hovered item (targetId) is accepted as a drop target if it is
              -- 1. not contained in item/box being dragged (id), this would create a cycle
              -- 2. OR draft assoc is in progress
              if not isCyclic || dragMode == DraftAssoc then
                Just target
              else
                Nothing
          in
          model -- update target
            |> setDragState (dragState newTarget)
        Nothing ->
          model -- reset target
            |> setDragState (dragState Nothing)
        _ -> model
    _ -> model


performDrag : Point -> Env2 -> Model
performDrag pos ({model} as env) =
  case model.topicMap.dragState of
    Drag dragMode id boxPath origPos lastPos target ->
      let
        boxId = Box.firstId boxPath
        newModel =
          case dragMode of
            DragTopic -> TM.updateTopicPos id boxId
              (\oldPos ->
                Point
                  (oldPos.x + pos.x - lastPos.x)
                  (oldPos.y + pos.y - lastPos.y)
              )
              model
            DraftAssoc -> model
      in
      -- update lastPos
      setDragState (Drag dragMode id boxPath origPos pos target) newModel
        |> Env.autoSize2 env
    _ -> U.logError "TopicMap.Mouse.performDrag"
      ("Received \"Drag\" when dragState is " ++ U.toString model.topicMap.dragState) model


dragStop : Env2 -> (Model, Cmd Msg)
dragStop {model} =
  let
    cmd =
      case model.topicMap.dragState of
        Drag DragTopic id boxPath origPos _ (Just (T targetId, targetPath)) ->
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
        Drag DragTopic _ _ _ _ _ ->
          let
            _ = U.info "TopicMap.Mouse.dragStop" "topic drag ended w/o target"
          in
          U.command TopicDragged
        Drag DraftAssoc id boxPath _ _ (Just (T targetId, targetPath)) ->
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
        Drag DraftAssoc _ _ _ _ _ ->
          let
            _ = U.info "TopicMap.Mouse.dragStop" "assoc ended w/o target"
          in
          Cmd.none
        DragEngaged _ id boxPath _ ->
          let
            _ = U.info "TopicMap.Mouse.dragStop" "item not moved -> ItemClicked"
          in
          U.command <| ItemClicked (T id) boxPath
        _ ->
          Cmd.none
  in
  (setDragState NoDrag model, cmd)


setDragState : DragState -> Model -> Model
setDragState dragState ({topicMap} as model) =
  { model | topicMap = { topicMap | dragState = dragState }}


clearHover : Model -> Model
clearHover model =
  model
    |> Mouse.setHover Nothing


isDragInProgress : Model -> Bool
isDragInProgress model =
  case model.topicMap.dragState of
    Drag _ _ _ _ _ _ -> True
    _ -> False
