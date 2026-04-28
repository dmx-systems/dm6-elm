module Feature.Mouse exposing (topicDownHandler, itemClickHandler, dragHandler,
  isDragInProgress, isHovered, clearHover, update)

import Box
import Config as C
import Env exposing (Env)
import Feature.MouseDef as MouseDef exposing (DragState(..), DragMode(..))
import Model exposing (Model, Msg(..))
import ModelBase exposing (..)
import TopicMap.TopicMap as TM
import Undo exposing (UndoModel)
import Utils as U

import Html.Events exposing (on)
import Json.Decode as D
import String exposing (fromInt)
import Task
import Time exposing (Posix, posixToMillis)



-- VIEW


topicDownHandler : TopicId -> BoxPath -> Attrs Msg
topicDownHandler topicId boxPath =
  [ U.stopPropagationWith "pointerdown"
      ( U.pointDecoder |> D.map
          (Mouse << MouseDef.DownOnTopic topicId boxPath)
      )
  ]


itemClickHandler : ItemId -> BoxPath -> Attrs Msg
itemClickHandler itemId boxPath =
  [ U.onClickStop <| ItemClicked itemId boxPath ]


dragHandler : Attrs Msg
dragHandler =
  [ on "pointerdown" <| D.succeed <| Mouse MouseDef.Down
  , on "pointermove" <| D.map (Mouse << MouseDef.Move) U.pointDecoder
  , on "pointerup" <| D.succeed <| Mouse MouseDef.Up
  ]



-- UPDATE


update : MouseDef.Msg -> Env -> (UndoModel, Cmd Msg)
update msg ({model, undoModel} as env) =
  case msg of
    MouseDef.Down -> (undoModel, mouseDown)
    -- Topic
    MouseDef.DownOnTopic topicId boxPath (pos, pointerType) ->
      mouseDownOnTopic topicId boxPath pos pointerType model |> Undo.swap undoModel
    MouseDef.Move (pos, _) -> mouseMove pos env |> Undo.swap undoModel
    MouseDef.Up -> mouseUp model |> Undo.swap undoModel
    MouseDef.Time time -> timeArrived time undoModel


mouseDown : Cmd Msg
mouseDown =
  U.command <| Cancel Nothing


mouseDownOnTopic : TopicId -> BoxPath -> Point -> PointerType -> Model -> (Model, Cmd Msg)
mouseDownOnTopic topicId boxPath pos pointerType model =
  ( model
      |> emulateHover topicId boxPath pointerType
      |> setDragState (WaitForStartTime topicId boxPath pos)
  , Cmd.batch
      [ U.command <| Cancel <| Just (T topicId, boxPath)
      , Task.perform (Mouse << MouseDef.Time) Time.now
      ]
  )


emulateHover : TopicId -> BoxPath -> PointerType -> Model -> Model
emulateHover topicId boxPath pointerType model =
  case pointerType == "touch" of
    True -> model |> setHover (Just (T topicId, boxPath))
    False -> model


timeArrived : Posix -> UndoModel -> (UndoModel, Cmd Msg)
timeArrived time ({present} as undoModel) =
  case present.mouse.dragState of
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
      U.logError "Feature.Mouse.timeArrived" "Received Time when dragState is not WaitFor..Time"
        (undoModel, Cmd.none)


mouseMove : Point -> Env -> (Model, Cmd Msg)
mouseMove pos env =
  let
    model = enterLeave pos env
    newEnv = Env.withModel env model
  in
  case model.mouse.dragState of
    DragEngaged time id boxPath pos_ ->
      ( setDragState (WaitForEndTime time id boxPath pos_) model
      , Task.perform (Mouse << MouseDef.Time) Time.now
      )
    Drag _ _ _ _ _ _ ->
      ( performDrag pos newEnv, Cmd.none )
    _ ->
      ( model, Cmd.none )


performDrag : Point -> Env -> Model
performDrag pos ({model} as env) =
  case model.mouse.dragState of
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
        |> Env.autoSize env
    _ -> U.logError "Feature.Mouse.performDrag"
      ("Received \"Move\" when dragState is " ++ U.toString model.mouse.dragState) model


mouseUp : Model -> (Model, Cmd Msg)
mouseUp model =
  let
    cmd =
      case model.mouse.dragState of
        Drag DragTopic id boxPath origPos _ (Just (T (TopicId targetId), targetPath)) ->
          let
            _ = U.info "Feature.Mouse.mouseUp" ("dropped " ++ fromInt (toTopicId id) ++ " (box "
              ++ Box.fromPath boxPath ++ ") on " ++ fromInt targetId ++ " (box "
              ++ Box.fromPath targetPath ++ ") --> "
              ++ if shouldMoveToBox then "move topic to box" else "abort")
            boxId = Box.firstId boxPath
            -- When dragging a topic inside a nested box that box will be the target (this is
            -- since target is determined by map geometry, not by enter/leave events anymore).
            -- We distinguish a topic-moved-to-box from a topic-dragged-inside-box by comparing
            -- the dragged topic's parent box.
            shouldMoveToBox = boxId /= targetId
          in
          case shouldMoveToBox of
            True -> U.command <| MoveTopicToBox id boxId origPos targetId targetPath
            False -> U.command TopicDragged -- store topic pos
        Drag DragTopic _ _ _ _ _ ->
          let
            _ = U.info "Feature.Mouse.mouseUp" "topic drag ended w/o target"
          in
          U.command TopicDragged
        Drag DraftAssoc id boxPath _ _ (Just (T targetId, targetPath)) ->
          let
            _ = U.info "Feature.Mouse.mouseUp" ("assoc drawn from " ++ fromInt (toTopicId id)
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
            _ = U.info "Feature.Mouse.mouseUp" "assoc ended w/o target"
          in
          Cmd.none
        DragEngaged _ id boxPath _ ->
          let
            _ = U.info "Feature.Mouse.mouseUp" "item not moved -> ItemClicked"
          in
          U.command <| ItemClicked (T id) boxPath
        _ ->
          Cmd.none
  in
  (setDragState NoDrag model, cmd)


{- Emulates enter/leave events by the means of geometry. Based on the given pointer
coordinate decides whether to call the "enter" and/or "leave" handlers. -}
enterLeave : Point -> Env -> Model
enterLeave pos {model, ext} =
  let
    initPos =
      Point
        (pos.x)
        (pos.y - C.appHeaderHeight)
    excludeTopicId =
      case model.mouse.dragState of
        Drag DragTopic topicId _ _ _ _ -> Just topicId
        _ -> Nothing
  in
  case ext.hitTest model.boxId [] initPos excludeTopicId model of
    Just target ->
      case model.mouse.hover of
        Just oldTarget ->
          case target /= oldTarget of
            True ->
              model
                |> leave oldTarget
                |> enter target
            False -> model
        Nothing -> enter target model
    Nothing ->
      case model.mouse.hover of
        Just oldTarget -> leave oldTarget model
        Nothing -> model


enter : Target -> Model -> Model
enter (targetId, targetPath) model =
  case targetId of
    T (TopicId topicId) ->
      let
        newModel =
          case model.mouse.dragState of
            Drag dragMode id boxPath origPos lastPos _ ->
              let
                isCyclic = Box.hasDeepItem (toTopicId id) topicId model
                target =
                  -- the hovered item (targetId) is accepted as a drop target if it is
                  -- 1. not contained in item/box being dragged (id), this would create a cycle
                  -- 2. OR draft assoc is in progress
                  if not isCyclic || dragMode == DraftAssoc then
                    Just (targetId, targetPath)
                  else
                    Nothing
              in
              -- update target
              model
                |> setDragState (Drag dragMode id boxPath origPos lastPos target)
            _ -> model
      in
      -- update hover
      newModel
        |> setHover (Just (targetId, targetPath))
    A _ ->
      model


leave : Target -> Model -> Model
leave (targetId, targetPath) model =
  let
    newModel =
      case model.mouse.dragState of
        Drag dragMode id boxPath origPos lastPos _ ->
          -- reset target
          model
            |> setDragState (Drag dragMode id boxPath origPos lastPos Nothing)
        _ -> model
  in
  -- reset hover
  newModel
    |> setHover Nothing


setDragState : DragState -> Model -> Model
setDragState dragState ({mouse} as model) =
  { model | mouse = { mouse | dragState = dragState }}


setHover : Maybe Target -> Model -> Model
setHover hover ({mouse} as model) =
  { model | mouse = { mouse | hover = hover }}


clearHover : Model -> Model
clearHover model =
  model |> setHover Nothing


isDragInProgress : Model -> Bool
isDragInProgress model =
  case model.mouse.dragState of
    Drag _ _ _ _ _ _ -> True
    _ -> False


isHovered : TopicId -> BoxId -> Model -> Bool
isHovered topicId boxId model =
  case model.mouse.hover of
    Just (T topicId_, boxId_ :: _ ) ->
      topicId == topicId_ && boxId == boxId_ -- TODO: box path?
    _ -> False
