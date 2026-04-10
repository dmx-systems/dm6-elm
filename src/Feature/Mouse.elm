module Feature.Mouse exposing (topicDownHandler, assocClickHandler, dragHandler,
  isDragInProgress, isHovered, clearHover, update)

import Box
import BoxRenderer exposing (BoxGeometry)
import Config as C
import Feature.MouseDef as MouseDef exposing (DragState(..), DragMode(..))
import Item
import Model exposing (Model, Msg(..))
import ModelBase exposing (..)
import TopicMap.Size as Size
import TopicMap.TopicMap as TM
import Undo exposing (UndoModel)
import Utils as U

import Html.Events exposing (on, onClick, stopPropagationOn)
import Json.Decode as D
import Random
import String exposing (fromInt)
import Task
import Time exposing (Posix, posixToMillis)



-- VIEW


topicDownHandler : Id -> BoxPath -> Attrs Msg
topicDownHandler topicId boxPath =
  [ stopPropagationOn "pointerdown"
      ( U.pointDecoder |> D.andThen
          (\pos -> D.succeed
            ( Mouse <| MouseDef.DownOnTopic topicId boxPath pos
            , True -- stopPropagation
            )
          )
      )
  ]


assocClickHandler : Id -> BoxPath -> Attrs Msg
assocClickHandler assocId boxPath =
  [ onClick <| Mouse <| MouseDef.AssocClicked assocId boxPath ]


dragHandler : Attrs Msg
dragHandler =
  [ on "pointerdown" <| D.succeed <| Mouse MouseDef.Down
  , on "pointermove" <| D.map Mouse <| D.map MouseDef.Move U.pointDecoder
  , on "pointerup" <| D.map Mouse <| D.succeed MouseDef.Up
  ]



-- UPDATE


update : MouseDef.Msg -> BoxGeometry -> UndoModel -> (UndoModel, Cmd Msg)
update msg findTopicAt ({present} as undoModel) =
  case msg of
    -- Topic
    MouseDef.Down -> (undoModel, mouseDown)
    MouseDef.DownOnTopic id boxPath (pos, pointerType) ->
      mouseDownOnTopic id boxPath pos pointerType present |> Undo.swap undoModel
    MouseDef.Move (pos, _) -> mouseMove pos findTopicAt present |> Undo.swap undoModel
    MouseDef.Up -> mouseUp present |> Undo.swap undoModel
    MouseDef.Time time -> timeArrived time undoModel
    -- Association
    MouseDef.AssocClicked id boxPath -> (undoModel, U.command <| ItemClicked id boxPath)


mouseDown : Cmd Msg
mouseDown =
  U.command <| Cancel Nothing


mouseDownOnTopic : Id -> BoxPath -> Point -> PointerType -> Model -> (Model, Cmd Msg)
mouseDownOnTopic topicId boxPath pos pointerType model =
  ( model
      |> emulateHover topicId boxPath pointerType
      |> setDragState (WaitForStartTime topicId boxPath pos)
  , Cmd.batch
      [ U.command <| Cancel <| Just (topicId, boxPath)
      , Task.perform (Mouse << MouseDef.Time) Time.now
      ]
  )


emulateHover : Id -> BoxPath -> PointerType -> Model -> Model
emulateHover topicId boxPath pointerType model =
  case pointerType == "touch" of
    True -> model |> setHover (Just (topicId, boxPath))
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
      U.logError "timeArrived" "Received Time when dragState is not WaitFor..Time"
        (undoModel, Cmd.none)


mouseMove : Point -> BoxGeometry -> Model -> (Model, Cmd Msg)
mouseMove pos findTopicAt model =
  let
    newModel = enterLeave pos findTopicAt model
  in
  case newModel.mouse.dragState of
    DragEngaged time id boxPath pos_ ->
      ( setDragState (WaitForEndTime time id boxPath pos_) newModel
      , Task.perform (Mouse << MouseDef.Time) Time.now
      )
    Drag _ _ _ _ _ _ ->
      ( performDrag pos newModel, Cmd.none )
    _ ->
      ( newModel, Cmd.none )


performDrag : Point -> Model -> Model
performDrag pos model =
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
        |> Size.auto
    _ -> U.logError "performDrag"
      ("Received \"Move\" when dragState is " ++ U.toString model.mouse.dragState) model


mouseUp : Model -> (Model, Cmd Msg)
mouseUp model =
  let
    cmd =
      case model.mouse.dragState of
        Drag DragTopic id boxPath origPos _ (Just (targetId, targetPath)) ->
          let
            _ = U.info "mouseUp" ("dropped " ++ fromInt id ++ " (box " ++ Box.fromPath boxPath
              ++ ") on " ++ fromInt targetId ++ " (box " ++ Box.fromPath targetPath ++ ") --> "
              ++ if shouldMoveToBox then "move topic to box" else "abort")
            boxId = Box.firstId boxPath
            -- When dragging a topic inside a nested box that box will be the target (this is
            -- since target is determined by map geometry, not by enter/leave events anymore).
            -- We distinguish a topic-moved-to-box from a topic-dragged-inside-box by comparing
            -- the dragged topic's parent box.
            shouldMoveToBox = boxId /= targetId
            msg = MoveTopicToBox id boxId origPos targetId targetPath
          in
          case shouldMoveToBox of
            True -> Random.generate msg point
            False -> U.command TopicDragged -- store topic pos
        Drag DragTopic _ _ _ _ _ ->
          let
            _ = U.info "mouseUp" "topic drag ended w/o target"
          in
          U.command TopicDragged
        Drag DraftAssoc id boxPath _ _ (Just (targetId, targetPath)) ->
          let
            _ = U.info "mouseUp" ("assoc drawn from " ++ fromInt id ++ " (box " ++ Box.fromPath
              boxPath ++ ") to " ++ fromInt targetId ++ " (box " ++ Box.fromPath targetPath
              ++ ") --> " ++ if isSameBox then "create assoc" else "abort")
            boxId = Box.firstId boxPath
            isSameBox = boxId == Box.firstId targetPath
          in
          case isSameBox of
            True -> U.command <| CreateAssoc id targetId boxId
            False -> Cmd.none
        Drag DraftAssoc _ _ _ _ _ ->
          let
            _ = U.info "mouseUp" "assoc ended w/o target"
          in
          Cmd.none
        DragEngaged _ id boxPath _ ->
          let
            _ = U.info "mouseUp" "item not moved -> ItemClicked"
          in
          U.command <| ItemClicked id boxPath
        _ ->
          Cmd.none
  in
  (setDragState NoDrag model, cmd)


point : Random.Generator Point
point =
  let
    cx = C.topicW2 + C.whiteBoxPadding
    cy = C.topicH2 + C.whiteBoxPadding
    rw = C.whiteBoxRange.w
    rh = C.whiteBoxRange.h
  in
  Random.map2
    (\x y -> Point (cx + x) (cy + y))
    (Random.int 0 rw)
    (Random.int 0 rh)


{- Emulates enter/leave events by the means of geometry. Based on the given pointer
coordinate decides whether to call the "enter" and/or "leave" handlers. -}
enterLeave : Point -> BoxGeometry -> Model -> Model
enterLeave pos findTopicAt model =
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
  case findTopicAt model.boxId [] initPos excludeTopicId model of
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


enter : (Id, BoxPath) -> Model -> Model
enter (targetId, targetPath) model =
  let
    newModel =
      case model.mouse.dragState of
        Drag dragMode id boxPath origPos lastPos _ ->
          let
            isBox = Item.isBox targetId model
            isCyclic = Box.hasDeepItem id targetId model
            target =
              -- the hovered item (targetId) is a drop target if it is
              -- 1. a box AND
              -- 2. not contained in item/box being dragged (id), this would create a cycle OR
              -- 3. draft assoc is in progress
              if isBox && not isCyclic || dragMode == DraftAssoc then
                Just (targetId, targetPath)
              else
                Nothing
          in
          -- update target
          model |> setDragState (Drag dragMode id boxPath origPos lastPos target)
        _ -> model
  in
  -- update hover
  newModel |> setHover (Just (targetId, targetPath))


leave : (Id, BoxPath) -> Model -> Model
leave (targetId, targetPath) model =
  let
    newModel =
      case model.mouse.dragState of
        Drag dragMode id boxPath origPos lastPos _ ->
          -- reset target
          model |> setDragState (Drag dragMode id boxPath origPos lastPos Nothing)
        _ -> model
  in
  -- reset hover
  newModel |> setHover Nothing


setDragState : DragState -> Model -> Model
setDragState dragState ({mouse} as model) =
  { model | mouse = { mouse | dragState = dragState }}


setHover : Maybe (Id, BoxPath) -> Model -> Model
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


isHovered : Id -> BoxId -> Model -> Bool
isHovered itemId boxId model =
  case model.mouse.hover of
    Just (itemId_, boxId_ :: _ ) ->
      itemId == itemId_ && boxId == boxId_ -- TODO: box path?
    _ -> False
