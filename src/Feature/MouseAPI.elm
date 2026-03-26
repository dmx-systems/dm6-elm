module Feature.MouseAPI exposing (topicDownHandler, assocClickHandler, dragHandler,
  isDragInProgress, isHovered, clearHover, update)

import Box
import Config as C
import Feature.Mouse as Mouse exposing (DragState(..), DragMode(..))
import Item
import Model exposing (Model, Msg(..))
import ModelParts exposing (..)
import Render.TopicMap.Geometry as Geometry
import Render.TopicMap.Size as Size
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
            ( Mouse <| Mouse.DownOnTopic topicId boxPath pos
            , True -- stopPropagation
            )
          )
      )
  ]


assocClickHandler : Id -> BoxPath -> Attrs Msg
assocClickHandler assocId boxPath =
  [ onClick <| Mouse <| Mouse.AssocClicked assocId boxPath ]


dragHandler : Attrs Msg
dragHandler =
  [ on "pointerdown" <| D.succeed <| Mouse Mouse.Down
  , on "pointermove" <| D.map Mouse <| D.map Mouse.Move U.pointDecoder
  , on "pointerup" <| D.map Mouse <| D.succeed Mouse.Up
  ]



-- UPDATE


update : Mouse.Msg -> UndoModel -> (UndoModel, Cmd Msg)
update msg ({present} as undoModel) =
  case msg of
    -- Topic
    Mouse.Down -> (undoModel, mouseDown)
    Mouse.DownOnTopic id boxPath (pos, pointerType) ->
      mouseDownOnTopic id boxPath pos pointerType present |> Undo.swap undoModel
    Mouse.Move (pos, _) -> mouseMove pos present |> Undo.swap undoModel
    Mouse.Up -> mouseUp present |> Undo.swap undoModel
    Mouse.Time time -> timeArrived time undoModel
    -- Association
    Mouse.AssocClicked id boxPath -> (undoModel, U.command <| ItemClicked id boxPath)


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
      , Task.perform (Mouse << Mouse.Time) Time.now
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
        maybeOrigPos = Box.topicPos id (Box.firstId boxPath) present
        dragState =
          case maybeOrigPos of
            Just origPos -> Drag dragMode id boxPath origPos pos Nothing
            Nothing -> NoDrag -- error is already logged
      in
      (setDragState dragState present, Cmd.none) |> undo undoModel
    _ ->
      U.logError "timeArrived" "Received Time when dragState is not WaitFor..Time"
        (undoModel, Cmd.none)


mouseMove : Point -> Model -> (Model, Cmd Msg)
mouseMove pos model =
  let
    newModel = enterLeave pos model
  in
  case newModel.mouse.dragState of
    DragEngaged time id boxPath pos_ ->
      ( setDragState (WaitForEndTime time id boxPath pos_) newModel
      , Task.perform (Mouse << Mouse.Time) Time.now
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
            DragTopic -> Box.updateTopicPos id boxId
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
              ++ if not droppedOnSourceBox then "move topic" else "abort")
            boxId = Box.firstId boxPath
            -- Can this actually happen? Possibly an edge case when rendering lags behind mouse
            -- move, so that mouse leaves topic and enters box (background). FIXME: store model
            droppedOnSourceBox = boxId == targetId
            msg = MoveTopicToBox id boxId origPos targetId targetPath
          in
          case not droppedOnSourceBox of
            True -> Random.generate msg point
            False -> Cmd.none
        Drag DragTopic _ _ _ _ _ ->
          let
            _ = U.info "mouseUp" "topic drag ended w/o target"
          in
          U.command <| TopicDragged
        Drag DraftAssoc id boxPath _ _ (Just (targetId, targetPath)) ->
          let
            _ = U.info "mouseUp" ("assoc drawn from " ++ fromInt id ++ " (box " ++ Box.fromPath
              boxPath ++ ") to " ++ fromInt targetId ++ " (box " ++ Box.fromPath targetPath
              ++ ") --> " ++ if isSameBox then "create assoc" else "abort")
            boxId = Box.firstId boxPath
            isSameBox = boxId == Box.firstId targetPath
          in
          case isSameBox of
            True -> U.command <| AddAssoc id targetId boxId
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
enterLeave : Point -> Model -> Model
enterLeave pos model =
  let
    filterTopicId =
      case model.mouse.dragState of
        Drag DragTopic topicId _ _ _ _ -> Just topicId
        _ -> Nothing
  in
  case Geometry.pointerTarget pos filterTopicId model of
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
