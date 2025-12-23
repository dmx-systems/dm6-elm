module Feature.MouseAPI exposing (mouseDownHandler, hoverHandler, isHovered, update, sub)

import Box
import Box.Size as Size
import Config as C
import Feature.Mouse as Mouse exposing (DragState(..), DragMode(..))
import Item
import Model exposing (Model, Msg(..))
import ModelParts exposing (..)
import Undo exposing (UndoModel)
import Utils as U

import Browser.Events as Events
import Html.Events exposing (onMouseEnter, onMouseLeave, stopPropagationOn)
import Json.Decode as D
import Random
import String exposing (fromInt)
import Task
import Time exposing (Posix, posixToMillis)



-- VIEW


hoverHandler : Id -> BoxPath -> Attributes Msg
hoverHandler topicId boxPath =
  [ onMouseEnter <| Mouse <| Mouse.Hover "dmx-topic" topicId boxPath
  , onMouseLeave <| Mouse <| Mouse.Unhover "dmx-topic" topicId boxPath
  ]


mouseDownHandler : Id -> BoxPath -> Attributes Msg
mouseDownHandler topicId boxPath =
  [stopPropagationOn "mousedown"
    (U.pointDecoder |> D.andThen
      (\pos -> D.succeed
        ( Mouse <| Mouse.DownOnItem "dmx-topic" topicId boxPath pos
        , True
        )
      )
    )
  ]



-- UPDATE


update : Mouse.Msg -> UndoModel -> (UndoModel, Cmd Msg)
update msg ({present} as undoModel) =
  case msg of
    Mouse.Down -> (undoModel, mouseDown)
    Mouse.DownOnItem class id boxPath pos -> mouseDownOnItem class id boxPath pos present
      |> Undo.swap undoModel
    Mouse.Move pos -> mouseMove pos present |> Undo.swap undoModel
    Mouse.Up -> mouseUp present |> Undo.swap undoModel
    Mouse.Hover class id boxPath -> (hover class id boxPath present, Cmd.none)
      |> Undo.swap undoModel
    Mouse.Unhover class id boxPath -> (unhover class id boxPath present, Cmd.none)
      |> Undo.swap undoModel
    Mouse.Time time -> timeArrived time undoModel


mouseDown : Cmd Msg
mouseDown =
  U.command <| Cancel Nothing


mouseDownOnItem : Class -> Id -> BoxPath -> Point -> Model -> (Model, Cmd Msg)
mouseDownOnItem class id boxPath pos model =
  ( model
    |> setDragState (WaitForStartTime class id boxPath pos)
  , Cmd.batch
    [ U.command <| Cancel <| Just (id, boxPath)
    , Task.perform (Mouse << Mouse.Time) Time.now
    ]
  )


timeArrived : Posix -> UndoModel -> (UndoModel, Cmd Msg)
timeArrived time ({present} as undoModel) =
  case present.mouse.dragState of
    WaitForStartTime class id boxPath pos ->
      let
        dragState = DragEngaged time class id boxPath pos
      in
      (setDragState dragState present, Cmd.none)
      |> Undo.swap undoModel
    WaitForEndTime startTime class id boxPath pos ->
      let
        -- _ = U.info "endTimeArrived" {delay = delay}
        delay = posixToMillis time - posixToMillis startTime
        (dragMode, undo) =
          case delay > C.assocDelayMillis of
            True -> (DraftAssoc, Undo.swap)
            False -> (DragTopic, Undo.push)
        maybeOrigPos = Box.topicPos id (Box.firstId boxPath) present
        dragState =
          case class of
            "dmx-topic" ->
              case maybeOrigPos of
                Just origPos -> Drag dragMode id boxPath origPos pos Nothing
                Nothing -> NoDrag Nothing -- error is already logged
            _ -> NoDrag Nothing -- the error will be logged in performDrag
      in
      (setDragState dragState present, Cmd.none)
      |> undo undoModel
    _ ->
      U.logError "timeArrived" "Received Time when dragState is not WaitFor..Time"
        (undoModel, Cmd.none)


mouseMove : Point -> Model -> (Model, Cmd Msg)
mouseMove pos model =
  case model.mouse.dragState of
    DragEngaged time class id boxPath pos_ ->
      ( setDragState (WaitForEndTime time class id boxPath pos_) model
      , Task.perform (Mouse << Mouse.Time) Time.now
      )
    -- WaitForEndTime _ _ _ _ _ ->
    --   ( model, Cmd.none ) -- ignore -- TODO: can this happen? Is a move listener registered?
    Drag _ _ _ _ _ _ ->
      ( performDrag pos model, Cmd.none )
    _ -> U.logError "mouseMove"
      ("Received \"Move\" message when dragState is " ++ U.toString model.mouse.dragState)
      ( model, Cmd.none )


performDrag : Point -> Model -> Model
performDrag pos model =
  case model.mouse.dragState of
    Drag dragMode id boxPath origPos lastPos target ->
      let
        delta = Point
          (pos.x - lastPos.x)
          (pos.y - lastPos.y)
        boxId = Box.firstId boxPath
        newModel =
          case dragMode of
            DragTopic -> Box.setTopicPosByDelta id boxId delta model
            DraftAssoc -> model
      in
      -- update lastPos
      setDragState (Drag dragMode id boxPath origPos pos target) newModel
      |> Size.auto
    _ -> U.logError "performDrag"
      ("Received \"Move\" message when dragState is " ++ U.toString model.mouse.dragState)
      model


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
        DragEngaged _ _ id boxPath _ ->
          let
            _ = U.info "mouseUp" "item not moved -> ItemClicked"
          in
          U.command <| ItemClicked id boxPath
        _ ->
          U.logError "mouseUp"
            ("Received \"Up\" message when dragState is " ++ U.toString model.mouse.dragState)
            Cmd.none
  in
  (setDragState (NoDrag Nothing) model, cmd)


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


hover : Class -> Id -> BoxPath -> Model -> Model
hover class targetId targetPath model =
  case model.mouse.dragState of
    Drag dragMode id boxPath origPos lastPos _ ->
      let
        isSelf = (id, Box.firstId boxPath) == (targetId, Box.firstId targetPath)
        isBox = Item.isBox targetId model
        target =
          -- the hovered item is a potential drop target if
          -- 1. the hovered item is not the item being dragged (can't drop on self), AND
          -- 2. the hovered item is a box OR draft assoc is in progress
          if not isSelf && (isBox || dragMode == DraftAssoc) then
            Just (targetId, targetPath)
          else
            Nothing
      in
      -- update target
      model
      |> setDragState (Drag dragMode id boxPath origPos lastPos target)
    NoDrag _ ->
      -- update target
      model
      |> setDragState (NoDrag <| Just (targetId, targetPath))
    _ -> model


unhover : Class -> Id -> BoxPath -> Model -> Model
unhover class targetId targetPath model =
  case model.mouse.dragState of
    Drag dragMode id boxPath origPos lastPos _ ->
      -- reset target
      model
      |> setDragState (Drag dragMode id boxPath origPos lastPos Nothing)
    NoDrag _ ->
      -- reset target
      model
      |> setDragState (NoDrag Nothing)
    _ -> model


setDragState : DragState -> Model -> Model
setDragState dragState ({mouse} as model) =
  { model | mouse = { mouse | dragState = dragState }}


isHovered : Id -> BoxId -> Model -> Bool
isHovered itemId boxId model =
  case model.mouse.dragState of
    NoDrag (Just (itemId_, boxId_ :: _ )) ->
      itemId == itemId_ && boxId == boxId_ -- TODO: box path?
    _ -> False



-- SUBSCRIPTIONS


sub : UndoModel -> Sub Msg
sub {present} =
  case present.mouse.dragState of
    WaitForStartTime _ _ _ _ -> Sub.none
    WaitForEndTime _ _ _ _ _ -> Sub.none
    DragEngaged _ _ _ _ _ -> dragSub
    Drag _ _ _ _ _ _ -> dragSub
    NoDrag _ -> mouseDownSub


mouseDownSub : Sub Msg
mouseDownSub =
  Events.onMouseDown <| D.succeed <| Mouse Mouse.Down


dragSub : Sub Msg
dragSub =
  Sub.batch
    [ Events.onMouseMove <| D.map Mouse <| D.map Mouse.Move U.pointDecoder
    , Events.onMouseUp <| D.map Mouse <| D.succeed Mouse.Up
    ]
