module Feature.MouseAPI exposing (hoverHandler, subs, update)

import Box
import Box.Size as Size
import Config as C
import Feature.Mouse as Mouse exposing (DragState(..), DragMode(..))
import Item
import Model exposing (Model, Msg(..))
import ModelHelper exposing (..)
import Undo exposing (UndoModel)
import Utils as U

import Browser.Events as Events
import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as D
import Random
import String exposing (fromInt)
import Task
import Time exposing (Posix, posixToMillis)



-- VIEW


hoverHandler : List (Attribute Msg)
hoverHandler =
  [ on "mouseover" (mouseDecoder Mouse.Over)
  , on "mouseout" (mouseDecoder Mouse.Out)
  ]



-- UPDATE


update : Mouse.Msg -> UndoModel -> (UndoModel, Cmd Msg)
update msg ({present} as undoModel) =
  case msg of
    Mouse.Down -> (present, command ClickedBackground) |> Undo.swap undoModel
    Mouse.DownOnItem class id boxPath pos -> mouseDownOnItem class id boxPath pos present
      |> Undo.swap undoModel
    Mouse.Move pos -> mouseMove pos present |> Undo.swap undoModel
    Mouse.Up -> mouseUp present |> Undo.swap undoModel
    Mouse.Over class id boxPath -> (mouseOver class id boxPath present, Cmd.none)
      |> Undo.swap undoModel
    Mouse.Out class id boxPath -> (mouseOut class id boxPath present, Cmd.none)
      |> Undo.swap undoModel
    Mouse.Time time -> timeArrived time undoModel


mouseDownOnItem : Class -> Id -> BoxPath -> Point -> Model -> (Model, Cmd Msg)
mouseDownOnItem class id boxPath pos model =
  ( model
    |> setDragState (WaitForStartTime class id boxPath pos)
  , Cmd.batch
    [ command <| ClickedItem id boxPath
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
        delay = posixToMillis time - posixToMillis startTime > C.assocDelayMillis
        (dragMode, historyFunc) =
          case delay of
            True -> (DraftAssoc, Undo.swap)
            False -> (DragTopic, Undo.push)
        maybeOrigPos = Box.topicPos id (Box.firstId boxPath) present
        dragState =
          case class of
            "dmx-topic" ->
              case maybeOrigPos of
                Just origPos -> Drag dragMode id boxPath origPos pos Nothing
                Nothing -> NoDrag -- error is already logged
            _ -> NoDrag -- the error will be logged in performDrag
      in
      (setDragState dragState present, Cmd.none)
      |> historyFunc undoModel
    _ ->
      U.logError "timeArrived" "Received \"Time\" message when dragState is not WaitForTime"
        (undoModel, Cmd.none)


mouseMove : Point -> Model -> (Model, Cmd Msg)
mouseMove pos model =
  case model.mouse.dragState of
    DragEngaged time class id boxPath pos_ ->
      ( setDragState (WaitForEndTime time class id boxPath pos_) model
      , Task.perform (Mouse << Mouse.Time) Time.now
      )
    WaitForEndTime _ _ _ _ _ ->
      ( model, Cmd.none ) -- ignore -- TODO: can this happen at all? Is there a move listener?
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
        Drag DragTopic id boxPath origPos _ (Just (targetId, targetBoxPath)) ->
          let
            _ = U.info "mouseUp" ("dropped " ++ fromInt id ++ " (box " ++ Box.fromPath boxPath
              ++ ") on " ++ fromInt targetId ++ " (box " ++ Box.fromPath targetBoxPath
              ++ ") --> " ++ if not droppedOnSourceBox then "move topic" else "abort")
            boxId = Box.firstId boxPath
            -- Can this actually happen? Possibly an edge case when rendering lags behind mouse
            -- move, so that mouse leaves topic and enters box (background). FIXME: store model
            droppedOnSourceBox = boxId == targetId
            msg = MoveTopicToBox id boxId origPos targetId targetBoxPath
          in
          case not droppedOnSourceBox of
            True -> Random.generate msg point
            False -> Cmd.none
        Drag DragTopic _ _ _ _ _ ->
          let
            _ = U.info "mouseUp" "drag ended w/o target"
          in
          command <| DraggedTopic
        Drag DraftAssoc id boxPath _ _ (Just (targetId, targetBoxPath)) ->
          let
            _ = U.info "mouseUp" ("assoc drawn from " ++ fromInt id ++ " (box " ++ Box.fromPath
              boxPath ++ ") to " ++ fromInt targetId ++ " (box " ++ Box.fromPath targetBoxPath
              ++ ") --> " ++ if isSameBox then "create assoc" else "abort")
            boxId = Box.firstId boxPath
            isSameBox = boxId == Box.firstId targetBoxPath
          in
          case isSameBox of
            True -> command <| AddAssoc id targetId boxId
            False -> Cmd.none
        Drag DraftAssoc _ _ _ _ _ ->
          let
            _ = U.info "mouseUp" "assoc ended w/o target"
          in
          Cmd.none
        DragEngaged _ _ _ _ _ ->
          let
            _ = U.info "mouseUp" "drag aborted w/o moving"
          in
          Cmd.none
        _ ->
          U.logError "mouseUp"
            ("Received \"Up\" message when dragState is " ++ U.toString model.mouse.dragState)
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
    (Random.float 0 rw)
    (Random.float 0 rh)


mouseOver : Class -> Id -> BoxPath -> Model -> Model
mouseOver class targetId targetBoxPath model =
  case model.mouse.dragState of
    Drag dragMode id boxPath origPos lastPos _ ->
      let
        isSelf = (id, Box.firstId boxPath) == (targetId, Box.firstId targetBoxPath)
        isBox = Item.isBox targetId model
        target =
          -- the hovered item is a potential drop target if
          -- 1. the hovered item is not the item being dragged (can't drop on self), AND
          -- 2. the hovered item is a box OR draft assoc is in progress
          if not isSelf && (isBox || dragMode == DraftAssoc) then
            Just (targetId, targetBoxPath)
          else
            Nothing
      in
      -- update target
      setDragState (Drag dragMode id boxPath origPos lastPos target) model
    DragEngaged _ _ _ _ _ ->
      U.logError "mouseOver" "Received \"Over\" message when dragState is DragEngaged" model
    _ -> model


mouseOut : Class -> Id -> BoxPath -> Model -> Model
mouseOut class targetId targetBoxPath model =
  case model.mouse.dragState of
    Drag dragMode id boxPath origPos lastPos _ ->
      -- reset target
      setDragState (Drag dragMode id boxPath origPos lastPos Nothing) model
    _ -> model


setDragState : DragState -> Model -> Model
setDragState dragState ({mouse} as model) =
  { model | mouse = { mouse | dragState = dragState }}


command : Msg -> Cmd Msg
command msg =
  Task.succeed () |> Task.perform (\_ -> msg)



-- SUBSCRIPTIONS


subs : UndoModel -> Sub Msg
subs {present} =
  case present.mouse.dragState of
    WaitForStartTime _ _ _ _ -> Sub.none
    WaitForEndTime _ _ _ _ _ -> Sub.none
    DragEngaged _ _ _ _ _ -> dragSub
    Drag _ _ _ _ _ _ -> dragSub
    NoDrag -> mouseDownSub


mouseDownSub : Sub Msg
mouseDownSub =
  Events.onMouseDown <| D.oneOf
    [ D.map Mouse <| D.map4 Mouse.DownOnItem
        U.classDecoder U.idDecoder U.pathDecoder U.pointDecoder
    , D.succeed (Mouse Mouse.Down)
    ]


dragSub : Sub Msg
dragSub =
  Sub.batch
    [ Events.onMouseMove <| D.map Mouse <| D.map Mouse.Move U.pointDecoder
    , Events.onMouseUp <| D.map Mouse <| D.succeed Mouse.Up
    ]


mouseDecoder : (Class -> Id -> BoxPath -> Mouse.Msg) -> D.Decoder Msg
mouseDecoder msg =
  D.map Mouse <| D.map3 msg U.classDecoder U.idDecoder U.pathDecoder
