module MouseAPI exposing (hoverHandler, subs, update)

import AutoSize as Size
import Config as C
import Model exposing (Model, UndoModel, Msg(..))
import ModelAPI as A
import ModelHelper exposing (..)
import Storage as S
import Utils as U
-- feature modules
import Mouse exposing (DragState(..), DragMode(..))
import SearchAPI
import SelectionAPI as Sel
import IconMenuAPI

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
    Mouse.Down -> (mouseDown present, Cmd.none) |> A.swap undoModel
    Mouse.DownOnItem class id boxPath pos -> mouseDownOnItem class id boxPath pos present
      |> A.swap undoModel
    Mouse.Move pos -> mouseMove pos present |> A.swap undoModel
    Mouse.Up -> mouseUp undoModel
    Mouse.Over class id boxPath -> (mouseOver class id boxPath present, Cmd.none)
      |> A.swap undoModel
    Mouse.Out class id boxPath -> (mouseOut class id boxPath present, Cmd.none)
      |> A.swap undoModel
    Mouse.Time time -> timeArrived time undoModel


mouseDown : Model -> Model
mouseDown model =
  model
  |> Sel.reset
  |> IconMenuAPI.close
  |> SearchAPI.closeMenu


mouseDownOnItem : Class -> Id -> BoxPath -> Point -> Model -> (Model, Cmd Msg)
mouseDownOnItem class id boxPath pos model =
  (updateDragState (WaitForStartTime class id boxPath pos) model
    |> Sel.select id boxPath
  , Task.perform (Mouse << Mouse.Time) Time.now
  )


timeArrived : Posix -> UndoModel -> (UndoModel, Cmd Msg)
timeArrived time ({present} as undoModel) =
  case present.mouse.dragState of
    WaitForStartTime class id boxPath pos ->
      let
        dragState = DragEngaged time class id boxPath pos
      in
      (updateDragState dragState present, Cmd.none)
      |> A.swap undoModel
    WaitForEndTime startTime class id boxPath pos ->
      let
        delay = posixToMillis time - posixToMillis startTime > C.assocDelayMillis
        (dragMode, historyFunc) = if delay then (DraftAssoc, A.swap) else (DragTopic, A.push)
        maybeOrigPos = A.topicPos id (A.firstId boxPath) present.boxes
        dragState =
          case class of
            "dmx-topic" ->
              case maybeOrigPos of
                Just origPos -> Drag dragMode id boxPath origPos pos Nothing
                Nothing -> NoDrag -- error is already logged
            _ -> NoDrag -- the error will be logged in performDrag
      in
      (updateDragState dragState present, Cmd.none)
      |> historyFunc undoModel
    _ ->
      U.logError "timeArrived" "Received \"Time\" message when dragState is not WaitForTime"
        (undoModel, Cmd.none)


mouseMove : Point -> Model -> (Model, Cmd Msg)
mouseMove pos model =
  case model.mouse.dragState of
    DragEngaged time class id boxPath pos_ ->
      ( updateDragState (WaitForEndTime time class id boxPath pos_) model
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
        boxId = A.firstId boxPath
        newModel =
          case dragMode of
            DragTopic -> A.setTopicPosByDelta id boxId delta model
            DraftAssoc -> model
      in
      -- update lastPos
      updateDragState (Drag dragMode id boxPath origPos pos target) newModel
      |> Size.auto
    _ -> U.logError "performDrag"
      ("Received \"Move\" message when dragState is " ++ U.toString model.mouse.dragState)
      model


mouseUp : UndoModel -> (UndoModel, Cmd Msg)
mouseUp ({present} as undoModel) =
  let
    (model, cmd, historyFunc) =
      case present.mouse.dragState of
        Drag DragTopic id boxPath origPos _ (Just (targetId, targetBoxPath)) ->
          let
            _ = U.info "mouseUp" ("dropped " ++ fromInt id ++ " (box " ++ A.fromPath boxPath
              ++ ") on " ++ fromInt targetId ++ " (box " ++ A.fromPath targetBoxPath ++ ") --> "
              ++ if notDroppedOnOwnBox then "move topic" else "abort")
            boxId = A.firstId boxPath
            notDroppedOnOwnBox = boxId /= targetId
            msg = MoveTopicToBox id boxId origPos targetId targetBoxPath
          in
          if notDroppedOnOwnBox then
            (present, Random.generate msg point, A.swap)
          else
            (present, Cmd.none, A.swap)
        Drag DraftAssoc id boxPath _ _ (Just (targetId, targetBoxPath)) ->
          let
            _ = U.info "mouseUp" ("assoc drawn from " ++ fromInt id ++ " (box " ++ A.fromPath
              boxPath ++ ") to " ++ fromInt targetId ++ " (box " ++ A.fromPath targetBoxPath
              ++ ") --> " ++ if isSameBox then "create assoc" else "abort")
            boxId = A.firstId boxPath
            isSameBox = boxId == A.firstId targetBoxPath
          in
          if isSameBox then
            (addDefaultAssocAndAddToBox id targetId boxId present, Cmd.none, A.push)
          else
            (present, Cmd.none, A.swap)
        Drag _ _ _ _ _ _ ->
          let
            _ = U.info "mouseUp" "drag ended w/o target"
          in
          (present, Cmd.none, A.swap)
        DragEngaged _ _ _ _ _ ->
          let
            _ = U.info "mouseUp" "drag aborted w/o moving"
          in
          (present, Cmd.none, A.swap)
        _ ->
          U.logError "mouseUp"
            ("Received \"Up\" message when dragState is " ++ U.toString present.mouse.dragState)
            (present, Cmd.none, A.swap)
  in
  (updateDragState NoDrag model, cmd)
  |> S.storeWith
  |> historyFunc undoModel


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
        isSelf = (id, A.firstId boxPath) == (targetId, A.firstId targetBoxPath)
        isBox = A.isBox targetId model.boxes
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
      updateDragState (Drag dragMode id boxPath origPos lastPos target) model
    DragEngaged _ _ _ _ _ ->
      U.logError "mouseOver" "Received \"Over\" message when dragState is DragEngaged" model
    _ -> model


mouseOut : Class -> Id -> BoxPath -> Model -> Model
mouseOut class targetId targetBoxPath model =
  case model.mouse.dragState of
    Drag dragMode id boxPath origPos lastPos _ ->
      -- reset target
      updateDragState (Drag dragMode id boxPath origPos lastPos Nothing) model
    _ -> model


updateDragState : DragState -> Model -> Model
updateDragState dragState ({mouse} as model) =
  { model | mouse = { mouse | dragState = dragState }}


-- Presumption: both players exist in same box
addDefaultAssocAndAddToBox : Id -> Id -> BoxId -> Model -> Model
addDefaultAssocAndAddToBox player1 player2 boxId model =
  addAssocAndAddToBox
    "dmx.association"
    "dmx.default" player1
    "dmx.default" player2
    boxId model


-- Presumption: both players exist in same box
addAssocAndAddToBox : ItemType -> RoleType -> Id -> RoleType -> Id -> BoxId -> Model -> Model
addAssocAndAddToBox itemType role1 player1 role2 player2 boxId model =
  let
    (newModel, assocId) = A.addAssoc itemType role1 player1 role2 player2 model
    props = AssocV AssocProps
  in
  A.addItemToBox assocId props boxId newModel



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
