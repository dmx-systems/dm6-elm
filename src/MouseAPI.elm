module MouseAPI exposing (mouseHoverHandler, mouseSubs, updateMouse)

import AppModel exposing (UndoModel, Model, Msg(..))
import Config exposing (topicW2, topicH2, assocDelayMillis, whiteBoxRange, whiteBoxPadding)
import MapAutoSize exposing (autoSize)
import Model exposing (Class, Id, MapPath, Point)
import ModelAPI exposing (getTopicPos, setTopicPosByDelta, createDefaultAssocIn, getMapId,
  select, resetSelection, fromPath, push, swap)
import Storage exposing (storeWith)
import Utils exposing (classDecoder, idDecoder, pathDecoder, pointDecoder, logError, info,
  toString)
-- components
import Mouse exposing (DragState(..), DragMode(..))
import SearchAPI exposing (closeResultMenu)
import IconMenuAPI exposing (closeIconMenu)

import Browser.Events as Events
import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as D
import Random
import String exposing (fromInt)
import Task
import Time exposing (Posix, posixToMillis)



-- VIEW


mouseHoverHandler : List (Attribute Msg)
mouseHoverHandler =
  [ on "mouseover" (mouseDecoder Mouse.Over)
  , on "mouseout" (mouseDecoder Mouse.Out)
  ]



-- UPDATE


updateMouse : Mouse.Msg -> UndoModel -> (UndoModel, Cmd Msg)
updateMouse msg ({present} as undoModel) =
  case msg of
    Mouse.Down -> (mouseDown present, Cmd.none) |> swap undoModel
    Mouse.DownOnItem class id mapPath pos -> mouseDownOnItem present class id mapPath pos
      |> swap undoModel
    Mouse.Move pos -> mouseMove present pos |> swap undoModel
    Mouse.Up -> mouseUp undoModel
    Mouse.Over class id mapPath -> (mouseOver present class id mapPath, Cmd.none)
      |> swap undoModel
    Mouse.Out class id mapPath -> (mouseOut present class id mapPath, Cmd.none)
      |> swap undoModel
    Mouse.Time time -> timeArrived time undoModel


mouseDown : Model -> Model
mouseDown model =
  model
  |> resetSelection
  |> closeIconMenu
  |> closeResultMenu


mouseDownOnItem : Model -> Class -> Id -> MapPath -> Point -> (Model, Cmd Msg)
mouseDownOnItem model class id mapPath pos =
  (updateDragState model (WaitForStartTime class id mapPath pos)
    |> select id mapPath
  , Task.perform (Mouse << Mouse.Time) Time.now
  )


timeArrived : Posix -> UndoModel -> (UndoModel, Cmd Msg)
timeArrived time ({present} as undoModel) =
  case present.mouse.dragState of
    WaitForStartTime class id mapPath pos ->
      let
        dragState = DragEngaged time class id mapPath pos
      in
      (updateDragState present dragState, Cmd.none)
      |> swap undoModel
    WaitForEndTime startTime class id mapPath pos ->
      let
        delay = posixToMillis time - posixToMillis startTime > assocDelayMillis
        (dragMode, historyFunc) = if delay then (DrawAssoc, swap) else (DragTopic, push)
        maybeOrigPos = getTopicPos id (getMapId mapPath) present.maps
        dragState =
          case class of
            "dmx-topic" ->
              case maybeOrigPos of
                Just origPos -> Drag dragMode id mapPath origPos pos Nothing
                Nothing -> NoDrag -- error is already logged
            _ -> NoDrag -- the error will be logged in performDrag
      in
      (updateDragState present dragState, Cmd.none)
      |> historyFunc undoModel
    _ ->
      logError "timeArrived" "Received \"Time\" message when dragState is not WaitForTime"
        (undoModel, Cmd.none)


mouseMove : Model -> Point -> (Model, Cmd Msg)
mouseMove model pos =
  case model.mouse.dragState of
    DragEngaged time class id mapPath pos_ ->
      ( updateDragState model <| WaitForEndTime time class id mapPath pos_
      , Task.perform (Mouse << Mouse.Time) Time.now
      )
    WaitForEndTime _ _ _ _ _ ->
      ( model, Cmd.none ) -- ignore -- TODO: can this happen at all? Is there a move listener?
    Drag _ _ _ _ _ _ ->
      ( performDrag model pos, Cmd.none )
    _ -> logError "mouseMove"
      ("Received \"Move\" message when dragState is " ++ toString model.mouse.dragState)
      ( model, Cmd.none )


performDrag : Model -> Point -> Model
performDrag model pos =
  case model.mouse.dragState of
    Drag dragMode id mapPath origPos lastPos target ->
      let
        delta = Point
          (pos.x - lastPos.x)
          (pos.y - lastPos.y)
        mapId = getMapId mapPath
        newModel =
          case dragMode of
            DragTopic -> setTopicPosByDelta id mapId delta model
            DrawAssoc -> model
      in
      -- update lastPos
      updateDragState newModel (Drag dragMode id mapPath origPos pos target)
      |> autoSize
    _ -> logError "performDrag"
      ("Received \"Move\" message when dragState is " ++ toString model.mouse.dragState)
      model


mouseUp : UndoModel -> (UndoModel, Cmd Msg)
mouseUp ({present} as undoModel) =
  let
    (model, cmd, historyFunc) =
      case present.mouse.dragState of
        Drag DragTopic id mapPath origPos _ (Just (targetId, targetMapPath)) ->
          let
            _ = info "mouseUp" ("dropped " ++ fromInt id ++ " (map " ++ fromPath mapPath
              ++ ") on " ++ fromInt targetId ++ " (map " ++ fromPath targetMapPath ++ ") --> "
              ++ if notDroppedOnOwnMap then "move topic" else "abort")
            mapId = getMapId mapPath
            notDroppedOnOwnMap = mapId /= targetId
            msg = MoveTopicToMap id mapId origPos targetId targetMapPath
          in
          if notDroppedOnOwnMap then
            (present, Random.generate msg point, swap)
          else
            (present, Cmd.none, swap)
        Drag DrawAssoc id mapPath _ _ (Just (targetId, targetMapPath)) ->
          let
            _ = info "mouseUp" ("assoc drawn from " ++ fromInt id ++ " (map " ++ fromPath
              mapPath ++ ") to " ++ fromInt targetId ++ " (map " ++ fromPath targetMapPath
              ++ ") --> " ++ if isSameMap then "create assoc" else "abort")
            mapId = getMapId mapPath
            isSameMap = mapId == getMapId targetMapPath
          in
          if isSameMap then
            (createDefaultAssocIn id targetId mapId present, Cmd.none, push)
          else
            (present, Cmd.none, swap)
        Drag _ _ _ _ _ _ ->
          let
            _ = info "mouseUp" "drag ended w/o target"
          in
          (present, Cmd.none, swap)
        DragEngaged _ _ _ _ _ ->
          let
            _ = info "mouseUp" "drag aborted w/o moving"
          in
          (present, Cmd.none, swap)
        _ ->
          logError "mouseUp"
            ("Received \"Up\" message when dragState is " ++ toString present.mouse.dragState)
            (present, Cmd.none, swap)
  in
  (updateDragState model NoDrag, cmd)
  |> storeWith
  |> historyFunc undoModel


point : Random.Generator Point
point =
  let
    cx = topicW2 + whiteBoxPadding
    cy = topicH2 + whiteBoxPadding
    rw = whiteBoxRange.w
    rh = whiteBoxRange.h
  in
  Random.map2
    (\x y -> Point (cx + x) (cy + y))
    (Random.float 0 rw)
    (Random.float 0 rh)


mouseOver : Model -> Class -> Id -> MapPath -> Model
mouseOver model class targetId targetMapPath =
  case model.mouse.dragState of
    Drag dragMode id mapPath origPos lastPos _ ->
      let
        mapId = getMapId mapPath
        targetMapId = getMapId targetMapPath
        target =
          if (id, mapId) /= (targetId, targetMapId) then -- TODO: mapId comparison needed?
            Just (targetId, targetMapPath)
          else
            Nothing
      in
      -- update target
      updateDragState model <| Drag dragMode id mapPath origPos lastPos target
    DragEngaged _ _ _ _ _ ->
      logError "mouseOver" "Received \"Over\" message when dragState is DragEngaged" model
    _ -> model


mouseOut : Model -> Class -> Id -> MapPath -> Model
mouseOut model class targetId targetMapPath =
  case model.mouse.dragState of
    Drag dragMode id mapPath origPos lastPos _ ->
      -- reset target
      updateDragState model <| Drag dragMode id mapPath origPos lastPos Nothing
    _ -> model


updateDragState : Model -> DragState -> Model
updateDragState ({mouse} as model) dragState =
  { model | mouse = { mouse | dragState = dragState }}



-- SUBSCRIPTIONS


mouseSubs : UndoModel -> Sub Msg
mouseSubs {present} =
  case present.mouse.dragState of
    WaitForStartTime _ _ _ _ -> Sub.none
    WaitForEndTime _ _ _ _ _ -> Sub.none
    DragEngaged _ _ _ _ _ -> dragSub
    Drag _ _ _ _ _ _ -> dragSub
    NoDrag -> mouseDownSub


mouseDownSub : Sub Msg
mouseDownSub =
  Events.onMouseDown <| D.oneOf
    [ D.map Mouse <| D.map4 Mouse.DownOnItem classDecoder idDecoder pathDecoder pointDecoder
    , D.succeed (Mouse Mouse.Down)
    ]


dragSub : Sub Msg
dragSub =
  Sub.batch
    [ Events.onMouseMove <| D.map Mouse <| D.map Mouse.Move pointDecoder
    , Events.onMouseUp <| D.map Mouse <| D.succeed Mouse.Up
    ]


mouseDecoder : (Class -> Id -> MapPath -> Mouse.Msg) -> D.Decoder Msg
mouseDecoder msg =
  D.map Mouse <| D.map3 msg classDecoder idDecoder pathDecoder
