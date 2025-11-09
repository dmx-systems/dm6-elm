module MouseAPI exposing (mouseHoverHandler, mouseSubs, updateMouse)

import AppModel exposing (UndoModel, Model, Msg(..))
import Config as C
import MapAutoSize exposing (autoSize)
import Model exposing (Class, Id, MapId, MapPath, Point, ItemType, RoleType, MapProps(..),
  AssocProps)
import ModelAPI as A
import Storage exposing (storeWith)
import Utils as U exposing (logError, info, toString)
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
    Mouse.Down -> (mouseDown present, Cmd.none) |> A.swap undoModel
    Mouse.DownOnItem class id mapPath pos -> mouseDownOnItem class id mapPath pos present
      |> A.swap undoModel
    Mouse.Move pos -> mouseMove pos present |> A.swap undoModel
    Mouse.Up -> mouseUp undoModel
    Mouse.Over class id mapPath -> (mouseOver class id mapPath present, Cmd.none)
      |> A.swap undoModel
    Mouse.Out class id mapPath -> (mouseOut class id mapPath present, Cmd.none)
      |> A.swap undoModel
    Mouse.Time time -> timeArrived time undoModel


mouseDown : Model -> Model
mouseDown model =
  model
  |> A.resetSelection
  |> closeIconMenu
  |> closeResultMenu


mouseDownOnItem : Class -> Id -> MapPath -> Point -> Model -> (Model, Cmd Msg)
mouseDownOnItem class id mapPath pos model =
  (updateDragState (WaitForStartTime class id mapPath pos) model
    |> A.select id mapPath
  , Task.perform (Mouse << Mouse.Time) Time.now
  )


timeArrived : Posix -> UndoModel -> (UndoModel, Cmd Msg)
timeArrived time ({present} as undoModel) =
  case present.mouse.dragState of
    WaitForStartTime class id mapPath pos ->
      let
        dragState = DragEngaged time class id mapPath pos
      in
      (updateDragState dragState present, Cmd.none)
      |> A.swap undoModel
    WaitForEndTime startTime class id mapPath pos ->
      let
        delay = posixToMillis time - posixToMillis startTime > C.assocDelayMillis
        (dragMode, historyFunc) = if delay then (DraftAssoc, A.swap) else (DragTopic, A.push)
        maybeOrigPos = A.topicPos id (A.firstId mapPath) present.maps
        dragState =
          case class of
            "dmx-topic" ->
              case maybeOrigPos of
                Just origPos -> Drag dragMode id mapPath origPos pos Nothing
                Nothing -> NoDrag -- error is already logged
            _ -> NoDrag -- the error will be logged in performDrag
      in
      (updateDragState dragState present, Cmd.none)
      |> historyFunc undoModel
    _ ->
      logError "timeArrived" "Received \"Time\" message when dragState is not WaitForTime"
        (undoModel, Cmd.none)


mouseMove : Point -> Model -> (Model, Cmd Msg)
mouseMove pos model =
  case model.mouse.dragState of
    DragEngaged time class id mapPath pos_ ->
      ( updateDragState (WaitForEndTime time class id mapPath pos_) model
      , Task.perform (Mouse << Mouse.Time) Time.now
      )
    WaitForEndTime _ _ _ _ _ ->
      ( model, Cmd.none ) -- ignore -- TODO: can this happen at all? Is there a move listener?
    Drag _ _ _ _ _ _ ->
      ( performDrag pos model, Cmd.none )
    _ -> logError "mouseMove"
      ("Received \"Move\" message when dragState is " ++ toString model.mouse.dragState)
      ( model, Cmd.none )


performDrag : Point -> Model -> Model
performDrag pos model =
  case model.mouse.dragState of
    Drag dragMode id mapPath origPos lastPos target ->
      let
        delta = Point
          (pos.x - lastPos.x)
          (pos.y - lastPos.y)
        mapId = A.firstId mapPath
        newModel =
          case dragMode of
            DragTopic -> A.setTopicPosByDelta id mapId delta model
            DraftAssoc -> model
      in
      -- update lastPos
      updateDragState (Drag dragMode id mapPath origPos pos target) newModel
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
            _ = info "mouseUp" ("dropped " ++ fromInt id ++ " (map " ++ A.fromPath mapPath
              ++ ") on " ++ fromInt targetId ++ " (map " ++ A.fromPath targetMapPath ++ ") --> "
              ++ if notDroppedOnOwnMap then "move topic" else "abort")
            mapId = A.firstId mapPath
            notDroppedOnOwnMap = mapId /= targetId
            msg = MoveTopicToMap id mapId origPos targetId targetMapPath
          in
          if notDroppedOnOwnMap then
            (present, Random.generate msg point, A.swap)
          else
            (present, Cmd.none, A.swap)
        Drag DraftAssoc id mapPath _ _ (Just (targetId, targetMapPath)) ->
          let
            _ = info "mouseUp" ("assoc drawn from " ++ fromInt id ++ " (map " ++ A.fromPath
              mapPath ++ ") to " ++ fromInt targetId ++ " (map " ++ A.fromPath targetMapPath
              ++ ") --> " ++ if isSameMap then "create assoc" else "abort")
            mapId = A.firstId mapPath
            isSameMap = mapId == A.firstId targetMapPath
          in
          if isSameMap then
            (addDefaultAssocAndPutOnMap id targetId mapId present, Cmd.none, A.push)
          else
            (present, Cmd.none, A.swap)
        Drag _ _ _ _ _ _ ->
          let
            _ = info "mouseUp" "drag ended w/o target"
          in
          (present, Cmd.none, A.swap)
        DragEngaged _ _ _ _ _ ->
          let
            _ = info "mouseUp" "drag aborted w/o moving"
          in
          (present, Cmd.none, A.swap)
        _ ->
          logError "mouseUp"
            ("Received \"Up\" message when dragState is " ++ toString present.mouse.dragState)
            (present, Cmd.none, A.swap)
  in
  (updateDragState NoDrag model, cmd)
  |> storeWith
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


mouseOver : Class -> Id -> MapPath -> Model -> Model
mouseOver class targetId targetMapPath model =
  case model.mouse.dragState of
    Drag dragMode id mapPath origPos lastPos _ ->
      let
        isSelf = (id, A.firstId mapPath) == (targetId, A.firstId targetMapPath)
        isBox = A.hasMap targetId model.maps
        target =
          -- the hovered item is a potential drop target if
          -- 1. the hovered item is not the item being dragged (can't drop on self), and
          -- 2. the hovered item is a box
          if not isSelf && isBox then
            Just (targetId, targetMapPath)
          else
            Nothing
      in
      -- update target
      updateDragState (Drag dragMode id mapPath origPos lastPos target) model
    DragEngaged _ _ _ _ _ ->
      logError "mouseOver" "Received \"Over\" message when dragState is DragEngaged" model
    _ -> model


mouseOut : Class -> Id -> MapPath -> Model -> Model
mouseOut class targetId targetMapPath model =
  case model.mouse.dragState of
    Drag dragMode id mapPath origPos lastPos _ ->
      -- reset target
      updateDragState (Drag dragMode id mapPath origPos lastPos Nothing) model
    _ -> model


updateDragState : DragState -> Model -> Model
updateDragState dragState ({mouse} as model) =
  { model | mouse = { mouse | dragState = dragState }}


-- Presumption: both players exist in same map
addDefaultAssocAndPutOnMap : Id -> Id -> MapId -> Model -> Model
addDefaultAssocAndPutOnMap player1 player2 mapId model =
  addAssocAndPutOnMap
    "dmx.association"
    "dmx.default" player1
    "dmx.default" player2
    mapId model


-- Presumption: both players exist in same map
addAssocAndPutOnMap : ItemType -> RoleType -> Id -> RoleType -> Id -> MapId -> Model -> Model
addAssocAndPutOnMap itemType role1 player1 role2 player2 mapId model =
  let
    (newModel, assocId) = A.addAssoc itemType role1 player1 role2 player2 model
    props = MapAssoc AssocProps
  in
  A.putItemOnMap assocId props mapId newModel



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


mouseDecoder : (Class -> Id -> MapPath -> Mouse.Msg) -> D.Decoder Msg
mouseDecoder msg =
  D.map Mouse <| D.map3 msg U.classDecoder U.idDecoder U.pathDecoder
