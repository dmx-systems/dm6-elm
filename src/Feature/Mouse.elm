module Feature.Mouse exposing (update, isDragActive, isTopicDragging, clearHover, isHovered)

import Box
import Config as C
import Env exposing (Env, ExtManager)
import Feature.MouseDef as MouseDef exposing (DragState)
import Model exposing (Model, Msg(..))
import ModelBase exposing (..)
import Outcome exposing (Outcome)
import Undo exposing (UndoModel)
import Utils as U



update : MouseDef.Msg -> Env -> (UndoModel, Cmd Msg)
update msg ({model, undoModel, ext} as env) =
  case (msg, model.mouse.dragState) of
    (MouseDef.DownOnTopic topicId boxPath ixBoxPath (pos, pointerType), _) ->
      model
        |> setDragState (Just (DragState topicId boxPath ixBoxPath pos pos))
        |> emulateHover topicId boxPath (Box.firstId ixBoxPath) pointerType
        |> ext.dragStart
        |> Undo.swap undoModel
    (MouseDef.Move (pos, _), Nothing) ->
      model
        |> updateHover pos ext
        |> Model.with Cmd.none
        |> Undo.swap undoModel
    (MouseDef.Move (pos, _), Just dragState) ->
      model
        |> updateHover pos ext
        |> ext.drag (Box.firstId dragState.ixBoxPath) pos
        |> dragTargeting pos ext
        |> Model.map (setDragState (Just {dragState | lastPointerPos = pos}))
        |> Undo.swap undoModel
    (MouseDef.Up, Just dragState) ->
      model
        |> dragStop dragState ext
        |> Outcome.map (setDragState Nothing)
        |> Outcome.exec undoModel
    (MouseDef.Cancel, _) ->
      (undoModel, U.command <| Cancel Nothing)
    _ ->
      -- TODO: match no-op vs. error cases explicitly
      (undoModel, Cmd.none)


dragTargeting : Point -> ExtManager -> (Model, Cmd Msg) -> (Model, Cmd Msg)
dragTargeting clientPos ext ((model, _) as mct) =
  case model.mouse.hover of
    Just {ixBoxId} ->
      mct
        |> Model.map (ext.dragTargeting ixBoxId clientPos)
    Nothing -> mct


dragStop : DragState -> ExtManager -> Model -> Outcome
dragStop dragState ext model =
  let
    maybeBoxId =
      case (model.mouse.hover, dragState.ixBoxPath) of
        (Just {ixBoxId}, _) ->
          Just ixBoxId
        (Nothing, boxId :: _) ->
          Just boxId
        _ ->
          let
            _ = U.logError "Feature.Mouse.dragStop" "Unexpected drag state"
              (model.mouse.hover, dragState.ixBoxPath)
          in
          Nothing
  in
  case maybeBoxId of
    Just boxId ->
      model
        |> ext.dragStop boxId
    Nothing ->
      model
        |> Outcome.with Cmd.none


setDragState : Maybe DragState -> Model -> Model
setDragState dragState ({mouse} as model) =
  { model | mouse = { mouse | dragState = dragState }}


isDragActive : Model -> Bool
isDragActive model =
  case model.mouse.dragState of
    Just _ -> True
    Nothing -> False


isTopicDragging : TopicId -> BoxPath -> Model -> Bool
isTopicDragging topicId boxPath model =
  case model.mouse.dragState of
    Just dragState -> dragState.topicId == topicId && dragState.boxPath == boxPath
    Nothing -> False


emulateHover : TopicId -> BoxPath -> BoxId -> PointerType -> Model -> Model
emulateHover topicId boxPath ixBoxId pointerType model =
  if pointerType == "touch" then
    model
      |> setHover (Just (BoxTarget ixBoxId (T topicId, boxPath)))
  else
    model


{- Updates the geometrically hovered topic, utilizing hit-test.
The Point is in client coordinates.
-}
updateHover : Point -> ExtManager -> Model -> Model
updateHover ({y} as clientPos) ext model =
  let
    localPos = { clientPos | y = y - C.appHeaderHeight } -- local to fullscreen box
    excludeTopicId =
      case model.mouse.dragState of
        Just {topicId} -> Just topicId
        Nothing -> Nothing
    maybeTarget = ext.hitTest model.boxId [] localPos excludeTopicId model
  in
  model
    |> setHover maybeTarget


setHover : Maybe BoxTarget -> Model -> Model
setHover hover ({mouse} as model) =
  { model | mouse = { mouse | hover = hover }}


clearHover : Model -> Model
clearHover model =
  model
    |> setHover Nothing


isHovered : TopicId -> BoxPath -> Model -> Bool
isHovered topicId boxPath model =
  case model.mouse.hover of
    Just {target} ->
      case target of
        (T topicId_, boxPath_) ->
          topicId == topicId_ && boxPath == boxPath_
        _ -> False
    _ -> False
