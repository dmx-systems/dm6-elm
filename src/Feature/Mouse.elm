module Feature.Mouse exposing (update, isDragInProgress, isDragging, clearHover, isHovered)

import Box
import Config as C
import Env exposing (Env, ExtManager)
import Feature.MouseDef as MouseDef exposing (DragState(..))
import Model exposing (Model, Msg(..))
import ModelBase exposing (..)
import Undo exposing (UndoModel)
import Utils as U



update : MouseDef.Msg -> Env -> (UndoModel, Cmd Msg)
update msg ({model, undoModel, ext} as env) =
  case (msg, model.mouse.dragState) of
    (MouseDef.DragStart topicId boxPath (pos, pointerType), _) ->
      model
        |> setDragState (DragInProgress topicId boxPath pos)
        |> emulateHover topicId boxPath pointerType
        |> ext.dragStart topicId boxPath pos pointerType
        |> Undo.swap undoModel
    (MouseDef.Move (pos, pointerType), NoDrag) ->
      model
        |> updateHover pos ext
        |> \model_ -> (model_, Cmd.none)
        |> Undo.swap undoModel
    (MouseDef.Move (pos, pointerType), DragInProgress topicId (boxId :: _) startPos) ->
      model
        |> updateHover pos ext
        |> ext.drag boxId pos
        |> Undo.swap undoModel
    (MouseDef.Up, DragInProgress topicId (boxId :: _) startPos) ->
      model
        |> ext.dragStop boxId
        |> \(model_, cmd) -> (model_ |> setDragState NoDrag, cmd)
        |> Undo.swap undoModel
    (MouseDef.Cancel, _) ->
      (undoModel, U.command <| Cancel Nothing)
    _ ->
      -- TODO: match no-op vs. error cases explicitly
      (undoModel, Cmd.none)


setDragState : DragState -> Model -> Model
setDragState dragState ({mouse} as model) =
  { model | mouse = { mouse | dragState = dragState }}


isDragInProgress : Model -> Bool
isDragInProgress model =
  case model.mouse.dragState of
    DragInProgress _ _ _ -> True
    NoDrag -> False


isDragging : TopicId -> BoxPath -> Model -> Bool
isDragging topicId boxPath model =
  case model.mouse.dragState of
    DragInProgress topicId_ boxPath_ _ -> topicId_ == topicId && boxPath_ == boxPath
    NoDrag -> False


emulateHover : TopicId -> BoxPath -> PointerType -> Model -> Model
emulateHover topicId boxPath pointerType model =
  if pointerType == "touch" then
    model
      |> setHover (Just (T topicId, boxPath))
  else
    model


updateHover : Point -> ExtManager -> Model -> Model
updateHover {x, y} ext model =
  let
    pos = Point x (y - C.appHeaderHeight)
    excludeTopicId =
      case model.mouse.dragState of
        DragInProgress topicId _ _ -> Just topicId
        _ -> Nothing
    maybeTarget = ext.hitTest model.boxId [] pos excludeTopicId model
  in
  model
    |> setHover maybeTarget


setHover : Maybe Target -> Model -> Model
setHover hover ({mouse} as model) =
  { model | mouse = { mouse | hover = hover }}


clearHover : Model -> Model
clearHover model =
  model
    |> setHover Nothing


isHovered : TopicId -> BoxPath -> Model -> Bool
isHovered topicId boxPath model =
  case model.mouse.hover of
    Just (T topicId_, boxPath_) ->
      topicId == topicId_ && boxPath == boxPath_
    _ -> False
