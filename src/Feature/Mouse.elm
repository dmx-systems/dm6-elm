module Feature.Mouse exposing (update, clearHover, isHovered)

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
    (MouseDef.Move (pos, pointerType), DragInProgress topicId boxPath startPos) ->
      model
        |> updateHover pos ext
        |> ext.drag (Box.firstId boxPath) pos
        |> Undo.swap undoModel
    (MouseDef.Up, DragInProgress topicId boxPath startPos) ->
      model
        |> ext.dragStop (Box.firstId boxPath)
        |> \(model_, cmd) -> (model_ |> setDragState NoDrag, cmd)
        |> Undo.swap undoModel
    (MouseDef.Cancel, _) ->
      (undoModel, U.command <| Cancel Nothing)
    _ ->
      -- TODO: match no-op vs. error cases explicitly
      (undoModel, Cmd.none)


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


setDragState : DragState -> Model -> Model
setDragState dragState ({mouse} as model) =
  { model | mouse = { mouse | dragState = dragState }}


clearHover : Model -> Model
clearHover model =
  model
    |> setHover Nothing


setHover : Maybe Target -> Model -> Model
setHover hover ({mouse} as model) =
  { model | mouse = { mouse | hover = hover }}


isHovered : TopicId -> BoxPath -> Model -> Bool
isHovered topicId boxPath model =
  case model.mouse.hover of
    Just (T topicId_, boxPath_) ->
      topicId == topicId_ && boxPath == boxPath_
    _ -> False
