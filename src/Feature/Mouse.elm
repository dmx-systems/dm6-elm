module Feature.Mouse exposing (update, setHover)

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
        |> setDragState (DragInProgress topicId (Box.firstId boxPath))
        |> ext.dragStart topicId boxPath pos pointerType
        |> Undo.swap undoModel
    (MouseDef.Move (pos, pointerType), NoDrag) ->
      model
        |> updateHover pos ext
        |> (\model_ -> Tuple.pair model_ Cmd.none)
        |> Undo.swap undoModel
    (MouseDef.Move (pos, pointerType), DragInProgress topicId boxId) ->
      model
        |> updateHover pos ext
        |> ext.drag boxId pos
        |> Undo.swap undoModel
    (MouseDef.Up, DragInProgress topicId boxId) ->
      model
        |> setDragState (MouseDef.NoDrag)
        |> ext.dragStop boxId
        |> Undo.swap undoModel
    (MouseDef.Cancel, _) ->
      (undoModel, U.command <| Cancel Nothing)
    _ ->
      -- Note: not an error
      (undoModel, Cmd.none)


updateHover : Point -> ExtManager -> Model -> Model
updateHover {x, y} ext model =
  let
    pos = Point x (y - C.appHeaderHeight)
    excludeTopicId =
      case model.mouse.dragState of
        DragInProgress topicId _ -> Just topicId
        _ -> Nothing
    maybeTarget = ext.hitTest model.boxId [] pos excludeTopicId model
  in
  model
    |> setHover maybeTarget


setDragState : DragState -> Model -> Model
setDragState dragState ({mouse} as model) =
  { model | mouse = { mouse | dragState = dragState }}


setHover : Maybe Target -> Model -> Model
setHover hover ({mouse} as model) =
  { model | mouse = { mouse | hover = hover }}
