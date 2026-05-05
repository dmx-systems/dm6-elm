module Feature.Mouse exposing (update)

import Box
import Env exposing (Env)
import Feature.MouseDef as MouseDef exposing (DragState(..))
import Model exposing (Model, Msg(..))
import Undo exposing (UndoModel)
import Utils as U



update : MouseDef.Msg -> Env -> (UndoModel, Cmd Msg)
update msg ({model, undoModel, ext} as env) =
  case (msg, model.mouse.dragState) of
    (MouseDef.DragStart topicId boxPath (pos, pointerType), _) ->
      model
        |> setDragState (DragInProgress (Box.firstId boxPath))
        |> ext.dragStart topicId boxPath pos pointerType
        |> Undo.swap undoModel
    (MouseDef.Drag (pos, pointerType), DragInProgress boxId) ->
      model
        |> ext.drag boxId pos
        |> Undo.swap undoModel
    (MouseDef.DragStop, DragInProgress boxId) ->
      model
        |> setDragState (MouseDef.NoDrag)
        |> ext.dragStop boxId
        |> Undo.swap undoModel
    (MouseDef.Cancel, _) ->
      (undoModel, U.command <| Cancel Nothing)
    _ ->
      -- Note: not an error
      (undoModel, Cmd.none)


setDragState : DragState -> Model -> Model
setDragState dragState ({mouse} as model) =
  { model | mouse = { mouse | dragState = dragState }}
