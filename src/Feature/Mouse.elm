module Feature.Mouse exposing (update, setDragState)

import Env exposing (Env)
import Feature.MouseDef as MouseDef exposing (DragState(..))
import Model exposing (Model, Msg(..))
import Undo exposing (UndoModel)
import Utils as U



update : MouseDef.Msg -> Env -> (UndoModel, Cmd Msg)
update msg ({model, undoModel, ext} as env) =
  case (msg, model.mouse.dragState) of
    (MouseDef.Move (pos, pointerType), DragEngaged boxId) ->
      model
        |> ext.mouseMove boxId pos
        |> Undo.swap undoModel
    (MouseDef.Up, DragEngaged boxId) ->
      model
        |> setDragState (MouseDef.NoDrag)
        |> ext.mouseUp boxId
        |> Undo.swap undoModel
    (MouseDef.Cancel, _) ->
      (undoModel, U.command <| Cancel Nothing)
    _ ->
      -- Note: not an error
      (undoModel, Cmd.none)


setDragState : DragState -> Model -> Model
setDragState dragState ({mouse} as model) =
  { model | mouse = { mouse | dragState = dragState }}
