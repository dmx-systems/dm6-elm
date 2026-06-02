module Feature.Mouse exposing (update, hasDragStarted, isDragging, clearHover, isHovered)

import Box
import Config as C
import Env exposing (Env, ExtManager)
import Feature.MouseDef as MouseDef exposing (DragState)
import Model exposing (Model, Msg(..))
import ModelBase exposing (..)
import Outcome
import Undo exposing (UndoModel)
import Utils as U



update : MouseDef.Msg -> Env -> (UndoModel, Cmd Msg)
update msg ({model, undoModel, ext} as env) =
  case (msg, model.mouse.dragState) of
    (MouseDef.DownOnTopic topicId boxPath ixBoxPath (pos, pointerType), _) ->
      model
        |> setDragState (Just (DragState topicId boxPath ixBoxPath pos))
        |> emulateHover topicId boxPath pointerType
        |> ext.dragStart
        |> Undo.swap undoModel
    (MouseDef.Move (pos, _), Nothing) ->
      model
        |> updateHover pos ext
        |> \model_ -> (model_, Cmd.none)
        |> Undo.swap undoModel
    (MouseDef.Move (pos, _), Just dragState) ->
      model
        |> updateHover pos ext
        |> ext.drag (Box.firstId dragState.ixBoxPath) pos
        |> Undo.swap undoModel
    (MouseDef.Up, Just dragState) ->
      model
        |> ext.dragStop (Box.firstId dragState.ixBoxPath)
        -- Note: typically extension's dragStop handler operates on DragStarted data,
        -- so we reset to NoDrag afterwards
        |> Outcome.map (setDragState Nothing)
        |> Outcome.exec undoModel
    (MouseDef.Cancel, _) ->
      (undoModel, U.command <| Cancel Nothing)
    _ ->
      -- TODO: match no-op vs. error cases explicitly
      (undoModel, Cmd.none)


setDragState : Maybe DragState -> Model -> Model
setDragState dragState ({mouse} as model) =
  { model | mouse = { mouse | dragState = dragState }}


hasDragStarted : Model -> Bool
hasDragStarted model =
  case model.mouse.dragState of
    Just _ -> True
    Nothing -> False


isDragging : TopicId -> BoxPath -> Model -> Bool
isDragging topicId boxPath model =
  case model.mouse.dragState of
    Just dragState -> dragState.topicId == topicId && dragState.boxPath == boxPath
    Nothing -> False


emulateHover : TopicId -> BoxPath -> PointerType -> Model -> Model
emulateHover topicId boxPath pointerType model =
  if pointerType == "touch" then
    model
      |> setHover (Just (T topicId, boxPath))
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
