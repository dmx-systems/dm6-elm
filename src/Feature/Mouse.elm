module Feature.Mouse exposing (update, clearHover, isHovered, isDragActive, isTopicDragging,
  isDropTarget)

import Box
import Config as C
import Env exposing (Env, ExtManager)
import Feature.MouseDef as MouseDef exposing (DragSource)
import Model exposing (Model, Msg(..))
import ModelBase exposing (..)
import Outcome exposing (Outcome)
import Undo exposing (UndoModel)
import Utils as U



update : MouseDef.Msg -> Env -> (UndoModel, Cmd Msg)
update msg ({model, undoModel, ext} as env) =
  case (msg, model.mouse.dragSource) of
    (MouseDef.DownOnTopic topicId boxPath ixBoxPath (pos, pointerType), _) ->
      model
        |> setDragSource (Just (DragSource topicId boxPath ixBoxPath pos pos))
        |> emulateHover topicId boxPath ixBoxPath pointerType ext
        |> ext.dragStart
        |> Undo.swap undoModel
    (MouseDef.Move (pos, _), Nothing) ->
      model
        |> updateHover pos ext
        |> Model.with Cmd.none
        |> Undo.swap undoModel
    (MouseDef.Move (pos, _), Just dragSource) ->
      model
        |> updateHover pos ext
        |> ext.drag (Box.firstId dragSource.ixBoxPath) pos
        |> updateDropTarget pos ext
        |> Model.map (setDragSource (Just {dragSource | lastPointerPos = pos}))
        |> Undo.swap undoModel
    (MouseDef.Up, Just dragSource) ->
      model
        |> dragStop dragSource ext
        |> Outcome.map (setDragSource Nothing)
        |> Outcome.map (setDropTarget Nothing)
        |> Outcome.exec undoModel
    (MouseDef.Cancel, _) ->
      (undoModel, U.command <| Cancel Nothing)
    _ ->
      -- TODO: match no-op vs. error cases explicitly
      (undoModel, Cmd.none)


updateDropTarget : Point -> ExtManager -> (Model, Cmd Msg) -> (Model, Cmd Msg)
updateDropTarget clientPos ext (model, cmd) =
  case model.mouse.hover of
    Just {ixBoxPath} ->
      let
        (model_, dropTarget) = ext.updateDropTarget (Box.firstId ixBoxPath) clientPos model
      in
      ( model_
          |> setDropTarget dropTarget
      , cmd
      )
    Nothing -> (model, cmd)


dragStop : DragSource -> ExtManager -> Model -> Outcome
dragStop dragSource ext model =
  let
    maybeBoxId =
      case (model.mouse.hover, dragSource.ixBoxPath) of
        (Just {ixBoxPath}, _) ->
          Just (Box.firstId ixBoxPath)
        (Nothing, boxId :: _) ->
          Just boxId
        _ ->
          let
            _ = U.logError "Feature.Mouse.dragStop" "Unexpected drag state"
              (model.mouse.hover, dragSource.ixBoxPath)
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


-- Hover

emulateHover : TopicId -> BoxPath -> BoxPath -> PointerType -> ExtManager -> Model -> Model
emulateHover topicId boxPath ixBoxPath pointerType ext model =
  if pointerType == "touch" then
    model
      |> setHover (Just (BoxTarget ixBoxPath (T topicId, boxPath))) ext
  else
    model


{- Updates the geometrically hovered topic, utilizing hit-test.
The Point is in client coordinates.
-}
updateHover : Point -> ExtManager -> Model -> Model
updateHover ({y} as clientPos) ext model =
  let
    localPos = { clientPos | y = y - C.appHeaderHeight } -- local to fullscreen box
    maybeFilter =
      case model.mouse.dragSource of
        Just {topicId} -> Just topicId
        Nothing -> Nothing
    maybeHover = ext.hitTest model.boxId [] localPos maybeFilter model
  in
  model
    |> setHover maybeHover ext


clearHover : ExtManager -> Model -> Model
clearHover ext model =
  model
    |> setHover Nothing ext


setHover : Maybe BoxTarget -> ExtManager -> Model -> Model
setHover maybeHover ext model =
  model
    |> setDropTarget Nothing -- updates app state
    |> resetDropTarget ext   -- updates renderer states, calls hook
    |> setHover_ maybeHover


setHover_ : Maybe BoxTarget -> Model -> Model
setHover_ maybeHover ({mouse} as model) =
  { model | mouse = { mouse | hover = maybeHover }}


isHovered : TopicId -> BoxPath -> Model -> Bool
isHovered topicId boxPath model =
  case model.mouse.hover of
    Just {target} ->
      case target of
        (T topicId_, boxPath_) ->
          topicId == topicId_ && boxPath == boxPath_
        _ -> False
    _ -> False


-- Drag Source

setDragSource : Maybe DragSource -> Model -> Model
setDragSource dragSource ({mouse} as model) =
  { model | mouse = { mouse | dragSource = dragSource }}


isDragActive : Model -> Bool
isDragActive model =
  case model.mouse.dragSource of
    Just _ -> True
    Nothing -> False


isTopicDragging : TopicId -> BoxPath -> Model -> Bool
isTopicDragging topicId boxPath model =
  case model.mouse.dragSource of
    Just dragSource -> dragSource.topicId == topicId && dragSource.boxPath == boxPath
    Nothing -> False


-- Drop Target

isDropTarget : TopicId -> BoxPath -> Model -> Bool
isDropTarget topicId boxPath model =
  case model.mouse.dropTarget of
    Just target -> target == (T topicId, boxPath)
    Nothing -> False


setDropTarget : Maybe Target -> Model -> Model
setDropTarget dropTarget ({mouse} as model) =
  { model | mouse = { mouse | dropTarget = dropTarget }}


resetDropTarget : ExtManager -> Model -> Model
resetDropTarget ext model =
  case model.mouse.hover of
    Just {ixBoxPath} ->
      model
        |> ext.resetDropTarget (Box.firstId ixBoxPath)
    Nothing ->
      model
