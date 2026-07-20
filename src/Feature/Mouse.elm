module Feature.Mouse exposing (update, clearHover, isHovered, isDragActive, isTopicDragging,
  isDropTarget)

import Box
import Config as C
import Console
import Env exposing (Env, Dispatch)
import Feature.MouseDef as MouseDef exposing (DragSource)
import Model exposing (Model, Msg(..))
import ModelBase exposing (..)
import Outcome exposing (Outcome)
import TopicMap.Mouse



update : MouseDef.Msg -> Env -> Outcome
update msg ({model, dispatch} as env) =
  case (msg, model.mouse.dragSource) of
    (MouseDef.DownOnTopic topicId boxPath ixBoxPath (pos, pointerType), _) ->
      model
        |> setDragSource (Just (DragSource topicId boxPath ixBoxPath pos pos))
        |> emulateHover topicId boxPath ixBoxPath pointerType
        |> dispatch.dragStart
        |> Outcome.from
    (MouseDef.Move (pos, _), Nothing) ->
      model
        |> updateHover pos dispatch
        |> Outcome.default
    (MouseDef.Move (pos, _), Just dragSource) ->
      model
        |> updateHover pos dispatch
        |> dispatch.drag (Box.firstId dragSource.ixBoxPath) pos
        |> updateDropTarget pos dispatch
        |> Model.map (setDragSource (Just {dragSource | lastPointerPos = pos}))
        |> Outcome.from
    (MouseDef.Up, Just dragSource) ->
      env
        |> dragStop dragSource
        |> Outcome.map resetDragState
    (MouseDef.Cancel, _) ->
      env
        |> Env.outcomeCmd (Outcome.command <| Cancel Nothing)
    (MouseDef.Leave, _) ->
      -- TODO: explicit abort-drag semantics resp. rethink pointer-leaves-screen in general.
      -- Possibly introduce another hook to let modules react idiosyncratically, e.g. the
      -- TopicMap module might want persist changed topic geometry.
      env
        |> Env.map resetDragState
        |> Env.outcome
    _ ->
      -- TODO: match no-op vs. error cases explicitly
      env
        |> Env.outcome


updateDropTarget : Point -> Dispatch -> (Model, Cmd Msg) -> (Model, Cmd Msg)
updateDropTarget clientPos dispatch (model, cmd) =
  case model.mouse.hover of
    Just {ixBoxPath} ->
      let
        (model_, dropTarget) = dispatch.updateDropTarget (Box.firstId ixBoxPath) clientPos model
      in
      ( model_
          |> setDropTarget dropTarget
      , cmd
      )
    Nothing -> (model, cmd)


dragStop : DragSource -> Env -> Outcome
dragStop dragSource {model, dispatch} =
  let
    isDraftAssoc = TopicMap.Mouse.isDraftAssoc model
    sourceBoxId = Box.firstId dragSource.ixBoxPath
    stopBoxId =
      case (model.mouse.hover, isDraftAssoc) of
        (Just {ixBoxPath}, False) -> Box.firstId ixBoxPath
        _ -> sourceBoxId
  in
  model
    |> dispatch.dragStop stopBoxId


resetDragState : Model -> Model
resetDragState model =
  model
    |> setDragSource Nothing
    |> setDropTarget Nothing


-- Hover

emulateHover : TopicId -> BoxPath -> BoxPath -> PointerType -> Model -> Model
emulateHover topicId boxPath ixBoxPath pointerType model =
  if pointerType == "touch" then
    model
      |> setHover (Just (BoxTarget ixBoxPath (T topicId, boxPath)))
  else
    model


{- Updates the geometrically hovered topic, utilizing hit-test.
The Point is in client coordinates.
-}
updateHover : Point -> Dispatch -> Model -> Model
updateHover ({y} as clientPos) dispatch model =
  let
    localPos = { clientPos | y = y - C.appHeaderHeight } -- local to fullscreen box
    maybeFilter =
      case model.mouse.dragSource of
        Just {topicId} -> Just topicId
        Nothing -> Nothing
    maybeHover = dispatch.hitTest model.boxId [] localPos maybeFilter model
  in
  model
    |> setHover maybeHover


clearHover : Model -> Model
clearHover model =
  model
    |> setHover Nothing


setHover : Maybe BoxTarget -> Model -> Model
setHover maybeHover model =
  model
    |> setDropTarget Nothing
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
