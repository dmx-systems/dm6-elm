module Feature.TextEditAPI exposing (update)

import Box
import Box.Size as Size
import Feature.SelAPI as SelAPI
import Feature.TextEdit as T exposing (EditState(..))
import Item
import Model exposing (Model, Msg(..))
import ModelParts exposing (..)
import Storage as S
import Task
import Undo exposing (UndoModel)
import Utils as U

import Browser.Dom as Dom
import String exposing (fromInt)



-- UPDATE


update : T.Msg -> UndoModel -> (UndoModel, Cmd Msg)
update msg ({present} as undoModel) =
  case msg of
    T.EditStart -> startEdit present |> Undo.push undoModel
    T.OnTextInput text -> onTextInput text present |> S.store |> Undo.swap undoModel
    T.OnTextareaInput text -> onTextareaInput text present |> S.storeWith |> Undo.swap undoModel
    T.SetTopicSize topicId boxId size ->
      ( present
        |> Box.setTopicSize topicId boxId size
        |> Size.auto
      , Cmd.none
      )
      |> Undo.swap undoModel
    T.EditEnd ->
      (endEdit present, Cmd.none)
      |> Undo.swap undoModel


startEdit : Model -> (Model, Cmd Msg)
startEdit model =
  let
    newModel = case SelAPI.single model of
      Just (topicId, boxPath) ->
        model
        |> setEditState (ItemEdit topicId (Box.firstId boxPath))
        |> setDetailDisplayIfMonade topicId (Box.firstId boxPath)
        |> Size.auto
      Nothing -> model
  in
  (newModel, focus newModel)


setDetailDisplayIfMonade : Id -> BoxId -> Model -> Model
setDetailDisplayIfMonade topicId boxId model =
  model |> Box.updateTopicProps topicId boxId
    (\props ->
      case props.displayMode of
        TopicD _ -> { props | displayMode = TopicD Detail }
        _ -> props
    )


onTextInput : String -> Model -> Model
onTextInput text model =
  case model.edit.state of
    ItemEdit topicId _ ->
      Item.updateTopicInfo topicId
        (\topic -> { topic | text = text })
        model
    NoEdit -> U.logError "onTextInput" "called when edit.state is NoEdit" model


onTextareaInput : String -> Model -> (Model, Cmd Msg)
onTextareaInput text model =
  case model.edit.state of
    ItemEdit topicId boxId ->
      Item.updateTopicInfo topicId
        (\topic -> { topic | text = text })
        model
      |> measureText text topicId boxId
    NoEdit -> U.logError "onTextareaInput" "called when edit.state is NoEdit" (model, Cmd.none)


measureText : String -> Id -> BoxId -> Model -> (Model, Cmd Msg)
measureText text topicId boxId model =
  ( model
    |> setMeasureText text
  , Dom.getElement "measure"
    |> Task.attempt
      (\result ->
        case result of
          Ok {element} -> Edit
            (T.SetTopicSize topicId boxId
              (Size element.width element.height)
            )
          Err err -> U.logError "measureText" (U.toString err) NoOp
      )
  )


endEdit : Model -> Model
endEdit model =
  model
  |> setEditState NoEdit
  |> Size.auto


focus : Model -> Cmd Msg
focus model =
  let
    nodeId =
      case model.edit.state of
        ItemEdit id boxId -> "dmx-input-" ++ fromInt id ++ "-" ++ fromInt boxId
        NoEdit -> U.logError "focus" "called when edit.state is NoEdit" ""
  in
  Dom.focus nodeId |> Task.attempt
    (\result ->
      case result of
        Ok () -> NoOp
        Err e -> U.logError "focus" (U.toString e) NoOp
    )


setEditState : EditState -> Model -> Model
setEditState state ({edit} as model) =
  { model | edit = { edit | state = state }}


setMeasureText : String -> Model -> Model
setMeasureText text ({edit} as model) =
  { model | edit = { edit | measureText = text }}
