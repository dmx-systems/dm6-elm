module Feature.TextEditAPI exposing (update, startEdit, isEdit)

import Box
import Box.Size as Size
import Feature.TextEdit as TextEdit exposing (EditState(..))
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


update : TextEdit.Msg -> UndoModel -> (UndoModel, Cmd Msg)
update msg ({present} as undoModel) =
  case msg of
    TextEdit.OnTextInput text -> onTextInput text present |> S.store
      |> Undo.swap undoModel
    TextEdit.OnTextareaInput text -> onTextareaInput text present |> S.storeWith
      |> Undo.swap undoModel
    TextEdit.GotTopicSize topicId size ->
      ( present
        |> Item.setTopicSize topicId size
        |> Size.auto
      , Cmd.none
      )
      |> Undo.swap undoModel
    TextEdit.EditEnd ->
      (endEdit present, Cmd.none)
      |> Undo.swap undoModel


startEdit : Id -> BoxId -> Model -> (Model, Cmd Msg)
startEdit topicId boxId model =
  let
    newModel =
      model
      |> setEditState (ItemEdit topicId boxId)
      |> switchTopicDisplay topicId boxId
      |> Size.auto
  in
  (newModel, focus newModel)


endEdit : Model -> Model
endEdit model =
  model
  |> setEditState NoEdit
  |> Size.auto


switchTopicDisplay : Id -> BoxId -> Model -> Model
switchTopicDisplay topicId boxId model =
  model
  |> Box.updateDisplayMode topicId boxId
    (\displayMode ->
      case displayMode of
        TopicD _ -> TopicD Detail
        _ -> displayMode
    )


onTextInput : String -> Model -> Model
onTextInput text model =
  case model.edit.state of
    ItemEdit topicId _ ->
      Item.updateTopic topicId
        (\topic -> { topic | text = text })
        model
    NoEdit -> U.logError "onTextInput" "called when edit.state is NoEdit" model


onTextareaInput : String -> Model -> (Model, Cmd Msg)
onTextareaInput text model =
  case model.edit.state of
    ItemEdit topicId _ ->
      Item.updateTopic topicId
        (\topic -> { topic | text = text })
        model
      |> measureText text topicId
    NoEdit -> U.logError "onTextareaInput" "called when edit.state is NoEdit" (model, Cmd.none)


measureText : String -> Id -> Model -> (Model, Cmd Msg)
measureText text topicId model =
  ( model
    |> setMeasureText text
  , Dom.getElement "measure"
    |> Task.attempt
      (\result ->
        case result of
          Ok {element} -> Edit
            (TextEdit.GotTopicSize topicId
              (Size element.width element.height)
            )
          Err err -> U.logError "measureText" (U.toString err) NoOp
      )
  )


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


isEdit : Id -> BoxId -> Model -> Bool
isEdit topicId boxId model =
  model.edit.state == ItemEdit topicId boxId


setEditState : EditState -> Model -> Model
setEditState state ({edit} as model) =
  { model | edit = { edit | state = state }}


setMeasureText : String -> Model -> Model
setMeasureText text ({edit} as model) =
  { model | edit = { edit | measureText = text }}
