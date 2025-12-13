module Feature.TextEditAPI exposing (update, startEdit, isEdit, markdown)

import Box
import Box.Size as Size -- TODO: don't import, let caller do the sizing instead
import Feature.TextEdit as TextEdit exposing (EditState(..))
import Item
import Model exposing (Model, Msg(..))
import ModelParts exposing (..)
import Storage as S
import Task
import Undo exposing (UndoModel)
import Utils as U

import Browser.Dom as Dom
import Html exposing (Html, text)
import Markdown.Parser as Parser
import Markdown.Renderer as Renderer
import String exposing (fromInt)



-- UPDATE


update : TextEdit.Msg -> UndoModel -> (UndoModel, Cmd Msg)
update msg ({present} as undoModel) =
  case msg of
    TextEdit.OnTextInput text -> onTextInput text present |> S.store
      |> Undo.swap undoModel
    TextEdit.OnTextareaInput text -> onTextareaInput text present |> S.storeWith
      |> Undo.swap undoModel
    TextEdit.GotTextSize topicId sizeField size ->
      ( present
        |> Item.setTopicSize topicId sizeField size
        |> Size.auto
      , Cmd.none
      )
      |> Undo.swap undoModel
    TextEdit.EditEnd -> endEdit present |> Undo.swap undoModel


startEdit : Id -> BoxPath -> Model -> (Model, Cmd Msg)
startEdit topicId boxPath model =
  let
    newModel =
      model
      |> setEditState (ItemEdit topicId boxPath)
      |> switchTopicDisplay topicId (Box.firstId boxPath)
      |> Size.auto
  in
  (newModel, focus newModel)


endEdit : Model -> (Model, Cmd Msg)
endEdit model =
  case model.edit.state of
    ItemEdit topicId boxPath ->
      let
        elemId = Box.elemId "topic" topicId boxPath
      in
      ( model
        |> setEditState NoEdit
        |> Size.auto
      , measureElement elemId topicId View
      )
    NoEdit -> U.logError "endEdit" "called when edit.state is NoEdit" (model, Cmd.none)


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
  ( model |> setMeasureText text
  , measureElement "measure" topicId Editor
  )


measureElement : String -> Id -> SizeField -> Cmd Msg
measureElement elemId topicId sizeField =
  Dom.getElement elemId |> Task.attempt
    (\result ->
      case result of
        Ok {element} -> Edit
          (TextEdit.GotTextSize topicId sizeField
            (Size element.width element.height)
          )
        Err err -> U.logError "measureElement" (U.toString err) NoOp
    )


focus : Model -> Cmd Msg
focus model =
  let
    elemId =
      case model.edit.state of
        ItemEdit id boxPath -> Box.elemId "input" id boxPath
        NoEdit -> U.logError "focus" "called when edit.state is NoEdit" ""
  in
  Dom.focus elemId |> Task.attempt
    (\result ->
      case result of
        Ok () -> NoOp
        Err e -> U.logError "focus" (U.toString e) NoOp
    )


isEdit : Id -> BoxPath -> Model -> Bool
isEdit topicId boxPath model =
  model.edit.state == ItemEdit topicId boxPath


setEditState : EditState -> Model -> Model
setEditState state ({edit} as model) =
  { model | edit = { edit | state = state }}


setMeasureText : String -> Model -> Model
setMeasureText text ({edit} as model) =
  { model | edit = { edit | measureText = text }}



-- MARKDOWN


markdown : String -> List (Html Msg)
markdown source =
  source
  |> Parser.parse
  |> Result.withDefault []
  |> Renderer.render Renderer.defaultHtmlRenderer
  |> Result.withDefault [ text "Markdown Problem!" ]
