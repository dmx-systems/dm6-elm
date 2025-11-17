module TextEditAPI exposing (update)

import AutoSize as Size
import Model exposing (Model, UndoModel, Msg(..))
import ModelAPI as A
import ModelHelper exposing (..)
import Storage as S
import Task
import Utils as U
-- feature modules
import TextEdit as T exposing (EditState(..))

import Browser.Dom as Dom
import String exposing (fromInt)



-- UPDATE


update : T.Msg -> UndoModel -> (UndoModel, Cmd Msg)
update msg ({present} as undoModel) =
  case msg of
    T.EditStart -> startEdit present |> A.push undoModel
    T.OnTextInput text -> onTextInput text present |> S.store |> A.swap undoModel
    T.OnTextareaInput text -> onTextareaInput text present |> S.storeWith |> A.swap undoModel
    T.SetTopicSize topicId boxId size ->
      ( present
        |> A.setTopicSize topicId boxId size
        |> Size.auto
      , Cmd.none
      )
      |> A.swap undoModel
    T.EditEnd ->
      (endEdit present, Cmd.none)
      |> A.swap undoModel


startEdit : Model -> (Model, Cmd Msg)
startEdit model =
  let
    newModel = case A.singleSelection model of
      Just (topicId, boxPath) ->
        model
        |> setEditState (ItemEdit topicId (A.firstId boxPath))
        |> setDetailDisplayIfMonade topicId (A.firstId boxPath)
        |> Size.auto
      Nothing -> model
  in
  (newModel, focus newModel)


setDetailDisplayIfMonade : Id -> BoxId -> Model -> Model
setDetailDisplayIfMonade topicId boxId model =
  model |> A.updateTopicProps topicId boxId
    (\props ->
      case props.displayMode of
        TopicD _ -> { props | displayMode = TopicD Detail }
        _ -> props
    )


onTextInput : String -> Model -> Model
onTextInput text model =
  case model.edit.state of
    ItemEdit topicId _ ->
      A.updateTopicInfo topicId
        (\topic -> { topic | text = text })
        model
    NoEdit -> U.logError "onTextInput" "called when edit.state is NoEdit" model


onTextareaInput : String -> Model -> (Model, Cmd Msg)
onTextareaInput text model =
  case model.edit.state of
    ItemEdit topicId boxId ->
      A.updateTopicInfo topicId
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
          Ok elem -> Edit
            (T.SetTopicSize topicId boxId
              (Size elem.element.width elem.element.height)
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
