module Feature.Crdt exposing (docModel, initDoc, encodeDoc, addTopic, setTopicIcon, setTopicText,
  setTopicViewSize, setTopicEditorSize, addAssocId, removeAssocId, removeTopic, markSingleEdit,
  markEditStart, markEditEnd, undo, redo, canUndo, canRedo)

import Console
import Feature.CrdtDef exposing (refs, topicIconRef, topicTextRef, topicViewSizeRef,
  topicEditorSizeRef, topicAssocIdsRef)
import Model exposing (Model)
import ModelBase exposing (..)

import Crdt.Doc as Doc exposing (Doc, Version)
import Crdt.Edit as Edit

import Json.Encode as E



docModel : Model -> Maybe DocModel
docModel model =
  case Doc.read model.crdt.doc of
    Ok docModel_ -> Just docModel_
    Err err ->
      let
        _ = Console.logError "Feature.Crdt.doc" "Schema read error" (Doc.readErrorToString err)
      in
      Nothing


initDoc : E.Value -> Model -> Model
initDoc ops model =
  Doc.decodeInto ops model.crdt.doc
    |> Result.map (setDoc model)
    |> Result.map updateLastStored
    |> Result.mapError (Console.logError "Feature.Crdt.initDoc" "Decoding failed")
    |> Result.withDefault model


encodeDoc : Model -> (Model, E.Value)
encodeDoc ({crdt} as model) =
  ( model |> setLastStored (Doc.version crdt.doc)
  , crdt.doc |> Doc.encodeSince crdt.lastStored
  )


-- Topic

addTopic : Topic -> Model -> Model
addTopic topic model =
  Edit.setKey refs.topics (toTopicId topic.id) topic model.crdt.doc
    |> editResult model


setTopicIcon : TopicId -> Maybe Icon -> Model -> Model
setTopicIcon (TopicId id) icon model =
  Edit.set (topicIconRef id) icon model.crdt.doc
    |> editResult model


setTopicText : TopicId -> String -> Model -> Model
setTopicText (TopicId id) text model =
  Edit.set (topicTextRef id) text model.crdt.doc
    |> editResult model


setTopicViewSize : TopicId -> Size -> Model -> Model
setTopicViewSize (TopicId id) size model =
  Edit.set (topicViewSizeRef id) size model.crdt.doc
    |> editResult model


setTopicEditorSize : TopicId -> Size -> Model -> Model
setTopicEditorSize (TopicId id) size model =
  Edit.set (topicEditorSizeRef id) size model.crdt.doc
    |> editResult model


addAssocId : AssocId -> TopicId -> Model -> Model
addAssocId assocId (TopicId topicId) model =
  Edit.append (topicAssocIdsRef topicId) assocId model.crdt.doc
    |> editResult model


removeAssocId : Int -> TopicId -> Model -> Model
removeAssocId index (TopicId topicId) model =
  Edit.remove (topicAssocIdsRef topicId) index model.crdt.doc
    |> editResult model


removeTopic : TopicId -> Model -> Model
removeTopic (TopicId id) model =
  Edit.removeKey refs.topics id model.crdt.doc
    |> editResult model



-- UNDO / REDO


markSingleEdit : Model -> Model -> Model
markSingleEdit oldModel model =
  Doc.recordEdit (Doc.version oldModel.crdt.doc) model.crdt.doc
    |> setDoc model


markEditStart : Model -> Model -> Model
markEditStart oldModel model =
  model
    |> setEditStartVersion (Just (Doc.version oldModel.crdt.doc))


markEditEnd : Model -> Model
markEditEnd model =
  case model.crdt.editStartVersion of
    Just version ->
      Doc.recordEdit version model.crdt.doc
        |> setDoc model
        |> setEditStartVersion Nothing
    Nothing ->
      let
        _ = Console.logError "Feature.Crdt.markEditEnd" "Edit start is not marked" Nothing
      in
      model


undo : Model -> Model
undo model =
  Doc.undo model.crdt.doc
    |> setDoc model


redo : Model -> Model
redo model =
  Doc.redo model.crdt.doc
    |> setDoc model


canUndo : Model -> Bool
canUndo model =
  Doc.canUndo model.crdt.doc


canRedo : Model -> Bool
canRedo model =
  Doc.canRedo model.crdt.doc



-- LOCAL HELPER


editResult : Model -> Result Edit.EditError (Doc DocModel) -> Model
editResult model result =
  case result of
    Ok doc -> doc |> setDoc model
    Err err ->
      let
        _ = Console.logError "Feature.Crdt.editResult" "Edit failed"
          (Edit.editErrorToString err)
      in
      model


setDoc : Model -> Doc DocModel -> Model
setDoc ({crdt} as model) doc =
  { model | crdt = { crdt | doc = doc }}


updateLastStored : Model -> Model
updateLastStored model =
  model
    |> setLastStored (Doc.version model.crdt.doc)


setLastStored : Version -> Model -> Model
setLastStored lastStored ({crdt} as model) =
  { model | crdt = { crdt | lastStored = lastStored }}


setEditStartVersion : Maybe Version -> Model -> Model
setEditStartVersion editStartVersion ({crdt} as model) =
  { model | crdt = { crdt | editStartVersion = editStartVersion }}
