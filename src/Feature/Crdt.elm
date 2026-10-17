module Feature.Crdt exposing (doc, initDoc, encodeDoc, setTopic, removeTopic, recordSingle,
  undo, redo, canUndo, canRedo)

import Console
import Feature.CrdtDef exposing (refs)
import Model exposing (Model)
import ModelBase exposing (..)

import Crdt.Doc as Doc exposing (Doc)
import Crdt.Edit as Edit

import Json.Encode as E



doc : Model -> Maybe DocModel
doc model =
  case Doc.read model.crdt.doc of
    Ok doc_ -> Just doc_
    Err err ->
      Console.logError "Feature.Crdt.doc"
        ("Schema read error: " ++ Doc.readErrorToString err) Nothing


initDoc : E.Value -> Model -> Model
initDoc ops model =
  Doc.decodeInto ops model.crdt.doc
    |> Result.map (setDoc model)
    |> Result.map updateLastStored
    |> Result.mapError (Console.logError "Feature.Crdt.initDoc" "while decoding")
    |> Result.withDefault model


encodeDoc : Model -> (Model, E.Value)
encodeDoc ({crdt} as model) =
  ( model |> setLastStored (Doc.version crdt.doc)
  , crdt.doc |> Doc.encodeSince crdt.lastStored
  )


setTopic : Topic -> Model -> Model
setTopic topic model =
  let
    (TopicId id) = topic.id
    doc_ = model.crdt.doc
  in
  Edit.setKey refs.topics id topic doc_
    |> orKeep doc_
    |> setDoc model


removeTopic : TopicId -> Model -> Model
removeTopic (TopicId id) model =
  let
    doc_ = model.crdt.doc
  in
  Edit.removeKey refs.topics id doc_
    |> orKeep doc_
    |> setDoc model



-- UNDO / REDO


recordSingle : Model -> Model -> Model
recordSingle oldModel model =
  Doc.recordEdit (Doc.version oldModel.crdt.doc) model.crdt.doc
    |> setDoc model


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

orKeep : Doc DocModel -> Result Edit.EditError (Doc DocModel) -> Doc DocModel
orKeep fallback result =
    Result.withDefault fallback result


setDoc : Model -> Doc DocModel -> Model
setDoc ({crdt} as model) doc_ =
  { model | crdt = { crdt | doc = doc_ }}


updateLastStored : Model -> Model
updateLastStored model =
  model
    |> setLastStored (Doc.version model.crdt.doc)


setLastStored : Doc.Version -> Model -> Model
setLastStored lastStored ({crdt} as model) =
  { model | crdt = { crdt | lastStored = lastStored }}
