module Feature.Sync exposing (model, setTopic, removeTopic)

import Console
import Feature.SyncDef exposing (refs)
import Model exposing (Model)
import ModelBase exposing (..)

import Crdt.Doc as Doc exposing (Doc)
import Crdt.Edit as Edit



model : Model -> Maybe SyncModel
model model_ =
  case model_.sync.doc |> Doc.read of
    Ok syncModel -> Just syncModel
    Err err ->
      Console.logError "Feature.Sync.model"
        ("Schema read error: " ++ Doc.readErrorToString err) Nothing


setTopic : Topic -> Model -> Model
setTopic topic model_ =
  let
    (TopicId id) = topic.id
    doc = model_.sync.doc
  in
  Edit.setKey refs.topics id topic doc
    |> orKeep doc
    |> setDoc model_


removeTopic : TopicId -> Model -> Model
removeTopic (TopicId id) model_ =
  let
    doc = model_.sync.doc
  in
  Edit.removeKey refs.topics id doc
    |> orKeep doc
    |> setDoc model_


--

orKeep : Doc SyncModel -> Result Edit.EditError (Doc SyncModel) -> Doc SyncModel
orKeep fallback result =
    Result.withDefault fallback result


setDoc : Model -> Doc SyncModel -> Model
setDoc ({sync} as model_) doc =
  { model_ | sync = { sync | doc = doc }}
