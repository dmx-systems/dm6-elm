module Feature.Sync exposing (createTopic)

import Feature.SyncDef exposing (SyncModel, refs)
import Model exposing (Model)
import ModelBase exposing (..)

import Crdt.Doc exposing (Doc)
import Crdt.Edit as Edit



createTopic : Topic -> Model -> Model
createTopic topic model =
  let
    (TopicId id) = topic.id
  in
  Edit.setKey refs.topics id topic model.sync.doc
    |> orKeep model.sync.doc
    |> setDoc model


--

orKeep : Doc SyncModel -> Result Edit.EditError (Doc SyncModel) -> Doc SyncModel
orKeep fallback result =
    Result.withDefault fallback result


setDoc : Model -> Doc SyncModel -> Model
setDoc ({sync} as model) doc =
  { model | sync = { sync | doc = doc }}
