module Feature.Crdt exposing (model, initDoc, encodeDoc, setTopic, removeTopic)

import Console
import Feature.CrdtDef exposing (refs)
import Model exposing (Model)
import ModelBase exposing (..)

import Crdt.Doc as Doc exposing (Doc)
import Crdt.Edit as Edit

import Json.Encode as E



model : Model -> Maybe DocModel
model model_ =
  case model_.crdt.doc |> Doc.read of
    Ok docModel -> Just docModel
    Err err ->
      Console.logError "Feature.Crdt.model"
        ("Schema read error: " ++ Doc.readErrorToString err) Nothing


initDoc : E.Value -> Model -> Model
initDoc ops model_ =
  Doc.decodeInto ops model_.crdt.doc
    |> Result.map (setDoc model_)
    |> Result.map updateLastStored
    |> Result.mapError (Console.logError "Feature.Crdt.initDoc" "while decoding")
    |> Result.withDefault model_


encodeDoc : Model -> (Model, E.Value)
encodeDoc ({crdt} as model_) =
  ( model_ |> setLastStored (Doc.version crdt.doc)
  , crdt.doc |> Doc.encodeSince crdt.lastStored
  )


setTopic : Topic -> Model -> Model
setTopic topic model_ =
  let
    (TopicId id) = topic.id
    doc = model_.crdt.doc
  in
  Edit.setKey refs.topics id topic doc
    |> orKeep doc
    |> setDoc model_


removeTopic : TopicId -> Model -> Model
removeTopic (TopicId id) model_ =
  let
    doc = model_.crdt.doc
  in
  Edit.removeKey refs.topics id doc
    |> orKeep doc
    |> setDoc model_


--

orKeep : Doc DocModel -> Result Edit.EditError (Doc DocModel) -> Doc DocModel
orKeep fallback result =
    Result.withDefault fallback result


setDoc : Model -> Doc DocModel -> Model
setDoc ({crdt} as model_) doc =
  { model_ | crdt = { crdt | doc = doc }}


updateLastStored : Model -> Model
updateLastStored model_ =
  model_
    |> setLastStored (Doc.version model_.crdt.doc)


setLastStored : Doc.Version -> Model -> Model
setLastStored lastStored ({crdt} as model_) =
  { model_ | crdt = { crdt | lastStored = lastStored }}
