module Assoc exposing (fromId, create, relatedTopics, otherTopicId)

import Console
import Feature.Crdt as Crdt
import Feature.Id as Id
import Model exposing (Model)
import ModelBase exposing (..)
import Topic

import Dict



{-| Looks up an Assoc in Model.assocs.
Logs an error if Assoc is missing.
-}
fromId : AssocId -> Model -> Maybe Assoc
fromId (AssocId id) model =
  case Crdt.docModel model of
    Just docModel ->
      case docModel.assocs |> Dict.get id of
        Just assoc -> Just assoc
        Nothing -> Console.logError "Assoc.fromId" (id ++ " not found") Nothing
    Nothing ->
      Console.fail "Assoc.fromId" id Nothing


create : AssocType -> TopicId -> TopicId -> Model -> (Model, AssocId)
create assocType topicId1 topicId2 model =
  let
    (id, model_) = Id.new model
    assocId = AssocId id
    assoc = Assoc assocId assocType topicId1 topicId2
  in
  (Crdt.createAssoc assoc model_, assocId)


relatedTopics : TopicId -> Model -> List (TopicId, AssocId)
relatedTopics topicId model =
  case Topic.fromId topicId model of
    Just topic ->
      topic.assocIds |> List.foldr
        (\assocId acc ->
          case otherTopicId assocId topicId model of
            Just id -> (id, assocId) :: acc
            Nothing -> acc
        )
        []
    Nothing -> Console.fail "Assoc.relatedTopics" {topicId = topicId} []


otherTopicId : AssocId -> TopicId -> Model -> Maybe TopicId
otherTopicId assocId topicId model =
  case fromId assocId model of
    Just {topicId1, topicId2} ->
      if topicId == topicId1 then
        Just topicId2
      else if topicId == topicId2 then
        Just topicId1
      else
        Console.logError "Assoc.otherTopicId" (Console.toString topicId
          ++ " is not connected by " ++ Console.toString assocId) Nothing
    Nothing -> Console.fail "Assoc.otherTopicId" {assocId = assocId, topicId = topicId} Nothing
