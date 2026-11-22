module Assoc exposing (lookup, create, relatedTopics, otherTopicId)

import Console
import Feature.Crdt as Crdt
import Feature.Id as Id
import Model exposing (Model)
import ModelBase exposing (..)
import Topic

import Dict



{-| Looks up an Assoc in cache.
Logs an error if Assoc is missing.
-}
lookup : AssocId -> Model -> Maybe Assoc
lookup (AssocId id) model =
  case Dict.get id model.cache.assocs of
    Just assoc ->
      Just assoc
    Nothing ->
      Console.logError "Assoc.lookup" ("Missing assoc " ++ id) Nothing


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
  case Topic.lookup topicId model of
    Just topic ->
      topic.assocIds |> List.filterMap
        (\assocId ->
          model
            |> otherTopicId assocId topicId
            |> Maybe.map (\id -> (id, assocId))
        )
    Nothing ->
      Console.fail "Assoc.relatedTopics" topicId []


otherTopicId : AssocId -> TopicId -> Model -> Maybe TopicId
otherTopicId assocId topicId model =
  case lookup assocId model of
    Just {topicId1, topicId2} ->
      if topicId == topicId1 then
        Just topicId2
      else if topicId == topicId2 then
        Just topicId1
      else
        Console.logError "Assoc.otherTopicId" (Console.toString topicId
          ++ " is not connected by " ++ Console.toString assocId) Nothing
    Nothing ->
      Console.fail "Assoc.otherTopicId" (assocId, topicId) Nothing
