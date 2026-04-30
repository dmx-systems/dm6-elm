module Assoc exposing (fromId, create, relatedTopics, otherTopicId)

import Model exposing (Model)
import ModelBase exposing (..)
import Topic
import Utils as U

import Dict



{-| Looks up an Assoc in Model.assocs.
Logs an error if Assoc is missing.
-}
fromId : AssocId -> Model -> Maybe Assoc
fromId assocId model =
  case model.assocs |> Dict.get (toAssocId assocId) of
    Just assoc -> Just assoc
    Nothing -> U.assocNotFound "Assoc.fromId" assocId Nothing


create : AssocType -> TopicId -> TopicId -> Model -> (Model, AssocId)
create assocType topicId1 topicId2 ({assocs} as model) =
  let
    id = AssocId model.nextId
    assoc = Assoc id assocType topicId1 topicId2
  in
  ( { model | assocs = assocs |> Dict.insert (toAssocId id) assoc }
      |> insertAssocId id topicId1
      |> insertAssocId id topicId2
      |> Model.nextId
  , id
  )


{-| Inserts an association ID into the item's set of association IDs.
No-op if the association ID is in the set already.
Logs an error if item does not exist.
Low-level API for maintaining the association ID set.
-}
insertAssocId : AssocId -> TopicId -> Model -> Model
insertAssocId assocId topicId model =
  model |> Topic.update topicId
    (\({assocIds} as topic) ->
      {topic | assocIds = assocId :: assocIds}
    )


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
    Nothing -> U.fail "Assoc.relatedTopics" {topicId = topicId} []


otherTopicId : AssocId -> TopicId -> Model -> Maybe TopicId
otherTopicId assocId topicId model =
  case fromId assocId model of
    Just {topicId1, topicId2} ->
      if topicId == topicId1 then
        Just topicId2
      else if topicId == topicId2 then
        Just topicId1
      else
        U.logError "Assoc.otherTopicId"
          (U.toString topicId ++ " is not connected by " ++ U.toString assocId) Nothing
    Nothing -> U.fail "Assoc.otherTopicId" {assocId = assocId, topicId = topicId} Nothing
