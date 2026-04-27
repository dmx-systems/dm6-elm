module Assoc exposing (fromId, create, relatedTopics, otherTopicId)

import Model exposing (Model)
import ModelBase exposing (..)
import Topic
import Utils as U

import Dict
import String exposing (fromInt)



{-| Looks up an Assoc in Model.assocs.
Logs an error if Assoc is missing.
-}
fromId : Id -> Model -> Maybe Assoc
fromId assocId model =
  case model.assocs |> Dict.get assocId of
    Just assoc -> Just assoc
    Nothing -> U.assocNotFound "Assoc.fromId" assocId Nothing


create : AssocType -> Id -> Id -> Model -> (Model, Id)
create assocType topicId1 topicId2 ({assocs} as model) =
  let
    id = model.nextId
    assoc = Assoc id assocType topicId1 topicId2
  in
  ( { model | assocs = assocs |> Dict.insert id assoc }
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
insertAssocId : Id -> Id -> Model -> Model
insertAssocId assocId topicId model =
  model |> Topic.update topicId
    (\({assocIds} as topic) ->
      {topic | assocIds = assocId :: assocIds}
    )


-- Result is (topic ID, assoc ID)
relatedTopics : Id -> Model -> List (Id, Id)
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


otherTopicId : Id -> Id -> Model -> Maybe Id
otherTopicId assocId topicId model =
  case fromId assocId model of
    Just {topicId1, topicId2} ->
      if topicId == topicId1 then
        Just topicId2
      else if topicId == topicId2 then
        Just topicId1
      else
        U.logError "Assoc.otherTopicId"
          ("Topic " ++ fromInt topicId ++ " is not connected by Assoc " ++ fromInt assocId)
          Nothing
    Nothing -> U.fail "Assoc.otherTopicId" {assocId = assocId, topicId = topicId} Nothing
