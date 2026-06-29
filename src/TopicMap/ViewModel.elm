module TopicMap.ViewModel exposing (topicsToRender, isLimboTopic, isLimboAssoc, limboState)

import Box
import Feature.SearchDef exposing (SearchResult(..))
import Model exposing (Model)
import ModelBase exposing (..)
import TopicMap.TopicMap as TM
import TopicMap.TopicMapDef exposing (TopicMap, MapTopic)
import Utils as U



{- Projects box data and search state ("limbo") into a TopicMap render model -}
topicsToRender : TopicMap -> Model -> List MapTopic
topicsToRender topicMap model =
  let
    topics = TM.allMapTopics topicMap model |> List.map
      (\topic -> effectiveExpansion topic topicMap.id model)
    limboTopic = limboMapTopic topicMap model
  in
  topics ++ limboTopic


effectiveExpansion : MapTopic -> BoxId -> Model -> MapTopic
effectiveExpansion topic boxId model =
  { topic | expansion =
    if isLimboTopic topic.id boxId model then
      Expanded
    else
      Box.expansionOf topic.id boxId model
  }


limboMapTopic : TopicMap -> Model -> List MapTopic
limboMapTopic topicMap model =
  case limboState model of
    Just (topicId, _, limboBoxId) ->
      if limboBoxId == topicMap.id && (not <| Box.hasItem (T topicId) topicMap.id model) then
        let
          _ = U.info "TopicMap.ViewModel.limboMapTopic"
            (topicId, "not in topicMap", topicMap.id)
          mapTopic =
            case TM.mapTopicOrNothing topicId topicMap of
              Just {pos} -> MapTopic topicId pos Expanded
              Nothing -> TM.initLimboMapTopic topicId topicMap.id model
        in
        [ mapTopic ]
      else
        []
    Nothing -> []


isLimboTopic : TopicId -> BoxId -> Model -> Bool
isLimboTopic topicId boxId model =
  case limboState model of
    Just (topicId_, _, boxId_) -> topicId == topicId_ && boxId == boxId_
    Nothing -> False


isLimboAssoc : AssocId -> BoxId -> Model -> Bool
isLimboAssoc assocId boxId model =
  case limboState model of
    Just (_, Just assocId_, boxId_) -> assocId == assocId_ && boxId == boxId_
    _ -> False


limboState : Model -> Maybe (TopicId, Maybe AssocId, BoxId)
limboState model =
  case TM.revelationBoxId model of
    Just boxId ->
      case model.search.result of
        Topics _ (Just topicId) -> Just (topicId, Nothing, boxId)
        RelTopics _ (Just (topicId, assocId)) -> Just (topicId, Just assocId, boxId)
        _ -> Nothing
    Nothing -> Nothing
