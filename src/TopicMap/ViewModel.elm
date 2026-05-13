module TopicMap.ViewModel exposing (topicsToRender, isLimboTopic, isLimboAssoc, limboState)

import Box
import Feature.SearchDef exposing (SearchResult(..))
import Model exposing (Model)
import ModelBase exposing (..)
import TopicMap.TopicMapDef exposing (BoxProps, TopicProps)
import TopicMap.BoxProps as TM
import Utils as U



{- Projects box data and search state ("limbo") into a TopicMap render model -}
topicsToRender : BoxProps -> Model -> List TopicProps
topicsToRender map model =
  let
    topics = TM.topics map model |> List.map
      (\topic -> effectiveExpansion topic map.id model)
    limboTopic = limboMapTopic map model
  in
  topics ++ limboTopic


effectiveExpansion : TopicProps -> BoxId -> Model -> TopicProps
effectiveExpansion topic boxId model =
  { topic | expansion =
    if isLimboTopic topic.id boxId model then
      Expanded
    else
      Box.expansionOf topic.id boxId model
  }


limboMapTopic : BoxProps -> Model -> List TopicProps
limboMapTopic map model =
  case limboState model of
    Just (topicId, _, limboBoxId) ->
      if limboBoxId == map.id && (not <| Box.hasItem (T topicId) map.id model) then
        let
          _ = U.info "TopicMap.ViewModel.limboMapTopic" (topicId, "not in map", map.id)
          mapTopic =
            case TM.mapTopicOrNothing topicId map of
              Just {pos} -> TopicProps topicId pos Expanded
              Nothing -> TM.initLimboMapTopic topicId map.id model
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
