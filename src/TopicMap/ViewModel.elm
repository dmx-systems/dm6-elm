module TopicMap.ViewModel exposing (topicsToRender, assocsToRender, isLimboTopic,
  isLimboAssoc, limboState)

import Box
import Feature.SearchDef exposing (SearchResult(..))
import Model exposing (Model)
import ModelBase exposing (..)
import TopicMap.TopicMap as TM
import TopicMap.TopicMapDef exposing (TopicMap, MapItem, ItemProps(..), TopicProps)
import Utils as U



{- Projects box data and search state ("limbo") into a TopicMap render model -}
topicsToRender : TopicMap -> Model -> List MapItem
topicsToRender map model =
  let
    topics = TM.topics map model |> List.map
      (\mapItem ->
        { mapItem
        | props =
          case mapItem.props of
            TopicP props -> TopicP <| effectiveExpansion mapItem.id props map.id model
            AssocP _ -> U.logError "TopicMap.ViewModel.topicsToRender"
              "Found assoc in a topic list" mapItem.props
        }
      )
    limboTopic = limboMapItem map model
  in
  topics ++ limboTopic


{- Projects box data and search state ("limbo") into a TopicMap render model -}
assocsToRender : TopicMap -> Model -> List MapItem
assocsToRender =
  TM.assocs


effectiveExpansion : Id -> TopicProps -> BoxId -> Model -> TopicProps
effectiveExpansion topicId props boxId model =
  { props | expansion =
    if isLimboTopic topicId boxId model then
      Expanded
    else
      Box.expansionOf topicId boxId model
  }


limboMapItem : TopicMap -> Model -> List MapItem
limboMapItem map model =
  case limboState model of
    Just (topicId, _, limboBoxId) ->
      if limboBoxId == map.id && (not <| Box.hasItem (fromTopicId topicId) map.id model) then
        let
          _ = U.info "TopicMap.ViewModel.limboMapItem" (topicId, "not in map", map.id)
          props =
            case TM.topicPropsOrNothing topicId map of
              Just {pos} -> TopicP <| TopicProps pos Expanded
              Nothing -> TopicP <| TM.initLimboTopicProps topicId map.id model
        in
        [ MapItem topicId props ]
      else
        []
    Nothing -> []


isLimboTopic : Id -> BoxId -> Model -> Bool
isLimboTopic topicId boxId model =
  case limboState model of
    Just (topicId_, _, boxId_) -> topicId == topicId_ && boxId == boxId_
    Nothing -> False


isLimboAssoc : Id -> BoxId -> Model -> Bool
isLimboAssoc assocId boxId model =
  case limboState model of
    Just (_, Just assocId_, boxId_) -> assocId == assocId_ && boxId == boxId_
    _ -> False


limboState : Model -> Maybe (Id, Maybe Id, BoxId) -- (topic ID, assoc ID, box ID)
limboState model =
  case TM.revelationBoxId model of
    Just boxId ->
      case model.search.result of
        Topics _ (Just topicId) -> Just (topicId, Nothing, boxId)
        RelTopics _ (Just (topicId, assocId)) -> Just (topicId, Just assocId, boxId)
        _ -> Nothing
    Nothing -> Nothing
