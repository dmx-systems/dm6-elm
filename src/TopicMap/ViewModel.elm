module TopicMap.ViewModel exposing (topicsToRender, assocsToRender, isLimboTopic,
  isLimboAssoc, limboState)

import Item
import Box
import Feature.SearchDef exposing (SearchResult(..))
import Model exposing (Model)
import ModelBase exposing (..)
import TopicMap.TopicMap as TM
import TopicMap.TopicMapDef exposing (TopicMap, MapItem, Visibility(..), ItemProps(..),
  TopicProps)
import Utils as U

import Dict



{- Projects box data and search state ("limbo") into a TopicMap render model -}
topicsToRender : TopicMap -> Model -> List MapItem
topicsToRender map model =
  let
    topics = itemsToRender map TM.isTopic model |> List.map
      (\mapItem ->
        { mapItem
        | props =
          case mapItem.props of
            TopicP props -> TopicP <| effectiveDisplayMode mapItem.id props map.id model
            AssocP _ -> U.logError "topicsToRender" "Found assoc in a topic list" mapItem.props
        }
      )
    limboTopic = limboMapItem map.id model
  in
  topics ++ limboTopic


{- Projects box data and search state ("limbo") into a TopicMap render model -}
assocsToRender : TopicMap -> Model -> List MapItem
assocsToRender map model =
  itemsToRender map TM.isAssoc model


itemsToRender : TopicMap -> (MapItem -> Bool) -> Model -> List MapItem
itemsToRender map filter model =
  map.items
    |> Dict.values
    |> List.filter filter
    |> List.filter (shouldItemRender map.id model)


shouldItemRender : BoxId -> Model -> MapItem -> Bool
shouldItemRender boxId model item =
  TM.isVisible item || isLimboItem item boxId model


effectiveDisplayMode : Id -> TopicProps -> BoxId -> Model -> TopicProps
effectiveDisplayMode topicId props boxId model =
  case Box.displayMode topicId boxId model of
    Just displayMode ->
      { props | displayMode =
        if isLimboTopic topicId boxId model then
          case Item.isBox topicId model of
            True -> BoxD WhiteBox
            False -> TopicD Detail
        else
          displayMode
      }
    Nothing -> props


limboMapItem : BoxId -> Model -> List MapItem
limboMapItem mapId model =
  case limboState model of
    Just (topicId, _, limboBoxId) ->
      if limboBoxId == mapId && (not <| Box.hasItem mapId topicId model) then
        let
          _ = U.info "viewLimboTopic" (topicId, "not in map", mapId)
          props = TopicP <| TM.initLimboTopicProps topicId mapId model
        in
        [ MapItem topicId Removed props ] -- TODO: explain Removed
      else
        []
    Nothing -> []


isLimboItem : MapItem -> BoxId -> Model -> Bool
isLimboItem item boxId model =
  let
    isLimbo =
      case item.props of
        TopicP _ -> isLimboTopic
        AssocP _ -> isLimboAssoc
  in
  isLimbo item.id boxId model


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
