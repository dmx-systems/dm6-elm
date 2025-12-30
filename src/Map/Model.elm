module Map.Model exposing (topicsToRender, assocsToRender, isLimboTopic, isLimboAssoc,
  limboState)

import Box
import Feature.Search exposing (SearchResult(..))
import Feature.SelAPI as SelAPI
import Model exposing (Model)
import ModelParts exposing (..)
import Utils as U

import Dict



{- Projects box data and search state ("limbo") into a map render model -}
topicsToRender : Box -> Model -> List BoxItem
topicsToRender box model =
  let
    topics = itemsToRender box Box.isTopic model |> List.map
      (\boxItem ->
        { boxItem
        | props =
          case boxItem.props of
            TopicP props -> TopicP <| effectiveDisplayMode boxItem.id box.id model props
            AssocP _ -> U.logError "topicsToRender" "Found assoc in a topic list" boxItem.props
        }
      )
    limboTopic =
      case limboState model of
        Just (topicId, _, limboBoxId) ->
          if limboBoxId == box.id && (not <| Box.hasItem box.id topicId model) then
            let
              _ = U.info "viewLimboTopic" (topicId, "not in box", box.id)
              props =
                TopicP
                  <| effectiveDisplayMode topicId box.id model
                  <| Box.initTopicProps topicId box.id model
            in
            [ BoxItem topicId -1 Removed props ] -- TODO: explain -1 Removed
          else
            []
        Nothing -> []
  in
  topics ++ limboTopic


{- Projects box data and search state ("limbo") into a map render model -}
assocsToRender : Box -> Model -> List BoxItem
assocsToRender box model =
  itemsToRender box Box.isAssoc model


itemsToRender : Box -> (BoxItem -> Bool) -> Model -> List BoxItem
itemsToRender box filter model =
  box.items
  |> Dict.values
  |> List.filter filter
  |> List.filter (shouldItemRender box.id model)


shouldItemRender : BoxId -> Model -> BoxItem -> Bool
shouldItemRender boxId model item =
  Box.isVisible item || isLimboItem item boxId model


-- Note: "props" is last paramter for piping
effectiveDisplayMode : Id -> BoxId -> Model -> TopicProps -> TopicProps
effectiveDisplayMode topicId boxId model props =
  { props | displayMode =
    if isLimboTopic topicId boxId model then
      case props.displayMode of
        TopicD _ -> TopicD Detail
        BoxD _ ->
          case Box.isEmpty topicId model of
            True -> props.displayMode -- don't whitebox an empty box
            False -> BoxD WhiteBox
    else
      props.displayMode
  }


isLimboItem : BoxItem -> BoxId -> Model -> Bool
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
  case SelAPI.revelationBoxId model of
    Just boxId ->
      case model.search.result of
        Topics _ (Just topicId) -> Just (topicId, Nothing, boxId)
        RelTopics _ (Just (topicId, assocId)) -> Just (topicId, Just assocId, boxId)
        _ -> Nothing
    Nothing -> Nothing
