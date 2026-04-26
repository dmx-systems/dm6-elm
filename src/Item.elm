module Item exposing (topicById, assocById, topicLabel, topicSize, setTopicSize, updateTopic,
  createTopic, createAssoc, relatedTopics, otherPlayerId, isTopic, isAssoc, isBox, nextId,
  topicByIdOrNothing, assocByIdOrNothing)

import Config as C
import Model exposing (Model)
import ModelBase exposing (..)
import Utils as U

import Dict
import String exposing (fromInt)



{-| Looks up a Topic in Model.topics.
Logs an error if Topic is missing.
-}
topicById : Id -> Model -> Maybe Topic
topicById topicId model =
  case model.topics |> Dict.get topicId of
    Just topic -> Just topic
    Nothing -> U.topicNotFound "Item.topicById" topicId Nothing


{-| Looks up an Assoc in Model.assocs.
Logs an error if Assoc is missing.
-}
assocById : Id -> Model -> Maybe Assoc
assocById assocId model =
  case model.assocs |> Dict.get assocId of
    Just assoc -> Just assoc
    Nothing -> U.assocNotFound "Item.assocById" assocId Nothing


-- ### TODO: drop
topicByIdOrNothing : Id -> Model -> Maybe Topic
topicByIdOrNothing topicId model =
  case model.topics |> Dict.get topicId of
    Just topic -> Just topic
    Nothing -> Nothing


-- ### TODO: drop
assocByIdOrNothing : Id -> Model -> Maybe Assoc
assocByIdOrNothing assocId model =
  case model.assocs |> Dict.get assocId of
    Just assoc -> Just assoc
    Nothing -> Nothing


topicLabel : Topic -> String
topicLabel topic =
  case topic.text |> String.lines |> List.head of
    Just line -> line
    Nothing -> ""


{-| Logs an error if topic does not exist, or ID refers not a topic (but an association). -}
-- TODO: rename to textSize/topicTextSize?
topicSize : Id -> (TextSize -> Size) -> Model -> Maybe Size
topicSize topicId get model =
  case topicById topicId model of
    Just { size } -> Just <| get size
    Nothing -> U.fail "Item.topicSize" {topicId = topicId} Nothing


{-| Logs an error if box does not exist, or topic is not in box -}
setTopicSize : Id -> SizeField -> Size -> Model -> Model
setTopicSize topicId sizeField size model =
  model
    |> updateTopic topicId
      (\topic ->
        let
          size_ = topic.size
        in
        { topic | size =
          case sizeField of
            -- detail width does not include icon box
            View -> { size_ | view = { size | w = size.w - C.topicHeight } }
            Editor -> { size_ | editor = size }
        }
      )


createTopic : String -> Maybe Icon -> Model -> (Model, Id)
createTopic text icon model =
  let
    id = model.nextId
    topic = Topic id icon text (TextSize C.topicDetailSize C.topicDetailSize) []
  in
  ( model
      |> createTopic_ topic
      |> nextId
  , id
  )


createTopic_ : Topic -> Model -> Model
createTopic_ topic ({topics} as model) =
  { model | topics = topics |> Dict.insert topic.id topic }


createAssoc : AssocType -> Id -> Id -> Model -> (Model, Id)
createAssoc assocType topicId1 topicId2 ({assocs} as model) =
  let
    id = model.nextId
    assoc = Assoc id assocType topicId1 topicId2
  in
  ( { model | assocs = assocs |> Dict.insert id assoc }
      |> insertAssocId_ id topicId1
      |> insertAssocId_ id topicId2
      |> nextId
  , id
  )


-- Result is (topic ID, assoc ID)
relatedTopics : Id -> Model -> List (Id, Id)
relatedTopics topicId model =
  case topicById topicId model of
    Just topic ->
      topic.assocIds |> List.foldr
        (\assocId acc ->
          (otherPlayerId assocId topicId model, assocId) :: acc
        )
        []
    Nothing -> U.fail "Item.relatedTopics" {topicId = topicId} []


otherPlayerId : Id -> Id -> Model -> Id
otherPlayerId assocId playerId model =
  case assocById assocId model of
    Just {topicId1, topicId2} ->
      if playerId == topicId1 then
        topicId2
      else if playerId == topicId2 then
        topicId1
      else
        U.logError "Item.otherPlayerId"
          (fromInt playerId ++ " is not a player in assoc " ++ fromInt assocId) -1
    Nothing -> -1 -- error is already logged


{-| Inserts an association ID into the item's set of association IDs.
No-op if the association ID is in the set already.
Logs an error if item does not exist.
Low-level API for maintaining the association ID set.
-}
insertAssocId_ : Id -> Id -> Model -> Model
insertAssocId_ assocId topicId model =
  model |> updateTopic topicId
    (\({assocIds} as topic) ->
      {topic | assocIds = assocId :: assocIds}
    )


{-| Canonical Topic transformation.
Logs an error if item does not exist.
-}
updateTopic : Id -> (Topic -> Topic) -> Model -> Model
updateTopic topicId transform ({topics} as model) =
  { model | topics = topics |> Dict.update topicId
    (\maybeTopic ->
      case maybeTopic of
        Just topic -> Just <| transform topic
        Nothing -> U.topicNotFound "Item.update" topicId Nothing
    )
  }


-- TODO: drop
isTopic : Id -> Model -> Bool
isTopic id model =
  model.topics |> Dict.member id


-- TODO: drop
isAssoc : Id -> Model -> Bool
isAssoc id model =
  model.assocs |> Dict.member id


isBox : Id -> Model -> Bool
isBox id model =
  model.boxes |> Dict.member id


nextId : Model -> Model
nextId model =
  { model | nextId = model.nextId + 1 }
