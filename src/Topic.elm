module Topic exposing (fromId, label, size, setSize, create, update, isBox)

import Config as C
import Console
import Feature.Id as Id
import Feature.Sync as Sync
import Model exposing (Model)
import ModelBase exposing (..)

import Dict



{-| Looks up a Topic in Model.topics.
Logs an error if Topic is missing.
-}
fromId : TopicId -> Model -> Maybe Topic
fromId topicId model =
  let
    (TopicId id) = topicId
  in
  case Sync.model model of
    Just syncModel ->
      case syncModel.topics |> Dict.get id of
        Just topic -> Just topic
        Nothing -> Console.logError "Topic.fromId" (id ++ " not found") Nothing
    Nothing ->
      Console.fail "Topic.fromId" id Nothing


label : Topic -> String
label topic =
  topic.text
    |> String.lines
    |> List.head
    |> Maybe.withDefault ""


{-| Logs an error if topic does not exist, or ID refers not a topic (but an association). -}
-- TODO: rename to textSize/topicTextSize?
size : TopicId -> (TextSize -> Size) -> Model -> Maybe Size
size topicId get model =
  case fromId topicId model of
    Just topic -> Just <| get topic.size
    Nothing -> Console.fail "Topic.size" {topicId = topicId} Nothing


{-| Logs an error if box does not exist, or topic is not in box -}
setSize : TopicId -> SizeField -> Size -> Model -> Model
setSize topicId sizeField size_ model =
  model
    |> update topicId
      (\topic ->
        let
          size__ = topic.size
        in
        { topic | size =
            case sizeField of
              -- detail width does not include icon box
              View -> { size__ | view = { size_ | w = size_.w - C.topicHeight } }
              Editor -> { size__ | editor = size_ }
        }
      )


create : String -> Maybe Icon -> Model -> (Model, TopicId)
create text icon model =
  let
    (id, model_) = Id.get model
    topicId = TopicId id
    size_ = TextSize C.topicDetailSize C.topicDetailSize
    topic = Topic topicId icon text size_ []
  in
  ( Sync.setTopic topic model_, topicId )


{-| Canonical Topic transformation.
Logs an error if item does not exist.
-}
update : TopicId -> (Topic -> Topic) -> Model -> Model
update topicId transform model =
  let
    (TopicId id) = topicId
  in
  case fromId topicId model of
    Just topic ->
      Sync.setTopic (transform topic) model
    Nothing ->
      let
        _ = Console.fail "Topic.update" id Nothing
      in
      model


isBox : TopicId -> Model -> Bool
isBox (TopicId id) model =
  model.boxes
    |> Dict.member id
