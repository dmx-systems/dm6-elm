module Topic exposing (fromId, label, size, setSize, create, update, isBox)

import Config as C
import Model exposing (Model)
import ModelBase exposing (..)
import Utils as U

import Dict



{-| Looks up a Topic in Model.topics.
Logs an error if Topic is missing.
-}
fromId : TopicId -> Model -> Maybe Topic
fromId topicId model =
  case model.topics |> Dict.get (toTopicId topicId) of
    Just topic -> Just topic
    Nothing -> U.topicNotFound "Topic.fromId" topicId Nothing


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
    Nothing -> U.fail "Topic.size" {topicId = topicId} Nothing


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
    id = TopicId model.nextId
    topic = Topic id icon text (TextSize C.topicDetailSize C.topicDetailSize) []
  in
  ( model
      |> create_ topic
      |> Model.nextId
  , id
  )


create_ : Topic -> Model -> Model
create_ topic ({topics} as model) =
  { model | topics = topics |> Dict.insert (toTopicId topic.id) topic }


{-| Canonical Topic transformation.
Logs an error if item does not exist.
-}
update : TopicId -> (Topic -> Topic) -> Model -> Model
update topicId transform ({topics} as model) =
  { model | topics = topics |> Dict.update (toTopicId topicId)
    (\maybeTopic ->
      case maybeTopic of
        Just topic -> Just <| transform topic
        Nothing -> U.topicNotFound "Topic.update" topicId Nothing
    )
  }


isBox : TopicId -> Model -> Bool
isBox (TopicId id) model =
  model.boxes
    |> Dict.member id
