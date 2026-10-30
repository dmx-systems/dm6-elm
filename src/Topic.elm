module Topic exposing (label, setIcon, setText, size, setSize, removeAssocId, create)

import Config as C
import Console
import Feature.Crdt as Crdt
import Feature.Id as Id
import Model exposing (Model)
import ModelBase exposing (..)
import Shared.Util as Util

import Dict



label : Topic -> String
label topic =
  topic.text
    |> String.lines
    |> List.head
    |> Maybe.withDefault ""


setIcon : TopicId -> Maybe Icon -> Model -> Model
setIcon =
  Crdt.setTopicIcon


setText : TopicId -> String -> Model -> Model
setText =
  Crdt.setTopicText


{-| Logs an error if topic does not exist, or ID refers not a topic (but an association). -}
-- TODO: rename to textSize/topicTextSize?
size : TopicId -> (TextSize -> Size) -> Model -> Maybe Size
size topicId get model =
  case Crdt.lookupTopic topicId model of
    Just topic -> Just <| get topic.size
    Nothing -> Console.fail "Topic.size" {topicId = topicId} Nothing


{-| Logs an error if box does not exist, or topic is not in box ### FIXDOC -}
setSize : TopicId -> SizeField -> Size -> Model -> Model
setSize topicId sizeField size_ model =
  case sizeField of
    -- detail width does not include icon box
    View -> Crdt.setTopicViewSize topicId { size_ | w = size_.w - C.topicHeight } model
    Editor -> Crdt.setTopicEditorSize topicId size_ model


-- ### TODO: make it Crdt internal?
{-| Removes an association ID from the topic's set of association IDs.
No-op if the given association ID is not in the set.
Logs an error if the topic does not exist. ### FIXDOC
-}
removeAssocId : AssocId -> TopicId -> Model -> Model
removeAssocId assocId topicId model =
  let
    findAssocId : Topic -> Maybe Int
    findAssocId topic =
      Util.requireIndex assocId identity topic.assocIds


    remove : Int -> Maybe Model
    remove index =
      Just <| Crdt.removeAssocId index topicId model


    result : Maybe Model -> Model
    result maybeModel =
      case maybeModel of
        Just model_ -> model_
        Nothing ->
          let
            _ = Console.fail "Topic.removeAssocId" topicId assocId
          in
          model
  in
  model
    |> Crdt.lookupTopic topicId
    |> Maybe.andThen findAssocId
    |> Maybe.andThen remove
    |> result


create : String -> Maybe Icon -> Model -> (Model, TopicId)
create text icon model =
  let
    (id, model_) = Id.new model
    topicId = TopicId id
    size_ = TextSize C.topicDetailSize C.topicDetailSize
    topic = Topic topicId icon text size_ []
  in
  (Crdt.createTopic topic model_, topicId)
