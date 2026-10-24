module Topic exposing (fromId, label, setIcon, setText, size, setSize, removeAssocId, create,
  isBox)

import Config as C
import Console
import Feature.Crdt as Crdt
import Feature.Id as Id
import Model exposing (Model)
import ModelBase exposing (..)

import Dict



{-| Looks up a Topic in Model.topics.
Logs an error if Topic is missing.
-}
fromId : TopicId -> Model -> Maybe Topic
fromId (TopicId id) model =
  case Crdt.docModel model of
    Just docModel ->
      case docModel.topics |> Dict.get id of
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
  case fromId topicId model of
    Just topic -> Just <| get topic.size
    Nothing -> Console.fail "Topic.size" {topicId = topicId} Nothing


{-| Logs an error if box does not exist, or topic is not in box ### FIXDOC -}
setSize : TopicId -> SizeField -> Size -> Model -> Model
setSize topicId sizeField size_ model =
  case sizeField of
    -- detail width does not include icon box
    View -> Crdt.setTopicViewSize topicId { size_ | w = size_.w - C.topicHeight } model
    Editor -> Crdt.setTopicEditorSize topicId size_ model


-- ### TODO: make it Crdt internal
{-| Removes an association ID from the topic's set of association IDs.
No-op if the given association ID is not in the set.
Logs an error if the topic does not exist. ### FIXDOC
-}
removeAssocId : AssocId -> TopicId -> Model -> Model
removeAssocId assocId topicId model =
  let
    findAssocId : Topic -> Maybe Int
    findAssocId topic =
      findIndex assocId topic.assocIds


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
    |> fromId topicId
    |> Maybe.andThen findAssocId
    |> Maybe.andThen remove
    |> result


findIndex : v -> List v -> Maybe Int
findIndex val list =
  let
    equality : Int -> v -> Maybe Int
    equality index val_ =
      case val_ == val of
        True -> Just index
        False -> Nothing


    result : List Int -> Maybe Int
    result indexes =
      case indexes of
        [ index ] ->
          Just index
        [] ->
          Console.logError "Topic.findIndex"
            (Console.toString val ++ " not in " ++ Console.toString list) Nothing
        _ ->
          Console.logError "Topic.findIndex"
            (Console.toString val ++ " more than once in " ++ Console.toString list) Nothing
  in
  list
    |> List.indexedMap equality
    |> List.filterMap identity
    |> result


create : String -> Maybe Icon -> Model -> (Model, TopicId)
create text icon model =
  let
    (id, model_) = Id.new model
    topicId = TopicId id
    size_ = TextSize C.topicDetailSize C.topicDetailSize
    topic = Topic topicId icon text size_ []
  in
  (Crdt.addTopic topic model_, topicId)


isBox : TopicId -> Model -> Bool
isBox (TopicId id) model =
  model.boxes
    |> Dict.member id
