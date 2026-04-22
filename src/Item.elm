module Item exposing (topicById, topicOrNothing, assocById, assocOrNothing, byId, topicLabel,
  topicSize, setTopicSize, updateTopic, createTopic, createAssoc, relatedItems, otherPlayerId,
  assocIds, update, isBox, nextId)

import Config as C
import Model exposing (Model)
import ModelBase exposing (..)
import Utils as U

import Dict
import String exposing (fromInt)



{-| Returns a topic by ID.
Crashes if no such item exists, or ID refers not a topic (but an association).
-}
topicById : Id -> Model -> TopicInfo
topicById topicId model =
  case (byId topicId model).info of
    Topic topic -> topic
    Assoc _ -> U.todo
      ("Item " ++ fromInt topicId ++ " is an association when a topic is expected")
      (TopicInfo -1 Nothing "" (TextSize {w = 0, h = 0} {w = 0, h = 0}))


{-| Returns a topic by ID, or Nothing if the ID refers not a topic (but an association).
Crashes if no such item exists.
-}
topicOrNothing : Id -> Model -> Maybe TopicInfo
topicOrNothing topicId model =
  case (byId topicId model).info of
    Topic topic -> Just topic
    Assoc _ -> Nothing


{-| Returns an association by ID.
Crashes if no such item exists, or ID refers not an association (but a topic).
-}
assocById : Id -> Model -> AssocInfo
assocById assocId model =
  case (byId assocId model).info of
    Assoc assoc -> assoc
    Topic _ -> U.todo
      ("Item " ++ fromInt assocId ++ " is a topic when an association is expected")
      (AssocInfo -1 Crosslink -1 -1)


{-| Returns an association by ID, or Nothing if the ID refers not an association (but a topic).
Crashes if no such item exists.
-}
assocOrNothing : Id -> Model -> Maybe AssocInfo
assocOrNothing assocId model =
  case (byId assocId model).info of
    Assoc assoc -> Just assoc
    Topic _ -> Nothing


{-| Returns an item by ID.
Crashes if no such item exists.
-}
byId : Id -> Model -> Item
byId itemId model =
  case model.items |> Dict.get itemId of
    Just item -> item
    Nothing -> U.todo
      ("Missing Item " ++ fromInt itemId ++ " in Model.items")
      (Item -1 (Topic (TopicInfo -1 Nothing "" (TextSize {w = 0, h = 0} {w = 0, h = 0}))) [])


topicLabel : TopicInfo -> String
topicLabel topic =
  case topic.text |> String.lines |> List.head of
    Just line -> line
    Nothing -> ""


{-| Logs an error if topic does not exist, or ID refers not a topic (but an association). -}
-- TODO: rename to textSize/topicTextSize?
-- TODO: remove Maybe
topicSize : Id -> (TextSize -> Size) -> Model -> Maybe Size
topicSize topicId get model =
  Just <| get (topicById topicId model).size


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


updateTopic : Id -> (TopicInfo -> TopicInfo) -> Model -> Model
updateTopic topicId transform model =
  model
  |> update topicId
    (\item ->
      case item.info of
        Topic topic -> { item | info = Topic <| transform topic }
        Assoc _  -> U.topicMismatch "Item.updateTopic" topicId item
    )


createTopic : String -> Maybe Icon -> Model -> (Model, Id)
createTopic text icon model =
  let
    id = model.nextId
    topic = TopicInfo id icon text <| TextSize C.topicDetailSize C.topicDetailSize
    item = Item id (Topic topic) []
  in
  ( model
      |> createTopic_ item
      |> nextId
  , id
  )


createTopic_ : Item -> Model -> Model
createTopic_ item ({items} as model) =
  { model | items = items |> Dict.insert item.id item }


createAssoc : AssocType -> Id -> Id -> Model -> (Model, Id)
createAssoc assocType player1 player2 model =
  let
    id = model.nextId
    assoc = AssocInfo id assocType player1 player2
    item = Item id (Assoc assoc) []
  in
  ( { model | items = model.items |> Dict.insert id item }
      |> insertAssocId_ id player1
      |> insertAssocId_ id player2
      |> nextId
  , id
  )


relatedItems : Id -> Model -> List (Id, Id)
relatedItems itemId model =
  assocIds itemId model |> List.foldr
    (\assocId relItemsAcc ->
      (otherPlayerId assocId itemId model, assocId) :: relItemsAcc
    )
    []


otherPlayerId : Id -> Id -> Model -> Id
otherPlayerId assocId playerId model =
  let
    {player1, player2} = assocById assocId model
  in
  if playerId == player1 then
    player2
  else if playerId == player2 then
    player1
  else
    U.todo (fromInt playerId ++ " is not a player in assoc " ++ fromInt assocId) -1


{-| Returns the item's set of association IDs.
Logs an error if item does not exist.
-}
assocIds : Id -> Model -> AssocIds
assocIds itemId model =
  (byId itemId model).assocIds


{-| Inserts an association ID into the item's set of association IDs.
No-op if the association ID is in the set already.
Logs an error if item does not exist.
Low-level API for maintaining the association ID set.
-}
insertAssocId_ : Id -> Id -> Model -> Model
insertAssocId_ assocId itemId model =
  model |> update itemId
    (\item ->
      {item | assocIds = assocId :: item.assocIds}
    )


{-| Canonical item transformation.
Logs an error if item does not exist.
-}
update : Id -> (Item -> Item) -> Model -> Model
update itemId transform model =
  { model | items = model.items |> Dict.update itemId
    (\maybeItem ->
      case maybeItem of
        Just item -> Just <| transform item
        Nothing -> U.itemNotFound "Item.update" itemId Nothing
    )
  }


{-| useful as a filter predicate -}
isTopic : Item -> Bool
isTopic item =
  case item.info of
    Topic _ -> True
    Assoc _ -> False


{-| useful as a filter predicate -}
isAssoc : Item -> Bool
isAssoc item =
  not (isTopic item)


isBox : Id -> Model -> Bool
isBox id model =
  model.boxes |> Dict.member id


nextId : Model -> Model
nextId model =
  { model | nextId = model.nextId + 1 }
