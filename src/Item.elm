module Item exposing (..)

import Config as C
import Model exposing (Model)
import ModelParts exposing (..)
import Utils as U

import Dict
import Set
import String exposing (fromInt)



{-| Returns a topic by ID.
Logs an error if no such topic exists, or ID refers not a topic (but an association).
-}
topicById : Id -> Model -> Maybe TopicInfo
topicById topicId model =
  case byId topicId model of
    Just {info} ->
      case info of
        Topic topic -> Just topic
        Assoc _ -> U.topicMismatch "topicById" topicId Nothing
    Nothing -> U.fail "topicById" topicId Nothing


{-| Returns an association by ID.
Logs an error if no such association exists, or ID refers not an association (but a topic).
-}
assocById : Id -> Model -> Maybe AssocInfo
assocById assocId model =
  case byId assocId model of
    Just {info} ->
      case info of
        Topic _ -> U.assocMismatch "assocById" assocId Nothing
        Assoc assoc -> Just assoc
    Nothing -> U.fail "assocById" assocId Nothing


{-| Returns an item by ID.
Logs an error if no such item exists.
-}
byId : Id -> Model -> Maybe Item
byId itemId model =
  case model.items |> Dict.get itemId of
    Just item -> Just item
    Nothing -> U.illegalItemId "byId" itemId Nothing


topicLabel : TopicInfo -> String
topicLabel topic =
  case topic.text |> String.lines |> List.head of
    Just line -> line
    Nothing -> ""


{-| Logs an error if topic does not exist, or ID refers not a topic (but an association). -}
topicSize : Id -> (TextSize -> Size) -> Model -> Maybe Size
topicSize topicId get model =
  case topicById topicId model of
    Just { size } -> Just <| get size
    Nothing -> U.fail "topicSize" {topicId = topicId} Nothing


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
        Assoc _  -> U.topicMismatch "updateTopic" topicId item
    )


addTopic : String -> Maybe Icon -> Model -> (Model, Id)
addTopic text icon model =
  let
    id = model.nextId
    topic = TopicInfo id icon text <| TextSize C.topicDetailSize C.topicDetailSize
    item = Item id (Topic topic) Set.empty
  in
  ( { model | items = model.items |> Dict.insert id item }
    |> nextId
  , id
  )


addAssoc : AssocType -> Id -> Id -> Model -> (Model, Id)
addAssoc assocType player1 player2 model =
  let
    id = model.nextId
    assoc = AssocInfo id assocType player1 player2
    item = Item id (Assoc assoc) Set.empty
  in
  ( { model | items = model.items |> Dict.insert id item }
      |> insertAssocId_ id player1
      |> insertAssocId_ id player2
      |> nextId
  , id
  )


relatedItems : Id -> Model -> List (Id, Id)
relatedItems itemId model =
  assocIds itemId model |> Set.foldr
    (\assocId relItemsAcc ->
      (otherPlayerId assocId itemId model, assocId) :: relItemsAcc
    )
    []


otherPlayerId : Id -> Id -> Model -> Id
otherPlayerId assocId playerId model =
  case assocById assocId model of
    Just {player1, player2} ->
      if playerId == player1 then
        player2
      else if playerId == player2 then
        player1
      else
        U.logError "otherPlayerId"
          (fromInt playerId ++ " is not a player in assoc " ++ fromInt assocId) -1
    Nothing -> -1 -- error is already logged


{-| Returns the item's set of association IDs.
Logs an error if item does not exist.
-}
assocIds : Id -> Model -> AssocIds
assocIds itemId model =
  case byId itemId model of
    Just item -> item.assocIds
    Nothing -> Set.empty -- error is already logged


{-| Inserts an association ID into the item's set of association IDs.
No-op if the association ID is in the set already.
Logs an error if item does not exist.
Low-level API for maintaining the association ID set.
-}
insertAssocId_ : Id -> Id -> Model -> Model
insertAssocId_ assocId itemId model =
  model |> update itemId
    (\item ->
      {item | assocIds = item.assocIds |> Set.insert assocId}
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
        Nothing -> U.illegalItemId "update" itemId Nothing
    )
  }


hasPlayer : Id -> Model -> Id -> Bool
hasPlayer playerId model assocId =
  case assocById assocId model of
    Just assoc -> assoc.player1 == playerId || assoc.player2 == playerId
    Nothing -> False


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
