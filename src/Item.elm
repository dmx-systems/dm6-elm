module Item exposing (..)

import Model exposing (Model)
import ModelHelper exposing (..)
import Utils as U

import Dict
import Set
import String exposing (fromInt)



topicById : Id -> Model -> Maybe TopicInfo
topicById topicId model =
  case itemById topicId model of
    Just {info} ->
      case info of
        Topic topic -> Just topic
        Assoc _ -> U.topicMismatch "topicById" topicId Nothing
    Nothing -> U.fail "topicById" topicId Nothing


assocById : Id -> Model -> Maybe AssocInfo
assocById assocId model =
  case itemById assocId model of
    Just {info} ->
      case info of
        Topic _ -> U.assocMismatch "assocById" assocId Nothing
        Assoc assoc -> Just assoc
    Nothing -> U.fail "assocById" assocId Nothing


itemById : Id -> Model -> Maybe Item
itemById itemId model =
  case model.items |> Dict.get itemId of
    Just item -> Just item
    Nothing -> U.illegalItemId "itemById" itemId Nothing


topicLabel : TopicInfo -> String
topicLabel topic =
  case topic.text |> String.lines |> List.head of
    Just line -> line
    Nothing -> ""


addTopic : String -> Maybe Icon -> Model -> (Model, Id)
addTopic text icon model =
  let
    id = model.nextId
    topic = Item id (Topic <| TopicInfo id text icon) Set.empty
  in
  ( { model | items = model.items |> Dict.insert id topic }
    |> nextId
  , id
  )


addAssoc : ItemType -> RoleType -> Id -> RoleType -> Id -> Model -> (Model, Id)
addAssoc itemType role1 player1 role2 player2 model =
  let
    id = model.nextId
    assoc = Item id (Assoc <| AssocInfo id itemType role1 player1 role2 player2) Set.empty
  in
  ( { model | items = model.items |> Dict.insert id assoc }
    |> insertAssocId_ id player1
    |> insertAssocId_ id player2
    |> nextId
  , id
  )


removeItem : Id -> Model -> Model
removeItem itemId model =
  itemAssocIds itemId model |> Set.foldr
    removeItem -- recursion
    model
    |> removeAssocRefs_ itemId
    |> removeItem_ itemId


removeAssocRefs_ : Id -> Model -> Model
removeAssocRefs_ itemId model =
  case itemById itemId model of
    Just {info} ->
      case info of
        Assoc assoc ->
          model
          |> removeAssocId_ assoc.id assoc.player1
          |> removeAssocId_ assoc.id assoc.player2
        Topic _ -> model
    Nothing -> model -- error is already logged


removeItem_ : Id -> Model -> Model
removeItem_ itemId model =
  { model
    | items = model.items |> Dict.remove itemId -- delete item
    , boxes = model.boxes |> Dict.map -- delete item from all boxes
      (\_ box -> { box | items = box.items |> Dict.remove itemId })
  }


relatedItems : Id -> Model -> List (Id, Id)
relatedItems itemId model =
  itemAssocIds itemId model |> Set.foldr
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


itemAssocIds : Id -> Model -> AssocIds
itemAssocIds itemId model =
  case itemById itemId model of
    Just {assocIds} -> assocIds
    Nothing -> Set.empty -- error is already logged


insertAssocId_ : Id -> Id -> Model -> Model
insertAssocId_ assocId itemId model =
  model
  |> updateItem itemId (\item -> {item | assocIds = item.assocIds |> Set.insert assocId})


removeAssocId_ : Id -> Id -> Model -> Model
removeAssocId_ assocId itemId model =
  model
  |> updateItem itemId (\item -> {item | assocIds = item.assocIds |> Set.remove assocId})


updateTopicInfo : Id -> (TopicInfo -> TopicInfo) -> Model -> Model
updateTopicInfo topicId transform model =
  model |> updateItem topicId
    (\item ->
      case item.info of
        Topic topic -> { item | info = Topic <| transform topic }
        Assoc _  -> U.topicMismatch "updateTopicInfo" topicId item
    )


updateItem : Id -> (Item -> Item) -> Model -> Model
updateItem itemId transform model =
  { model | items = model.items |> Dict.update itemId
    (\maybeItem ->
      case maybeItem of
        Just item -> Just <| transform item
        Nothing -> U.illegalItemId "updateItem" itemId Nothing
    )
  }


nextId : Model -> Model
nextId model =
  { model | nextId = model.nextId + 1 }
