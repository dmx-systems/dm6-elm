module Box exposing (topics, topicIds, assocIds, turnTopicIntoBox, addTopic, addAssoc,
  removeTopic, removeAssoc, deleteTopic, deleteAssoc, expansionOf, updateExpansion, rendererOf,
  setRenderer, hasItem, hasDeepItem, mapTitle, isFullscreen, elemId, firstId, fromPath)

import Assoc
import Extension exposing (Renderer)
import Model exposing (Model)
import ModelBase exposing (..)
import Topic
import Utils as U

import Dict
import String exposing (fromInt)



topics : BoxId -> Model -> List Topic
topics boxId model =
  topicIds boxId model |> List.foldr
    (\(TopicId id) acc ->
      case Topic.fromId id model of
        Just topic -> topic :: acc
        Nothing -> acc
    )
    []


topicIds : BoxId -> Model -> List TopicId
topicIds boxId model =
  case itemSetOf boxId model of
    Just itemSet ->
      itemSet.items |> List.foldr
        (\setItem acc ->
          case setItem.id of
            T id -> id :: acc
            A _ -> acc
        )
        []
    Nothing -> []


assocIds : BoxId -> Model -> List AssocId
assocIds boxId model =
  case itemSetOf boxId model of
    Just itemSet ->
      itemSet.items |> List.foldr
        (\setItem acc ->
          case setItem.id of
            A id -> id :: acc
            T _ -> acc
        )
        []
    Nothing -> []


-- Create box

turnTopicIntoBox : Id -> Model -> Model
turnTopicIntoBox topicId model =
  let
    setId = model.nextId
    set = ItemSet setId []
    box = Box topicId setId Dict.empty Extension.defaultRenderer
  in
  model
    |> create box
    |> createItemSet set
    |> Model.nextId


create : Box -> Model -> Model
create box ({boxes} as model) =
  { model | boxes = boxes |> Dict.insert box.id box }


createItemSet : ItemSet -> Model -> Model
createItemSet set ({itemSets} as model) =
  { model | itemSets = itemSets |> Dict.insert set.id set }


-- Add item to box

{-| Adds an item to a box and creates a connecting association. This is an idempotent operation.
This is a generic operation: works for both, topics and associations.
-}
addTopic : BoxTopic -> BoxId -> Model -> Model
addTopic topic boxId model =
  model
    |> addToItemSet (fromTopicId topic.id) boxId
    |> addToBoxTopics topic boxId


addAssoc : Id -> BoxId -> Model -> Model
addAssoc assocId boxId model =
  model
    |> addToItemSet (fromAssocId assocId) boxId


addToItemSet : ItemId -> BoxId -> Model -> Model
addToItemSet itemId boxId model =
  if hasItem itemId boxId model then
    model
  else
    case byId boxId model of
      Just box ->
        model
          |> addToItemSet_ (SetItem itemId) box.itemSetId
          |> createHierarchy itemId boxId
      Nothing -> model


addToItemSet_ : SetItem -> Id -> Model -> Model
addToItemSet_ setItem itemSetId ({itemSets} as model) =
  { model | itemSets = itemSets |> Dict.update itemSetId
    (\maybeItemSet ->
      case maybeItemSet of
        Just itemSet -> Just { itemSet | items = setItem :: itemSet.items }
        Nothing -> U.itemSetNotFound "Box.addToItemSet_" itemSetId Nothing
    )
  }


createHierarchy : ItemId -> BoxId -> Model -> Model
createHierarchy itemId boxId model =
  -- Only topics get connected to box by Hierarchy association.
  -- We don't connect associations to associations.
  case itemId of
    T (TopicId id) ->
      model
        |> Assoc.create Hierarchy boxId id
        |> Tuple.first
    A _ -> model


addToBoxTopics : BoxTopic -> BoxId -> Model -> Model
addToBoxTopics topic boxId ({boxes} as model) =
  if hasBoxTopic topic.id boxId model then
    model
  else
    { model | boxes = boxes |> Dict.update boxId
      (\maybeBox ->
        case maybeBox of
          Just box -> Just { box | topics = box.topics |> Dict.insert topic.id topic }
          Nothing -> Nothing
      )
    }


-- Remove item from box

removeTopic : Id -> BoxId -> Model -> Model
removeTopic topicId boxId model =
  case (Topic.fromId topicId model, hierarchyAssoc topicId boxId model) of
    (Just topic, Just assocId) ->
      model
        |> removeItems (topicId :: topic.assocIds) boxId
        |> deleteAssoc assocId
    _ -> U.fail "Box.removeTopic" {topicId = topicId, boxId = boxId} model


removeAssoc : Id -> BoxId -> Model -> Model
removeAssoc assocId boxId model =
  model
    |> removeItems [ assocId ] boxId


{-| Removes an item from a box's underlying ItemSet.
It's a no-op if the item is not in the box.
Note: the BoxItem itself is *not* removed.
-}
removeItems : List Id -> BoxId -> Model -> Model
removeItems itemIds boxId model =
  case byId boxId model of
    Just box ->
      { model | itemSets = model.itemSets
        |> Dict.update box.itemSetId
          (\maybeItemSet ->
            case maybeItemSet of
              Just itemSet -> Just
                { itemSet | items = itemSet.items
                  |> List.filter (\setItem -> not <| List.member (toId setItem.id) itemIds)
                }
              Nothing -> Nothing
          )
      }
    Nothing -> model


hierarchyAssoc : Id -> BoxId -> Model -> Maybe Id
hierarchyAssoc topicId boxId model =
  case Topic.fromId topicId model of
    Just topic ->
      findHierarchy topicId boxId topic.assocIds model
    Nothing -> U.fail "Box.hierarchyAssoc" {topicId = topicId, boxId = boxId} Nothing


findHierarchy : Id -> BoxId -> AssocIds -> Model -> Maybe Id
findHierarchy topicId boxId assocIds_ model =
  case assocIds_ of
    [] ->
      U.logError "Box.findHierarchy"
        ("Missing Hierarchy of Topic " ++ fromInt topicId ++ " in Box " ++ fromInt boxId)
        Nothing
    assocId :: ids ->
      case Assoc.fromId assocId model of
        Just {id, assocType, topicId1, topicId2} ->
          if assocType == Hierarchy && topicId1 == boxId && topicId2 == topicId then
            Just id
          else
            findHierarchy topicId boxId ids model -- recursion
        Nothing -> Nothing


-- Delete item

{-| Deletes an item, along its associations, and removes them from all boxes.
Logs an error if no such item exists.
It's a generic operation: works for both, topics and associations. ### FIXDOC
-}
deleteTopic : Id -> Model -> Model
deleteTopic topicId model =
  case Topic.fromId topicId model of
    Just topic ->
      ( topic.assocIds |> List.foldr
          deleteAssoc
          model
      )
      |> deleteTopic_ topicId
    Nothing -> U.fail "Box.deleteTopic" {topicId = topicId} model


deleteAssoc : Id -> Model -> Model
deleteAssoc assocId model =
  model
    |> removeAssocFromTopics assocId
    |> deleteAssoc_ assocId


{-| Removes the association ID from both connected topics.
No-op if the given ID refers not to an association (but a topic).
Logs an error no item for the given ID exists. ### FIXDOC
-}
removeAssocFromTopics : Id -> Model -> Model
removeAssocFromTopics assocId model =
  case Assoc.fromId assocId model of
    Just assoc ->
      model
        |> removeAssocFromTopic assoc.id assoc.topicId1
        |> removeAssocFromTopic assoc.id assoc.topicId2
    Nothing -> U.fail "Box.removeAssocFromTopics" {assocId = assocId} model


{-| Removes an association ID from the item's set of association IDs.
No-op if the given association ID is not in the set.
Logs an error if item does not exist.
-}
removeAssocFromTopic : Id -> Id -> Model -> Model
removeAssocFromTopic assocId topicId model =
  model |> Topic.update topicId
    (\topic ->
      {topic | assocIds = topic.assocIds |> List.filter ((/=) assocId)}
    )


{-| Deletes an item, and removes it from all boxes.
No-op if there is no such item.
Low-level function that does NOT delete the item's associations. ### FIXDOC
-}
deleteTopic_ : Id -> Model -> Model
deleteTopic_ topicId ({itemSets, topicMap} as model) =
  { model
  | topics = model.topics |> Dict.remove topicId -- delete topic
  , itemSets = itemSets |> Dict.map -- delete topic from all itemSets
      (\_ ({items} as itemSet) ->
        { itemSet | items = items |> List.filter
          (\setItem -> (toId setItem.id) /= topicId)
        }
      )
  -- TODO: if item is box delete from "boxes" state as well
  -- TODO: don't operate on "topicMap" directly, let ExtManager dispatch instead
  , topicMap = topicMap |> Dict.map -- delete item from all boxes
      (\_ map ->
        { map | topics = map.topics |> Dict.remove topicId }
      )
  }


{-| Deletes an item, and removes it from all boxes.
No-op if there is no such item.
Low-level function that does NOT delete the item's associations. ### FIXDOC
-}
deleteAssoc_ : Id -> Model -> Model
deleteAssoc_ assocId ({assocs, itemSets} as model) =
  { model
  | assocs = assocs |> Dict.remove assocId -- delete assoc
  , itemSets = itemSets |> Dict.map -- delete assoc from all itemSets
      (\_ ({items} as itemSet) ->
        { itemSet | items = items |> List.filter
          (\setItem -> (toId setItem.id) /= assocId)
        }
      )
  }


-- Expansion

expansionOf : Id -> BoxId -> Model -> Expansion
expansionOf topicId boxId model =
  case byId boxId model |> Maybe.andThen (\box -> topicById topicId box model) of
    Just {expansion} -> expansion
    Nothing -> U.fail "Box.expansionOf" {topicId = topicId, boxId = boxId} Collapsed


updateExpansion : Id -> BoxId -> (Expansion -> Expansion) -> Model -> Model
updateExpansion topicId boxId transform model =
  { model | boxes = model.boxes |> Dict.update boxId
    (\maybeBox ->
      case maybeBox of
        Just box -> Just
          { box | topics = box.topics |> Dict.update topicId
            (\maybeTopic ->
              case maybeTopic of
                Just topic -> Just
                  { topic | expansion = transform topic.expansion }
                Nothing -> Nothing
            )
          }
        Nothing -> Nothing
    )
  }


-- Renderer

rendererOf : BoxId -> Model -> Maybe Renderer
rendererOf boxId model =
  case byId boxId model of
    Just box -> Just box.renderer
    Nothing -> U.fail "Box.rendererOf" {boxId = boxId} Nothing


setRenderer : BoxId -> Renderer -> Model -> Model
setRenderer boxId renderer ({boxes} as model) =
  { model | boxes = boxes |> Dict.update boxId
    (\maybeBox ->
      case maybeBox of
        Just box -> Just { box | renderer = renderer }
        Nothing -> Nothing
    )
  }


--

{-| Checks if an item is contained in a box's underlying ItemSet.
Logs an error if the box or its ItemSet is absent.
-}
hasItem : ItemId -> BoxId -> Model -> Bool
hasItem itemId boxId model =
  case itemSetOf boxId model of
    Just itemSet ->
      itemSet.items
        |> List.any (\setItem -> setItem.id == itemId)
    Nothing -> False


hasDeepItem : BoxId -> Id -> Model -> Bool
hasDeepItem boxId itemId model =
  if itemId == boxId then
    True
  else
    if Topic.isBox boxId model then
      case itemSetOf boxId model of
        Just itemSet -> itemSet.items |> List.any
          (\setItem -> hasDeepItem (toId setItem.id) itemId model) -- recursion
        Nothing -> False
    else
      False


itemSetOf : BoxId -> Model -> Maybe ItemSet
itemSetOf boxId model =
  case byId boxId model of
    Just box -> model.itemSets |> Dict.get box.itemSetId
    Nothing -> U.fail "Box.itemSetOf" {boxId = boxId} Nothing


hasBoxTopic : Id -> BoxId -> Model -> Bool
hasBoxTopic topicId boxId model =
  case byId boxId model of
    Just box -> box.topics |> Dict.member topicId
    Nothing -> U.fail "Box.hasBoxTopic" {topicId = topicId, boxId = boxId} False


{-| Logs an error if box does not exist. -}
byId : BoxId -> Model -> Maybe Box
byId boxId model =
  case model.boxes |> Dict.get boxId of
    Just box -> Just box
    Nothing -> U.boxNotFound "Box.byId" boxId Nothing


topicById : Id -> Box -> Model -> Maybe BoxTopic
topicById topicId box model =
  case box.topics |> Dict.get topicId of
    Just topic -> Just topic
    Nothing -> U.logError "Box.topicById"
      ("Missing BoxTopic " ++ fromInt topicId ++ " in Box " ++ fromInt box.id)
      Nothing


{-| Title of fullscreen box.
TODO: rename
-}
mapTitle : Model -> String
mapTitle model =
  case Topic.fromId model.boxId model of
    Just topic -> Topic.label topic
    Nothing -> U.fail "mapTitle" model.boxId "??"


isFullscreen : BoxId -> Model -> Bool
isFullscreen boxId model =
  boxId == model.boxId


elemId : String -> Id -> BoxPath -> String
elemId name id boxPath =
  name ++ "-" ++ fromInt id ++ "," ++ fromPath boxPath


{-| Logs an error (and returns -1) if boxPath is empty.
-}
firstId : BoxPath -> BoxId
firstId boxPath =
  case boxPath of
    boxId :: _ -> boxId
    [] -> U.logError "firstId" "boxPath is empty!" -1


fromPath : BoxPath -> String
fromPath boxPath =
  boxPath
    |> List.map fromInt
    |> String.join ","
