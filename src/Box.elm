module Box exposing (topicCount, traverseWith, topicIds, assocIds, turnTopicIntoBox, addTopic,
  addAssoc, removeTopic, removeAssoc, deleteTopic, deleteAssoc, expansionOf, updateExpansion,
  rendererOf, setRenderer, hasItem, hadDeepTopic, mapTitle, isFullscreen, elemId, firstId,
  fromPath)

import Assoc
import Extension exposing (Renderer)
import Model exposing (Model)
import ModelBase exposing (..)
import Topic
import Utils as U

import Dict
import String exposing (fromInt)


-- Traversal

type alias Transform =
  BoxId -> Model -> List TopicId -> List TopicId


type alias Acc acc =
  Topic -> Level -> BoxPath -> acc -> Maybe acc -> Model -> acc


type alias LevelDone acc =
  BoxPath -> acc -> acc


topicCount : BoxId -> Model -> Int
topicCount boxId model =
  traverse [boxId] 0
    (\_ _ _ count childrenCount _ ->
      count + 1 + (childrenCount |> Maybe.withDefault 0)
    )
    model


{- A simple traversal, takes just an Acc function and the initial accumulator value.
-}
traverse : BoxPath -> acc -> Acc acc -> Model -> acc
traverse boxPath initAcc accumulate model =
  traverseWith
    boxPath
    (\_ _ topicIds_ -> topicIds_)
    initAcc
    accumulate
    (\_ levelResult -> levelResult)
    model


{- A more complex traversal, takes additionally Transform and LevelDone functions.
-}
traverseWith : BoxPath -> Transform -> acc -> Acc acc -> LevelDone acc -> Model -> acc
traverseWith boxPath transform initAcc accumulate levelDone model =
  traverseWith_ 0 boxPath transform initAcc accumulate levelDone model


traverseWith_ : Level -> BoxPath -> Transform -> acc -> Acc acc -> LevelDone acc -> Model -> acc
traverseWith_ level boxPath transform initAcc accumulate levelDone model =
  let
    boxId = firstId boxPath
    topicAccumulator : Topic -> acc -> acc
    topicAccumulator topic acc =
      let
        childPath = BoxId topic.id :: boxPath
        children =
          if Topic.isBox topic.id model then
            -- recursion
            Just <|
              traverseWith_ (level + 1) childPath transform initAcc accumulate levelDone model
          else
            Nothing
      in
      accumulate topic level boxPath acc children model
  in
  topicIds boxId model
    |> transform boxId model
    |> List.filterMap (topicLookup model)
    |> List.foldl topicAccumulator initAcc
    |> levelDone boxPath


topicLookup : Model -> TopicId -> Maybe Topic
topicLookup model topicId =
  case Topic.fromId topicId model of
    Just topic -> Just topic
    Nothing -> U.fail "Box.topicLookup" topicId Nothing


-- Not used
topics : BoxId -> Model -> List Topic
topics boxId model =
  topicIds boxId model |> List.foldr
    (\id acc ->
      case Topic.fromId id model of
        Just topic -> topic :: acc
        Nothing -> acc
    )
    []


{-| The TopicIds contained in the box's underlying ItemSet.
-}
topicIds : BoxId -> Model -> List TopicId
topicIds =
  itemIds maybeTopicId


{-| The AssocIds contained in the box's underlying ItemSet.
-}
assocIds : BoxId -> Model -> List AssocId
assocIds =
  itemIds maybeAssocId


itemIds : (ItemId -> Maybe id) -> BoxId -> Model -> List id
itemIds filter boxId model =
  case itemSetOf boxId model of
    Just itemSet ->
      itemSet.items
        |> List.filterMap (.id >> filter)
    Nothing -> U.fail "Box.itemIds" boxId []


-- Create box

-- Note: these 3 are actually low-level functions as the default view is not initialized.
-- Possibly move these 3 to Tool.createBoxOnDemand (and export none of them).
turnTopicIntoBox : TopicId -> Renderer -> Model -> Model
turnTopicIntoBox topicId renderer model =
  let
    setId = model.nextId
    set = ItemSet setId []
    box = Box (BoxId topicId) setId Dict.empty renderer
  in
  model
    |> create box
    |> createItemSet set
    |> Model.nextId


create : Box -> Model -> Model
create box ({boxes} as model) =
  { model | boxes = boxes |> Dict.insert (toBoxId box.id) box }


createItemSet : ItemSet -> Model -> Model
createItemSet set ({itemSets} as model) =
  { model | itemSets = itemSets |> Dict.insert set.id set }


-- Add item to box

{-| Adds an item to a box and creates a connecting association. This is an idempotent operation.
This is a generic operation: works for both, topics and associations.
-- Note: this is actually a low-level function as the view is not initialized.
-- TODO: move it to Tool module (like createBoxOnDemand)?
-}
addTopic : BoxTopic -> BoxId -> Model -> Model
addTopic topic boxId model =
  model
    |> addToItemSet (T topic.id) boxId
    |> addToBoxTopics topic boxId


addAssoc : AssocId -> BoxId -> Model -> Model
addAssoc assocId boxId model =
  model
    |> addToItemSet (A assocId) boxId


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
    T topicId ->
        model
          |> Assoc.create Hierarchy (fromBoxId boxId) topicId
          |> Tuple.first
    A _ -> model


addToBoxTopics : BoxTopic -> BoxId -> Model -> Model
addToBoxTopics topic boxId ({boxes} as model) =
  if hasBoxTopic topic.id boxId model then
    model
  else
    { model | boxes = boxes |> Dict.update (toBoxId boxId)
      (\maybeBox ->
        case maybeBox of
          Just box -> Just
            { box | topics = box.topics
              |> Dict.insert (toTopicId topic.id) topic
            }
          Nothing -> Nothing
      )
    }


-- Remove item from box

removeTopic : TopicId -> BoxId -> Model -> Model
removeTopic topicId boxId model =
  case (Topic.fromId topicId model, hierarchyAssoc topicId boxId model) of
    (Just topic, Just assocId) ->
      model
        |> removeItem (T topicId) boxId
        |> removeAssocs topic boxId
        |> deleteAssoc assocId
    _ -> U.fail "Box.removeTopic" {topicId = topicId, boxId = boxId} model


removeAssocs : Topic -> BoxId -> Model -> Model
removeAssocs topic boxId model =
  topic.assocIds |> List.foldr
    (\assocId modelAcc -> removeAssoc assocId boxId modelAcc)
    model


removeAssoc : AssocId -> BoxId -> Model -> Model
removeAssoc assocId boxId model =
  model
    |> removeItem (A assocId) boxId


{-| Removes an item from a box's underlying ItemSet.
It's a no-op if the item is not in the box.
Note: the BoxItem itself is *not* removed.
-}
removeItem : ItemId -> BoxId -> Model -> Model
removeItem itemId boxId model =
  case byId boxId model of
    Just box ->
      { model | itemSets = model.itemSets
        |> Dict.update box.itemSetId
          (\maybeItemSet ->
            case maybeItemSet of
              Just itemSet -> Just
                { itemSet | items = itemSet.items
                  |> List.filter (\setItem -> setItem.id /= itemId)
                }
              Nothing -> Nothing
          )
      }
    Nothing -> model


hierarchyAssoc : TopicId -> BoxId -> Model -> Maybe AssocId
hierarchyAssoc topicId boxId model =
  case Topic.fromId topicId model of
    Just topic ->
      findHierarchy topicId boxId topic.assocIds model
    Nothing -> U.fail "Box.hierarchyAssoc" {topicId = topicId, boxId = boxId} Nothing


findHierarchy : TopicId -> BoxId -> AssocIds -> Model -> Maybe AssocId
findHierarchy topicId boxId assocIds_ model =
  case assocIds_ of
    [] ->
      U.logError "Box.findHierarchy" ("Missing Hierarchy association between Topic "
        ++ fromInt (toTopicId topicId) ++ " and Box " ++ fromInt (toBoxId boxId)) Nothing
    assocId :: ids ->
      case Assoc.fromId assocId model of
        Just {id, assocType, topicId1, topicId2} ->
          if assocType == Hierarchy && topicId1 == fromBoxId boxId && topicId2 == topicId then
            Just id
          else
            findHierarchy topicId boxId ids model -- recursion
        Nothing -> Nothing


-- Delete item

{-| Deletes an item, along its associations, and removes them from all boxes.
Logs an error if no such item exists.
It's a generic operation: works for both, topics and associations. ### FIXDOC
-}
deleteTopic : TopicId -> Model -> Model
deleteTopic topicId model =
  case Topic.fromId topicId model of
    Just topic ->
      ( topic.assocIds |> List.foldr
          deleteAssoc
          model
      )
      |> deleteTopic_ topicId
    Nothing -> U.fail "Box.deleteTopic" {topicId = topicId} model


deleteAssoc : AssocId -> Model -> Model
deleteAssoc assocId model =
  model
    |> removeAssocFromTopics assocId
    |> deleteAssoc_ assocId


{-| Removes the association ID from both connected topics.
No-op if the given ID refers not to an association (but a topic).
Logs an error no item for the given ID exists. ### FIXDOC
-}
removeAssocFromTopics : AssocId -> Model -> Model
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
removeAssocFromTopic : AssocId -> TopicId -> Model -> Model
removeAssocFromTopic assocId topicId model =
  model |> Topic.update topicId
    (\topic ->
      {topic | assocIds = topic.assocIds |> List.filter ((/=) assocId)}
    )


{-| Deletes an item, and removes it from all boxes.
No-op if there is no such item.
Low-level function that does NOT delete the item's associations. ### FIXDOC
-}
deleteTopic_ : TopicId -> Model -> Model
deleteTopic_ topicId ({itemSets, topicMap} as model) =
  { model
  | topics = model.topics |> Dict.remove (toTopicId topicId) -- delete topic
  , itemSets = itemSets |> Dict.map -- delete topic from all itemSets
      (\_ ({items} as itemSet) ->
        { itemSet | items = items |> List.filter
          (\setItem -> setItem.id /= T topicId)
        }
      )
  -- TODO: if item is box delete from "boxes" state as well
  -- TODO: don't operate on "topicMap" directly, let ExtManager dispatch instead
  , topicMap =
      { topicMap | view = topicMap.view |> Dict.map -- delete item from all boxes
        (\_ topicMap_ ->
          { topicMap_ | topics = topicMap_.topics |> Dict.remove (toTopicId topicId) }
        )
      }
  }


{-| Deletes an item, and removes it from all boxes.
No-op if there is no such item.
Low-level function that does NOT delete the item's associations. ### FIXDOC
-}
deleteAssoc_ : AssocId -> Model -> Model
deleteAssoc_ assocId ({assocs, itemSets} as model) =
  { model
  | assocs = assocs |> Dict.remove (toAssocId assocId) -- delete assoc
  , itemSets = itemSets |> Dict.map -- delete assoc from all itemSets
      (\_ ({items} as itemSet) ->
        { itemSet | items = items |> List.filter
          (\setItem -> setItem.id /= A assocId)
        }
      )
  }


-- Expansion

expansionOf : TopicId -> BoxId -> Model -> Expansion
expansionOf topicId boxId model =
  case byId boxId model |> Maybe.andThen (\box -> topicFrom topicId box model) of
    Just {expansion} -> expansion
    Nothing -> U.fail "Box.expansionOf" {topicId = topicId, boxId = boxId} Collapsed


updateExpansion : TopicId -> BoxId -> (Expansion -> Expansion) -> Model -> Model
updateExpansion topicId boxId transform model =
  { model | boxes = model.boxes |> Dict.update (toBoxId boxId)
    (\maybeBox ->
      case maybeBox of
        Just box -> Just
          { box | topics = box.topics |> Dict.update (toTopicId topicId)
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
  { model | boxes = boxes |> Dict.update (toBoxId boxId)
    (\maybeBox ->
      case maybeBox of
        Just box -> Just { box | renderer = renderer }
        Nothing -> Nothing
    )
  }


--

{-| Checks if an item (Topic or Assoc) is contained in a box's underlying ItemSet.
Logs an error if the box or its ItemSet is missing.
-}
hasItem : ItemId -> BoxId -> Model -> Bool
hasItem itemId boxId model =
  case itemSetOf boxId model of
    Just itemSet ->
      itemSet.items
        |> List.any (\setItem -> setItem.id == itemId)
    Nothing -> False


{-| Checks if a topic (1st parameter, the *needle*) is contained in a box (2nd parameter, the
*haystack*), either directly or somewhere deep in its sub-boxes. Used for cycle detection.
Note that the check is positive if the passed topic is the passed box itself.
-}
hadDeepTopic : TopicId -> TopicId -> Model -> Bool
hadDeepTopic topicId boxId model =
  if topicId == boxId then
    True
  else
    if Topic.isBox boxId model then
      case itemSetOf (BoxId boxId) model of
        Just itemSet -> itemSet.items |> List.any
          (\setItem ->
            case setItem.id of
              T id -> hadDeepTopic topicId id model
              A _ -> False
          ) -- recursion
        Nothing -> False
    else
      False


{-| The box's underlying ItemSet.
Logs an error (and returns Nothing) if the box is missing, or its underlying ItemSet is missing.
-}
itemSetOf : BoxId -> Model -> Maybe ItemSet
itemSetOf boxId model =
  case byId boxId model of
    Just box ->
      case Dict.get box.itemSetId model.itemSets of
        Just itemSet -> Just itemSet
        Nothing -> U.logError "Box.itemSetOf" ("Missing underlying ItemSet ("
          ++ fromInt box.itemSetId ++ ") of Box " ++ fromInt (toBoxId boxId)) Nothing
    Nothing -> U.fail "Box.itemSetOf" {boxId = boxId} Nothing


hasBoxTopic : TopicId -> BoxId -> Model -> Bool
hasBoxTopic topicId boxId model =
  case byId boxId model of
    Just box -> box.topics |> Dict.member (toTopicId topicId)
    Nothing -> U.fail "Box.hasBoxTopic" {topicId = topicId, boxId = boxId} False


{-| Logs an error if the box is missing. -}
byId : BoxId -> Model -> Maybe Box
byId boxId model =
  case model.boxes |> Dict.get (toBoxId boxId) of
    Just box -> Just box
    Nothing -> U.boxNotFound "Box.byId" boxId Nothing


topicFrom : TopicId -> Box -> Model -> Maybe BoxTopic
topicFrom (TopicId id) box model =
  case box.topics |> Dict.get id of
    Just topic -> Just topic
    Nothing -> U.logError "Box.topicFrom"
      ("Missing BoxTopic " ++ fromInt id ++ " in Box " ++ fromInt (toBoxId box.id))
      Nothing


{-| Title of fullscreen box.
TODO: rename
-}
mapTitle : Model -> String
mapTitle model =
  case Topic.fromId (fromBoxId model.boxId) model of
    Just topic -> Topic.label topic
    Nothing -> U.fail "mapTitle" model.boxId "??"


isFullscreen : BoxId -> Model -> Bool
isFullscreen boxId model =
  boxId == model.boxId


elemId : String -> TopicId -> BoxPath -> String
elemId name (TopicId id) boxPath =
  name ++ "-" ++ fromInt id ++ "," ++ fromPath boxPath


{-| Logs an error (and returns -1) if boxPath is empty.
-}
firstId : BoxPath -> BoxId
firstId boxPath =
  case boxPath of
    boxId :: _ -> boxId
    [] -> U.logError "firstId" "boxPath is empty!" (BoxId (TopicId -1)) -- ### FIXME: -1


fromPath : BoxPath -> String
fromPath boxPath =
  boxPath
    |> List.map (fromInt << toBoxId)
    |> String.join ","
