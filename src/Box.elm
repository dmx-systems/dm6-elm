module Box exposing (topicIds, assocIds, turnTopicIntoBox, init, addTopic, addAssoc,
  removeTopic, removeAssoc, deleteTopic, deleteAssoc, expansionOf, updateExpansion, rendererOf,
  setRenderer, hasItem, hadDeepTopic, mapTitle, isFullscreen, setFullscreen, elemId, firstId,
  fromPath, topicCount, traverse)

import Assoc
import Console
import Env exposing (Env)
import Extension exposing (Renderer)
import Model exposing (Model)
import ModelBase exposing (..)
import Topic

import Dict
import String exposing (fromInt)



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
    Nothing -> Console.fail "Box.itemIds" boxId []


-- Create box

turnTopicIntoBox : TopicId -> Renderer -> Env -> Env
turnTopicIntoBox topicId renderer ({model} as env) =
  if Topic.isBox topicId model then
    env
  else
    env
      |> turnTopicIntoBox_ topicId renderer
      |> init (BoxId topicId)


turnTopicIntoBox_ : TopicId -> Renderer -> Env -> Env
turnTopicIntoBox_ topicId renderer ({model} as env) =
  let
    setId = model.nextId
    set = ItemSet setId []
    box = Box (BoxId topicId) setId Dict.empty renderer
  in
  env
    |> Env.map (create box)
    |> Env.map (createItemSet set)
    |> Env.map Model.nextId


create : Box -> Model -> Model
create box ({boxes} as model) =
  { model | boxes = boxes |> Dict.insert (toBoxId box.id) box }


createItemSet : ItemSet -> Model -> Model
createItemSet set ({itemSets} as model) =
  { model | itemSets = itemSets |> Dict.insert set.id set }


-- Add item to box

{-| Adds an item to a box and creates a connecting association. This is an idempotent operation.
This is a generic operation: works for both, topics and associations.
-}
addTopic : BoxTopic -> BoxId -> Env -> Env
addTopic topic boxId env =
  env
    |> Env.map (addToItemSet (T topic.id) boxId)
    |> Env.map (addToBoxTopics topic boxId)
    |> init boxId


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
        Nothing -> Console.itemSetNotFound "Box.addToItemSet_" itemSetId Nothing
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


-- Initialize/Update box view model

init : BoxId -> Env -> Env
init boxId ({model, dispatch} as env) =
  let
    initBox : BoxId -> Model -> Model
    initBox =
      dispatch.init boxId model
    --
    init_ : BoxId -> Model -> Model
    init_ boxId_ model_ =
      model_
        -- 1) init child boxes, must perform first, TODO: why?
        |> topicIds boxId_
        |> List.foldl childBoxInit model_
        -- 2) init box itself
        |> initBox boxId_
    --
    childBoxInit : TopicId -> Model -> Model
    childBoxInit topicId modelAcc =
      if Topic.isBox topicId modelAcc then
        modelAcc |> init_ (BoxId topicId) -- recursion
      else
        modelAcc
  in
  env
    |> Env.map (init_ boxId)


-- Remove item from box

removeTopic : TopicId -> BoxId -> Model -> Model
removeTopic topicId boxId model =
  case (Topic.fromId topicId model, hierarchyAssoc topicId boxId model) of
    (Just topic, Just assocId) ->
      model
        |> removeItem (T topicId) boxId
        |> removeAssocs topic boxId
        |> deleteAssoc assocId
    _ ->
      let
        _ = Console.fail "Box.removeTopic" "Unexpected state" {topicId = topicId, boxId = boxId}
      in
      model


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
    Nothing -> Console.fail "Box.hierarchyAssoc" {topicId = topicId, boxId = boxId} Nothing


findHierarchy : TopicId -> BoxId -> AssocIds -> Model -> Maybe AssocId
findHierarchy topicId boxId assocIds_ model =
  case assocIds_ of
    [] ->
      Console.logError "Box.findHierarchy" ("Missing Hierarchy association between Topic "
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
    Nothing -> Console.fail "Box.deleteTopic" {topicId = topicId} model


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
    Nothing -> Console.fail "Box.removeAssocFromTopics" {assocId = assocId} model


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
  -- TODO: don't operate on "topicMap" directly, dispatch instead
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
    Nothing -> Console.fail "Box.expansionOf" {topicId = topicId, boxId = boxId} Collapsed


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
    Nothing -> Console.fail "Box.rendererOf" {boxId = boxId} Nothing


setRenderer : BoxId -> Renderer -> Model -> Model
setRenderer boxId renderer model =
  -- set for box itself
  let
    model_ = setRenderer_ boxId renderer model
  in
  -- set for all child boxes (deep)
  fold boxId model_
    (\topicId modelAcc ->
      if Topic.isBox topicId modelAcc then
        setRenderer_ (BoxId topicId) renderer modelAcc
      else
        modelAcc
    )
    model_


setRenderer_ : BoxId -> Renderer -> Model -> Model
setRenderer_ boxId renderer ({boxes} as model) =
  { model | boxes = boxes |> Dict.update (toBoxId boxId)
    (\maybeBox ->
      case maybeBox of
        Just box -> Just { box | renderer = renderer }
        Nothing ->
          let
            _ = Console.logError "Box.setRenderer_" "Box not found" boxId
          in
          Nothing
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
        Nothing -> Console.logError "Box.itemSetOf" ("Missing underlying ItemSet ("
          ++ fromInt box.itemSetId ++ ") of Box " ++ fromInt (toBoxId boxId)) Nothing
    Nothing -> Console.fail "Box.itemSetOf" {boxId = boxId} Nothing


hasBoxTopic : TopicId -> BoxId -> Model -> Bool
hasBoxTopic topicId boxId model =
  case byId boxId model of
    Just box -> box.topics |> Dict.member (toTopicId topicId)
    Nothing -> Console.fail "Box.hasBoxTopic" {topicId = topicId, boxId = boxId} False


{-| Logs an error if the box is missing. -}
byId : BoxId -> Model -> Maybe Box
byId boxId model =
  case model.boxes |> Dict.get (toBoxId boxId) of
    Just box -> Just box
    Nothing -> Console.boxNotFound "Box.byId" boxId Nothing


topicFrom : TopicId -> Box -> Model -> Maybe BoxTopic
topicFrom (TopicId id) box model =
  case box.topics |> Dict.get id of
    Just topic -> Just topic
    Nothing -> Console.logError "Box.topicFrom"
      ("Missing BoxTopic " ++ fromInt id ++ " in Box " ++ fromInt (toBoxId box.id))
      Nothing


{-| Title of fullscreen box.
TODO: rename
-}
mapTitle : Model -> String
mapTitle model =
  case Topic.fromId (fromBoxId model.boxId) model of
    Just topic -> Topic.label topic
    Nothing -> Console.fail "mapTitle" model.boxId "??"


isFullscreen : BoxId -> Model -> Bool
isFullscreen boxId model =
  boxId == model.boxId


setFullscreen : BoxId -> Model -> Model
setFullscreen boxId model =
  { model | boxId = boxId }


elemId : String -> TopicId -> BoxPath -> String
elemId name (TopicId id) boxPath =
  name ++ "-" ++ fromInt id ++ "," ++ fromPath boxPath


{-| Logs an error (and returns -1) if boxPath is empty.
-}
firstId : BoxPath -> BoxId
firstId boxPath =
  case boxPath of
    boxId :: _ -> boxId
    [] -> Console.logError "firstId" "boxPath is empty!" (BoxId (TopicId -1)) -- ### FIXME: -1


fromPath : BoxPath -> String
fromPath boxPath =
  boxPath
    |> List.map (fromInt << toBoxId)
    |> String.join ","


-- Fold

type alias TopicFold acc =
  TopicId -> acc -> acc


{- Folds box content into a single value.
-}
fold : BoxId -> acc -> TopicFold acc -> Model -> acc
fold boxId initAcc foldTopic model =
  let
    fold_ : BoxId -> acc -> acc
    fold_ boxId_ initAcc_ =
      topicIds boxId_ model
        |> List.foldl
          (\topicId accVal ->
            let
              newAcc = foldTopic topicId accVal
            in
            if Topic.isBox topicId model then
              fold_ (BoxId topicId) newAcc -- recursion
            else
              newAcc
          )
          initAcc_
  in
  fold_ boxId initAcc


topicCount : BoxId -> Model -> Int
topicCount boxId model =
  fold boxId 0
    (\_ count -> count + 1)
    model


-- Traversal

-- TODO: are both acc parameter really needed?
type alias Acc acc =
  Topic -> Level -> BoxPath -> acc -> Maybe acc -> Model -> acc


-- Box content transformation before traversal
type alias Transform =
  BoxId -> Model -> List TopicId -> List TopicId


type alias LevelDone acc =
  BoxPath -> acc -> acc


{- A box content traversal that accumulates a single value.
TODO: rename to "fold"?
-}
traverse : BoxPath -> Transform -> acc -> Acc acc -> LevelDone acc -> Model -> acc
traverse boxPath transform initAcc accumulate levelDone model =
  let
    traverse_ : Level -> BoxPath -> acc
    traverse_ level boxPath_ =
      let
        boxId = firstId boxPath_
        topicAccumulator : Topic -> acc -> acc
        topicAccumulator topic acc =
          let
            children =
              if Topic.isBox topic.id model then
                Just <| traverse_ (level + 1) (BoxId topic.id :: boxPath_) -- recursion
              else
                Nothing
          in
          accumulate topic level boxPath_ acc children model
      in
      topicIds boxId model
        |> transform boxId model
        |> List.filterMap (topicLookup model)
        |> List.foldl topicAccumulator initAcc
        |> levelDone boxPath_
  in
  traverse_ 0 boxPath


topicLookup : Model -> TopicId -> Maybe Topic
topicLookup model topicId =
  case Topic.fromId topicId model of
    Just topic -> Just topic
    Nothing -> Console.fail "Box.topicLookup" topicId Nothing
