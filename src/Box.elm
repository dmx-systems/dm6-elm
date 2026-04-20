module Box exposing (topics, assocs, turnTopicIntoBox, addItem, removeItem, deleteItem,
  expansionOf, updateExpansion, rendererOf, setRenderer, hasItem, hasDeepItem, mapTitle,
  isFullscreen, elemId, firstId, fromPath)

import Item
import Model exposing (Model)
import ModelBase exposing (..)
import Utils as U

import Dict
import String exposing (fromInt)



-- TODO: unify these 2
topics : BoxId -> Model -> List TopicInfo
topics boxId model =
  case itemSetOf boxId model of
    Just itemSet ->
      itemSet.items |> List.foldr
        (\setItem topicsAcc ->
          case Item.topicOrNothing setItem.id model of
            Just topic -> topic :: topicsAcc
            Nothing -> topicsAcc
        )
        []
    Nothing -> []


assocs : BoxId -> Model -> List AssocInfo
assocs boxId model =
  case itemSetOf boxId model of
    Just itemSet ->
      itemSet.items |> List.foldr
        (\setItem assocsAcc ->
          case Item.assocOrNothing setItem.id model of
            Just assoc -> assoc :: assocsAcc
            Nothing -> assocsAcc
        )
        []
    Nothing -> []


-- Create box

turnTopicIntoBox : Id -> Model -> Model
turnTopicIntoBox topicId model =
  let
    setId = model.nextId
    set = ItemSet setId []
    box = Box topicId setId Dict.empty "TopicMap"
  in
  model
    |> create box
    |> createItemSet set
    |> Item.nextId


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
addItem : BoxItem -> BoxId -> Model -> Model
addItem item boxId model =
  model
    |> addToItemSet item.id boxId
    |> addToBoxItems item boxId


addToItemSet : Id -> BoxId -> Model -> Model
addToItemSet itemId boxId model =
  if hasItem itemId boxId model then
    model
  else
    case byId boxId model of
      Just box ->
        model
          |> Item.createAssoc Hierarchy boxId itemId
          |> Tuple.first
          |> addToItemSet_ (SetItem itemId) box.itemSetId
      Nothing -> model


addToItemSet_ : SetItem -> Id -> Model -> Model
addToItemSet_ setItem itemSetId ({itemSets} as model) =
  { model | itemSets =
    itemSets
      |> Dict.update itemSetId
        (\maybeItemSet ->
          case maybeItemSet of
            Just itemSet -> Just { itemSet | items = setItem :: itemSet.items }
            Nothing -> U.itemSetNotFound "Box.addToItemSet_" itemSetId Nothing
        )
  }


addToBoxItems : BoxItem -> BoxId -> Model -> Model
addToBoxItems item boxId ({boxes} as model) =
  if hasBoxItem item.id boxId model then
    model
  else
    { model | boxes =
      boxes
        |> Dict.update boxId
          (\maybeBox ->
            case maybeBox of
              Just box -> Just { box | items = box.items |> Dict.insert item.id item }
              Nothing -> Nothing
          )
    }


-- Remove item from box

removeItem : Id -> BoxId -> Model -> Model
removeItem itemId boxId model =
  let
    itemIds =
      itemId :: Item.assocIds itemId model
  in
  case hierarchyAssoc itemId boxId model of
    Just assocId ->
      model
        |> removeItems itemIds boxId
        |> deleteItem assocId
    Nothing -> model


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
                  |> List.filter (\setItem -> not <| List.member setItem.id itemIds)
                }
              Nothing -> Nothing
          )
      }
    Nothing -> model


hierarchyAssoc : Id -> BoxId -> Model -> Maybe Id
hierarchyAssoc itemId boxId model =
  findHierarchy itemId boxId (Item.assocIds itemId model) model


findHierarchy : Id -> BoxId -> AssocIds -> Model -> Maybe Id
findHierarchy itemId boxId assocIds model =
  case assocIds of
    [] ->
      U.logError "Box.hierarchyAssoc"
        ("Hierarchy of " ++ fromInt itemId ++ " in " ++ fromInt boxId ++ " not found")
        Nothing
    assocId :: ids ->
      case Item.assocById assocId model of
        Just {id, assocType, player1, player2} ->
          if assocType == Hierarchy && player1 == boxId && player2 == itemId then
            Just id
          else
            findHierarchy itemId boxId ids model -- recursion
        Nothing -> Nothing


-- Delete item

{-| Deletes an item, along its associations, and removes them from all boxes.
Logs an error if no such item exists.
It's a generic operation: works for both, topics and associations.
Note: while this functions supports associations as players in associations,
at the moment DM6 Elm makes no use of it.
-}
deleteItem : Id -> Model -> Model
deleteItem itemId model =
  Item.assocIds itemId model
    |> List.foldr deleteItem model -- recursion
    |> removeAssocFromPlayers itemId
    |> deleteItem_ itemId


{-| Removes the association ID from both player's set of association IDs.
No-op if the given ID refers not to an association (but a topic).
Logs an error no item for the given ID exists.
-}
removeAssocFromPlayers : Id -> Model -> Model
removeAssocFromPlayers assocId model =
  case Item.byId assocId model of
    Just {info} ->
      case info of
        Assoc assoc -> -- Note: assocId and assoc.id are the same
          model
            |> removeAssocFromPlayer assoc.id assoc.player1
            |> removeAssocFromPlayer assoc.id assoc.player2
        Topic _ -> model
    Nothing -> model -- error is already logged


{-| Removes an association ID from the item's set of association IDs.
No-op if the given association ID is not in the set.
Logs an error if item does not exist.
-}
removeAssocFromPlayer : Id -> Id -> Model -> Model
removeAssocFromPlayer assocId itemId model =
  model |> Item.update itemId
    (\item ->
      {item | assocIds = item.assocIds |> List.filter ((/=) assocId)}
    )


{-| Deletes an item, and removes it from all boxes.
No-op if there is no such item.
Low-level function that does NOT delete the item's associations.
-}
deleteItem_ : Id -> Model -> Model
deleteItem_ itemId ({topicMap} as model) =
  { model
  | items = model.items |> Dict.remove itemId -- delete item
  , itemSets = model.itemSets |> Dict.map -- delete item from all itemSets
      (\_ itemSet ->
        { itemSet | items = itemSet.items |> List.filter
          (\setItem -> setItem.id /= itemId)
        }
      )
  -- TODO: if item is box delete from "boxes" state as well
  -- TODO: don't operate on "topicMap" directly
  , topicMap = topicMap |> Dict.map -- delete item from all boxes
      (\_ map ->
        { map | items = map.items |> Dict.remove itemId }
      )
  }


-- Expansion

expansionOf : Id -> BoxId -> Model -> Expansion
expansionOf topicId boxId model =
  case byId boxId model |> Maybe.andThen (\box -> itemById topicId box model) of
    Just {expansion} -> expansion
    Nothing -> U.fail "Box.expansionOf" {topicId = topicId, boxId = boxId} Collapsed


-- Not used
{-| Logs an error if box does not exist, or if topic is not in box -}
setExpansion : Id -> BoxId -> Expansion -> Model -> Model
setExpansion topicId boxId expansion model =
  model
    |> updateExpansion topicId boxId (\_ -> expansion)


updateExpansion : Id -> BoxId -> (Expansion -> Expansion) -> Model -> Model
updateExpansion topicId boxId transform model =
  { model | boxes = model.boxes |> Dict.update boxId
    (\maybeBox ->
      case maybeBox of
        Just box -> Just
          { box | items = box.items |> Dict.update topicId
            (\maybeItem ->
              case maybeItem of
                Just item -> Just
                  { item | expansion = transform item.expansion }
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
    Nothing -> U.fail "Box.renderer" {boxId = boxId} Nothing


setRenderer : BoxId -> Renderer -> Model -> Model
setRenderer boxId renderer model =
  { model | boxes = model.boxes |> Dict.update boxId
    (\maybeBox ->
      case maybeBox of
        Just box -> Just { box | renderer = renderer }
        Nothing -> Nothing
    )
  }


--

{-| Logs an error if box does not exist. -}
byId : BoxId -> Model -> Maybe Box
byId boxId model =
  case model.boxes |> Dict.get boxId of
    Just box -> Just box
    Nothing -> U.boxNotFound "Box.byId" boxId Nothing


itemById : Id -> Box -> Model -> Maybe BoxItem
itemById itemId box model =
  case box.items |> Dict.get itemId of
    Just item -> Just item
    Nothing -> U.logError "Box.itemById" "missing dict entry in box.items" Nothing


itemSetOf : BoxId -> Model -> Maybe ItemSet
itemSetOf boxId model =
  case byId boxId model of
    Just box -> model.itemSets |> Dict.get box.itemSetId
    Nothing -> U.fail "Box.itemSetOf" {boxId = boxId} Nothing


{-| Returns True if an item is *visible* in a box. An item is regarded visible if it is
contained in the ItemSet assigned to that box. ### TODO: rename to "isItemVisible"?
Logs an error if box does not exist.
-}
hasItem : Id -> BoxId -> Model -> Bool
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
    if Item.isBox boxId model then
      case itemSetOf boxId model of
        Just itemSet -> itemSet.items |> List.any
          (\setItem -> hasDeepItem setItem.id itemId model) -- recursion
        Nothing -> False
    else
      False


hasBoxItem : Id -> BoxId -> Model -> Bool
hasBoxItem itemId boxId model =
  case byId boxId model of
    Just box -> box.items |> Dict.member itemId
    Nothing -> U.fail "Box.hasBoxItem" {itemId = itemId, boxId = boxId} False


{-| Title of fullscreen box.
TODO: rename
-}
mapTitle : Model -> String
mapTitle model =
  case Item.topicById model.boxId model of
    Just topic -> Item.topicLabel topic
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
  boxPath |> List.map fromInt |> String.join ","
