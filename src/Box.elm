module Box exposing (addBox, setDisplayMode, updateDisplayMode, deleteItem, mapTitle,
  isFullscreen, displayMode, elemId, firstId, fromPath)

import Item
import Model exposing (Model)
import ModelParts exposing (..)
import Utils as U

import Dict
import Set
import String exposing (fromInt)



{-| Logs an error if box does not exist. -}
byId : BoxId -> Model -> Maybe Box
byId boxId model =
  case model.boxes |> Dict.get boxId of
    Just box -> Just box
    Nothing -> U.illegalBoxId "byId" boxId Nothing


itemProps : Id -> Box -> Model -> Maybe ItemProps
itemProps itemId box model =
  case box.itemProps |> Dict.get itemId of
    Just props -> Just props
    Nothing -> U.illegalItemId "itemProps" itemId Nothing


-- Add item to box

addItem : Id -> BoxId -> Model -> Model
addItem itemId boxId model =
  case byId boxId model of
    Just box ->
      let
        (newModel, boxAssocId) = Item.addAssoc Hierarchy boxId itemId model
      in
      { newModel | itemSets = newModel.itemSets |> Dict.update box.itemSetId
          (\maybeItemSet ->
            case maybeItemSet of
              Just itemSet -> Just
                { itemSet | items = SetItem itemId boxAssocId :: itemSet.items }
              Nothing -> U.illegalItemSetId "itemSet" box.itemSetId Nothing
          )
      }
    Nothing -> model


-- Display Mode

displayMode : Id -> BoxId -> Model -> Maybe DisplayMode
displayMode topicId boxId model =
  case byId boxId model |> Maybe.andThen (\box -> itemProps topicId box model) of
    Just props -> Just props.displayMode
    Nothing -> U.fail "displayMode" {topicId = topicId, boxId = boxId} Nothing


{-| Logs an error if box does not exist, or if topic is not in box -}
setDisplayMode : Id -> BoxId -> DisplayMode -> Model -> Model
setDisplayMode topicId boxId display model =
  model
    |> updateDisplayMode topicId boxId (\_ -> display)


updateDisplayMode : Id -> BoxId -> (DisplayMode -> DisplayMode) -> Model -> Model
updateDisplayMode topicId boxId transform model =
  { model | boxes = model.boxes |> Dict.update boxId
    (\maybeBox ->
      case maybeBox of
        Just box -> Just
          { box | itemProps = box.itemProps |> Dict.update topicId
            (\maybeItem ->
              case maybeItem of
                Just item -> Just
                  { item | displayMode = transform item.displayMode }
                Nothing -> Nothing
            )
          }
        Nothing -> Nothing
    )
  }


-- Not used
itemSetById : Id -> Model -> Maybe ItemSet
itemSetById setId model =
  case model.itemSets |> Dict.get setId of
    Just itemSet -> Just itemSet
    Nothing -> U.illegalItemSetId "itemSet" setId Nothing


-- Create box

addBox : String -> Maybe Icon -> Model -> (Model, BoxId)
addBox text icon model =
  let
    (newModel, topicId) = Item.addTopic text icon model
    setId = newModel.nextId
    box = Box topicId setId Dict.empty
    set = ItemSet setId []
  in
  ( newModel
      |> addBox_ box
      |> addItemSet set
      |> Item.nextId
  , topicId
  )


addBox_ : Box -> Model -> Model
addBox_ box ({boxes} as model) =
  { model | boxes = boxes |> Dict.insert box.id box }


addItemSet : ItemSet -> Model -> Model
addItemSet set ({itemSets} as model) =
  { model | itemSets = itemSets |> Dict.insert set.id set }


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
    |> Set.foldr deleteItem model -- recursion
    |> deleteAssocRefs_ itemId
    |> deleteItem_ itemId


{-| Removes the association ID from both player's set of association IDs.
No-op if the given ID refers not to an association (but a topic).
Logs an error no item for the given ID exists.
Low-level API for maintaining the sets of association IDs.
-}
deleteAssocRefs_ : Id -> Model -> Model
deleteAssocRefs_ assocId model =
  case Item.byId assocId model of
    Just {info} ->
      case info of
        Assoc assoc -> -- Note: assocId and assoc.id are the same
          model
            |> deleteAssocId_ assoc.id assoc.player1
            |> deleteAssocId_ assoc.id assoc.player2
        Topic _ -> model
    Nothing -> model -- error is already logged


{-| Removes an association ID from the item's set of association IDs.
No-op if the given association ID is not in the set.
Logs an error if item does not exist.
Low-level API for maintaining the association ID set.
-}
deleteAssocId_ : Id -> Id -> Model -> Model
deleteAssocId_ assocId itemId model =
  model |> Item.update itemId
    (\item ->
      {item | assocIds = item.assocIds |> Set.remove assocId}
    )


{-| Deletes an item, and removes it from all boxes.
No-op if there is no such item.
Low-level API that does NOT delete the item's associations.
-}
deleteItem_ : Id -> Model -> Model
deleteItem_ itemId ({topicMap} as model) =
  { model
  | items = model.items |> Dict.remove itemId -- delete item
  -- TODO: update "box" state
  -- TODO: don't operate on "topicMap" directly
  , topicMap = topicMap |> Dict.map -- delete item from all boxes
      (\_ map ->
        { map | items = map.items |> Dict.remove itemId }
      )
  }


--

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
