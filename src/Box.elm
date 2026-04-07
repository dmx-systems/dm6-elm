module Box exposing (hasItem, hasDeepItem, topics, create, addItem, displayMode, setDisplayMode,
  updateDisplayMode, rendererOf, setRenderer, deleteItem, mapTitle, isFullscreen, elemId,
  firstId, fromPath)

import BoxRendererDef exposing (Renderer)
import Item
import Model exposing (Model)
import ModelBase exposing (..)
import Utils as U

import Dict
import Set
import String exposing (fromInt)



{-| Logs an error if box does not exist. -}
byId : BoxId -> Model -> Maybe Box
byId boxId model =
  case model.boxes |> Dict.get boxId of
    Just box -> Just box
    Nothing -> U.boxNotFound "Box.byId" boxId Nothing


itemPropsOf : Id -> Box -> Model -> Maybe ItemProps
itemPropsOf itemId box model =
  case box.itemProps |> Dict.get itemId of
    Just props -> Just props
    Nothing -> U.logError "Box.itemPropsOf" "missing dict entry in box.itemProps" Nothing


itemSetOf : BoxId -> Model -> Maybe ItemSet
itemSetOf boxId model =
  case byId boxId model of
    Just box -> model.itemSets |> Dict.get box.itemSetId
    Nothing -> U.fail "Box.itemSetOf" {boxId = boxId} Nothing


{-| Logs an error if box does not exist.
-}
hasItem : BoxId -> Id -> Model -> Bool
hasItem boxId itemId model =
  case itemSetOf boxId model of
    Just itemSet -> itemSet.items |> List.any
      (\setItem -> setItem.id == itemId)
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


topics : BoxId -> Model -> Maybe (List TopicInfo)
topics boxId model =
  case itemSetOf boxId model of
    Just itemSet -> Just
      (itemSet.items |> List.foldr
        (\setItem topicsAcc ->
          case Item.topicOrNothing setItem.id model of
            Just topic -> topic :: topicsAcc
            Nothing -> topicsAcc
        )
        []
      )
    Nothing -> Nothing


-- Create box

create : String -> Maybe Icon -> Model -> (Model, BoxId)
create text icon model =
  let
    (newModel, topicId) = Item.createTopic text icon model
    setId = newModel.nextId
    box = Box topicId setId Dict.empty BoxRendererDef.TopicMap
    set = ItemSet setId []
  in
  ( newModel
      |> create_ box
      |> createItemSet set
      |> Item.nextId
  , topicId
  )


create_ : Box -> Model -> Model
create_ box ({boxes} as model) =
  { model | boxes = boxes |> Dict.insert box.id box }


createItemSet : ItemSet -> Model -> Model
createItemSet set ({itemSets} as model) =
  { model | itemSets = itemSets |> Dict.insert set.id set }


-- Add item to box

addItem : ItemProps -> BoxId -> Model -> Model
addItem props boxId model =
  case byId boxId model of
    Just box ->
      let
        (newModel, boxAssocId) = Item.createAssoc Hierarchy boxId props.id model
        setItem = SetItem props.id boxAssocId
      in
      newModel
        |> addToItemSet setItem box.itemSetId
        |> addToItemProps props boxId
    Nothing -> model


addToItemSet : SetItem -> Id -> Model -> Model
addToItemSet setItem itemSetId ({itemSets} as model) =
  { model | itemSets =
    itemSets
      |> Dict.update itemSetId
        (\maybeItemSet ->
          case maybeItemSet of
            Just itemSet -> Just { itemSet | items = setItem :: itemSet.items }
            Nothing -> U.itemSetNotFound "Box.addToItemSet" itemSetId Nothing
        )
  }


addToItemProps : ItemProps -> BoxId -> Model -> Model
addToItemProps props boxId ({boxes} as model) =
  { model | boxes =
    boxes
      |> Dict.update boxId
        (\maybeBox ->
          case maybeBox of
            Just box -> Just { box | itemProps = box.itemProps |> Dict.insert props.id props }
            Nothing -> Nothing
        )
  }


-- Display Mode

displayMode : Id -> BoxId -> Model -> Maybe DisplayMode
displayMode topicId boxId model =
  case byId boxId model |> Maybe.andThen (\box -> itemPropsOf topicId box model) of
    Just props -> Just props.displayMode
    Nothing -> U.fail "Box.displayMode" {topicId = topicId, boxId = boxId} Nothing


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
      {item | assocIds = item.assocIds |> Set.remove assocId}
    )


{-| Deletes an item, and removes it from all boxes.
No-op if there is no such item.
Low-level function that does NOT delete the item's associations.
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
