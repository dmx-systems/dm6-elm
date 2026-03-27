module Box exposing (addBox, deleteItem)

import Item
import Model exposing (Model)
import ModelParts exposing (Id, ItemInfo(..), Icon, Box, BoxId)
import Render.TopicMap.Box as TMBox

import Dict
import Set



addBox : String -> Maybe Icon -> Model -> (Model, BoxId)
addBox text icon model =
  let
    (newModel, topicId) = Item.addTopic text icon model
    -- TODO: init renderer-specific data: (Rectangle 0 0 0 0) (Point 0 0) Dict.empty
  in
  ( newModel
    |> addBox_ topicId
    |> TMBox.addBox topicId
  , topicId
  )


addBox_ : BoxId -> Model -> Model
addBox_ boxId model =
  { model | boxes = model.boxes |> Dict.insert boxId (Box boxId) }


-- Delete Item

deleteItem : Id -> Model -> Model
deleteItem itemId model =
  model
    |> deleteItem_ itemId


{-| Deletes an item, along its associations, and removes them from all boxes.
Logs an error if no such item exists.
It's a generic operation: works for both, topics and associations.
Note: while this functions supports associations as players in associations,
at the moment DM6 Elm makes no use of it.
-}
deleteItem_ : Id -> Model -> Model
deleteItem_ itemId model =
  Item.assocIds itemId model
    |> Set.foldr deleteItem_ model -- recursion
    |> deleteAssocRefs_ itemId
    |> deleteItem__ itemId


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
deleteItem__ : Id -> Model -> Model
deleteItem__ itemId ({topicMap} as model) =
  { model
  | items = model.items |> Dict.remove itemId -- delete item
  , topicMap =
    { topicMap | boxes = topicMap.boxes |> Dict.map -- delete item from all boxes
      (\_ box ->
        { box | items = box.items |> Dict.remove itemId }
      )
    }
  }
